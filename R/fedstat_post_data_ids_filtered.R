#' Post data filters ids to fedstat.ru and download specified subset of data
#'
#' @description Creates a request body from `data_ids` and sends it to
#'   \code{https://www.fedstat.ru/indicator/downloadData.do?format=\{format\}}.
#'   Gets an sdmx or excel with data in binary format.
#'
#'   The request includes a CSRF token that is automatically extracted by
#'   \code{fedstat_get_data_ids} and passed through via data.frame attributes.
#'   Token reuse may succeed, but Fedstat can reject a token at any time. This
#'   primitive sends one POST; the convenience wrapper and batch downloader own
#'   retry and token-refresh decisions.
#'
#'   If no CSRF token is found in `data_ids` attributes (e.g. when using cached
#'   data_ids from a previous session), the function will attempt to fetch a
#'   fresh token automatically, using the same HTTP configuration.
#'
#'   sdmx raw bytes can be passed to `fedstat_parse_sdmx_to_table` to create a
#'   data.frame or to `rawToChar` and `writeLines` to create an xml file
#'
#'   excel raw bytes can be passed to `writeBin` to create an xls file
#'
#' @param data_ids data.frame, a result of `fedstat_get_data_ids` to download all available data,
#'   or a result of `fedstat_data_ids_filter` to download subset of available data
#' @param ... other arguments passed to httr::POST
#' @param data_format string, one of sdmx, excel
#' @param timeout_seconds numeric, maximum time before POST request times out
#' @param retry_max_times numeric, not used for POST (exports are not retried here),
#'   but used for fallback GET if CSRF token needs to be fetched
#' @param httr_verbose `httr::verbose()` or NULL, outputs messages to the console
#' about the processing of the request
#'
#' @return raw bytes (sdmx or excel)
#' @export
#'
#' @seealso \code{\link{fedstat_parse_sdmx_to_table}}
#'
#' @examples
#' \dontrun{
#' # Get data filters identificators for CPI
#' # filter the data_ids to get data for january of 2023
#' # for all goods and services for Russian Federation
#' # POST filters and download data in sdmx format
#' data <- fedstat_get_data_ids("31074") %>%
#'     fedstat_data_ids_filter(
#'         filters = list(
#'             "Territory" = "Russian Federation",
#'             "Year" = "2023",
#'             "Period" = "January",
#'             "Types of goods and services" = "*"
#'         )
#'     ) %>%
#'     fedstat_post_data_ids_filtered()
#'
#' # Not actual filter field titles and filter values titles because of ASCII requirement for CRAN
#' }
fedstat_post_data_ids_filtered <- function(data_ids,
                                           ...,
                                           data_format = c("sdmx", "excel"),
                                           timeout_seconds = 180,
                                           retry_max_times = 3,
                                           httr_verbose = NULL) {
    data_format <- match.arg(data_format)
    session <- fedstat_session(data_ids)
    before <- session$before
    if (is.null(before)) before <- function() NULL
    if (is.null(session$token) || is.null(session$token_name)) {
        fedstat_refresh(session, httr_verbose, ...,
            timeout_seconds = timeout_seconds,
            attempts = retry_max_times, before = before
        )
    }
    session$preview_empty <- FALSE
    if (isTRUE(session$prepare_before_download)) {
        session$preview_empty <- fedstat_prepare_before_download(data_ids, session, httr_verbose, ...,
            timeout_seconds = timeout_seconds, before = before
        )
    }
    body <- fedstat_form(data_ids, session, data_format)
    # A failed POST may have consumed the token too. The recovery owner decides
    # whether to refresh; this primitive never silently repeats an export.
    session$used <- TRUE
    response <- tryCatch(fedstat_request("POST",
        paste0(session$base_url, "/indicator/downloadData.do?format=", data_format),
        httr_verbose, httr::content_type("application/x-www-form-urlencoded"),
        ...,
        body = body, handle = session$handle, timeout_seconds = timeout_seconds,
        stage = "export", before = before
    ), fedstat_redirect = function(e) {
        if (isTRUE(session$preview_empty)) {
            e$evidence <- "empty_preview"
            e$message <- "Fedstat reported an empty preview but redirected the SDMX export. This part is not yet confirmed."
        }
        stop(e)
    })
    fedstat_export_content(response, data_format)
}

fedstat_form <- function(data_ids, session, format, grid = FALSE) {
    required <- c("filter_field_id", "filter_value_id", "filter_value_title", "filter_field_object_ids")
    if (!is.data.frame(data_ids) || !all(required %in% names(data_ids)) || !nrow(data_ids) ||
        anyNA(data_ids[required])) {
        stop("data_ids must contain non-missing filter IDs, labels and layout.", call. = FALSE)
    }
    indicator <- data_ids[data_ids$filter_field_id == "0", , drop = FALSE]
    if (nrow(indicator) != 1L || indicator$filter_value_id != session$indicator_id) {
        stop("data_ids must contain exactly the requested indicator.", call. = FALSE)
    }
    fields <- unique(data_ids[c("filter_field_id", "filter_field_object_ids")])
    if (anyDuplicated(fields$filter_field_id) ||
        any(!fields$filter_field_object_ids %in% c("lineObjectIds", "columnObjectIds", "filterObjectIds"))) {
        stop("Each filter field must have one valid export layout.", call. = FALSE)
    }
    values <- c(
        indicator$filter_value_title, session$token_name, session$token,
        session$indicator_id, fields$filter_field_id,
        paste0(data_ids$filter_field_id, "_", data_ids$filter_value_id)
    )
    keys <- c(
        "title", "struts.token.name", session$token_name, "id",
        fields$filter_field_object_ids, rep("selectedFilterIds", nrow(data_ids))
    )
    if (grid) {
        # The browser puts hidden/filter dimensions on rows in its preview.
        keys <- c(
            ifelse(fields$filter_field_object_ids == "filterObjectIds", "lineObjectIds", fields$filter_field_object_ids),
            rep("selectedFilterIds", nrow(data_ids))
        )
        values <- c(fields$filter_field_id, paste0(data_ids$filter_field_id, "_", data_ids$filter_value_id))
    }
    encode <- function(x) vapply(enc2utf8(as.character(x)), utils::URLencode, "", reserved = TRUE)
    paste0(encode(keys), "=", encode(values), collapse = "&")
}

fedstat_export_content <- function(response, format) {
    content <- response$content
    # Error text is classified, never echoed: it can contain session tokens.
    # ASCII signatures only: the byte prefix may end inside a UTF-8 character.
    prefix <- tryCatch(rawToChar(head(content, 65536L)), error = function(e) "")
    type <- response$headers[["content-type"]]
    if (is.null(type)) type <- ""
    type <- tolower(trimws(strsplit(paste0(type, ";"), ";", fixed = TRUE)[[1L]][1L]))
    html <- grepl("<!DOCTYPE\\s+html|<html", prefix, ignore.case = TRUE, perl = TRUE, useBytes = TRUE) || type == "text/html"
    if (html) {
        evidence <- fedstat_access_marker(content)
        if (!is.null(evidence)) {
            fedstat_error(
                "Fedstat returned a possible access check or refusal; this does not confirm a CAPTCHA.",
                "blocked", "export", response$status_code,
                evidence = evidence
            )
        }
        if (grepl("CSRF|invalid[ ._-]*token|token.{0,30}(expired|invalid|used)", prefix, ignore.case = TRUE, useBytes = TRUE)) {
            fedstat_error("Fedstat rejected the download token (CSRF); refresh the session and retry.", "token", "export")
        }
        fedstat_error(
            "Fedstat returned an HTML page instead of data; the selection or service may be unavailable.",
            "unexpected_response", "export"
        )
    }
    valid_type <- if (format == "sdmx") {
        type %in% c("text/xml", "application/xml", "application/octet-stream")
    } else {
        type %in% c("application/vnd.ms-excel", "application/octet-stream")
    }
    if (!length(content) || !valid_type) {
        fedstat_error(
            paste0("Expected ", format, " export; received Content-Type: ", type, "."),
            "unexpected_response", "export"
        )
    }
    content
}
