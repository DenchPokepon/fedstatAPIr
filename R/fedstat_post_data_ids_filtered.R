#' Post data filters ids to fedstat.ru and download specified subset of data
#'
#' @description Creates a request body from `data_ids` and sends it to
#'   \code{https://www.fedstat.ru/indicator/downloadData.do?format=\{format\}}.
#'   Gets an sdmx or excel with data in binary format.
#'
#'   The request includes a CSRF token that is automatically extracted by
#'   \code{fedstat_get_data_ids} and passed through via data.frame attributes.
#'   CSRF tokens are single-use: each POST consumes the token. For subsequent
#'   downloads, call \code{fedstat_get_data_ids} again.
#'
#'   If no CSRF token is found in `data_ids` attributes (e.g. when using cached
#'   data_ids from a previous session), the function will attempt to fetch a
#'   fresh token automatically with a warning.
#'
#'   sdmx raw bytes can be passed to `fedstat_parse_sdmx_to_table` to create a
#'   data.frame or to `rawToChar` and `writeLines` to create an xml file
#'
#'   excel raw bytes can be passed to `writeBin` to create an xls file
#'
#' @param data_ids data.frame, can be a result of `fedstat_get_data_ids` or
#'   `fedstat_get_data_ids_special_cases_handle` to download all available data,
#'   or a result of `fedstat_data_ids_filter` to download subset of available data
#' @param ... other arguments passed to httr::POST
#' @param data_format string, one of sdmx, excel
#' @param timeout_seconds numeric, maximum time before POST request times out
#' @param retry_max_times numeric, not used for POST (CSRF tokens are single-use),
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
#'   fedstat_data_ids_filter(
#'     filters = list(
#'       "Territory" = "Russian Federation",
#'       "Year" = "2023",
#'       "Period" = "January",
#'       "Types of goods and services" = "*"
#'     )
#'   ) %>%
#'   fedstat_post_data_ids_filtered()
#'
#' # Not actual filter field titles and filter values titles because of ASCII requirement for CRAN
#' }
fedstat_post_data_ids_filtered <- function(data_ids,
                                           ...,
                                           data_format = c("sdmx", "excel"),
                                           timeout_seconds = 180,
                                           retry_max_times = 3,
                                           httr_verbose = NULL) {

  # workaround for `:=` and CMD check
  filter_field_id <- filter_value_id <- filter_field_object_ids <- NULL

  # --- Extract attributes before data.table conversion (may strip custom attrs) ---
  csrf_token <- attr(data_ids, "fedstat_csrf_token")
  csrf_token_name <- attr(data_ids, "fedstat_csrf_token_name")
  indicator_id <- attr(data_ids, "fedstat_indicator_id")
  fedstat_handle <- attr(data_ids, "fedstat_handle")
  base_url <- attr(data_ids, "fedstat_base_url")

  data_ids <- data.table::as.data.table(data_ids)

  data_format <- match.arg(data_format, data_format)

  # Fallback: get indicator_id from data_ids content
  indicator <- data_ids[filter_field_id == "0", c("filter_value_id", "filter_value_title")]
  if (is.null(indicator_id)) {
    indicator_id <- indicator[["filter_value_id"]]
  }

  # Default base URL
  if (is.null(base_url)) {
    base_url <- "https://www.fedstat.ru"
  }

  # If no handle or no CSRF token, fetch both via a fresh GET request
  if (is.null(fedstat_handle) || is.null(csrf_token) || is.null(csrf_token_name)) {
    if (is.null(csrf_token) || is.null(csrf_token_name)) {
      warning(
        "No CSRF token found in data_ids attributes. ",
        "Fetching a fresh token from fedstat.ru (extra GET request). ",
        "To avoid this, use fedstat_get_data_ids() in the same R session ",
        "and pipe directly into fedstat_post_data_ids_filtered().",
        call. = FALSE
      )
    }

    fedstat_handle <- httr::handle(base_url)

    token_url <- paste0(base_url, "/indicator/", indicator_id)
    token_res <- httr::RETRY(
      "GET", token_url,
      handle = fedstat_handle,
      fedstat_default_config(),
      httr::timeout(timeout_seconds),
      times = retry_max_times
    )

    if (httr::http_error(token_res)) {
      stop("Failed to fetch CSRF token for indicator ", indicator_id, ". ",
           "HTTP status: ", httr::status_code(token_res),
           call. = FALSE)
    }

    token_html <- xml2::read_html(token_res, encoding = "UTF-8")
    holder <- xml2::xml_find_first(
      token_html, ".//div[@id='downloadTokenHolder']"
    )

    if (is.na(xml2::xml_text(holder))) {
      stop("Could not find downloadTokenHolder on indicator page for ", indicator_id, ". ",
           "EMISS may have changed their page structure.",
           call. = FALSE)
    }

    token_name_input <- xml2::xml_find_first(
      holder, ".//input[@name='struts.token.name']"
    )
    csrf_token_name <- xml2::xml_attr(token_name_input, "value")

    if (is.na(csrf_token_name)) {
      stop("Could not extract CSRF token name from indicator page.", call. = FALSE)
    }

    token_input <- xml2::xml_find_first(
      holder, paste0(".//input[@name='", csrf_token_name, "']")
    )
    csrf_token <- xml2::xml_attr(token_input, "value")

    if (is.na(csrf_token)) {
      stop("Could not extract CSRF token value from indicator page.", call. = FALSE)
    }
  }

  # --- Build POST URL ---
  POST_URL <- paste0(base_url, "/indicator/downloadData.do?format=", data_format)

  # --- Build URL-encoded POST body ---
  filters <- unique(data.table::as.data.table(data_ids), by = "filter_field_id")[
    , c("filter_field_id", "filter_field_object_ids")
  ]

  # Collect lineObjectIds, columnObjectIds, filterObjectIds
  line_ids <- filters[filter_field_object_ids == "lineObjectIds", filter_field_id]
  col_ids <- filters[filter_field_object_ids == "columnObjectIds", filter_field_id]
  filter_obj_ids <- filters[filter_field_object_ids == "filterObjectIds", filter_field_id]

  # Build selectedFilterIds: field_id + "_" + value_id for each row
  selected_ids <- paste0(data_ids[["filter_field_id"]], "_", data_ids[["filter_value_id"]])

  # Assemble body parts
  body_parts <- c(
    paste0("title=", utils::URLencode(indicator[["filter_value_title"]], reserved = TRUE)),
    paste0("struts.token.name=", csrf_token_name),
    paste0(csrf_token_name, "=", csrf_token),
    paste0("id=", indicator_id)
  )

  for (lid in line_ids) body_parts <- c(body_parts, paste0("lineObjectIds=", lid))
  for (cid in col_ids) body_parts <- c(body_parts, paste0("columnObjectIds=", cid))
  for (sid in selected_ids) body_parts <- c(body_parts, paste0("selectedFilterIds=", sid))
  for (foid in filter_obj_ids) body_parts <- c(body_parts, paste0("filterObjectIds=", foid))

  POST_body <- paste(body_parts, collapse = "&")

  # --- POST (no retry: CSRF tokens are single-use) ---
  POST_res <- httr::POST(
    POST_URL,
    config = c(
      fedstat_default_config(),
      httr::config(followlocation = FALSE)
    ),
    httr_verbose,
    httr::timeout(timeout_seconds),
    httr::content_type("application/x-www-form-urlencoded"),
    handle = fedstat_handle,
    body = POST_body,
    ... = ...
  )

  # --- Error handling ---
  status <- httr::status_code(POST_res)

  # Handle 302 redirect (server rejected the request)
  if (status == 302) {
    stop("EMISS rejected the download request (HTTP 302 redirect). ",
         "Possible causes:\n",
         "  - Filter combination produces no data\n",
         "  - Invalid filter value IDs\n",
         "  - Query too broad or too narrow\n",
         "Try adjusting your filters or inspect filter values with fedstat_get_data_ids().",
         call. = FALSE)
  }

  if (httr::http_error(POST_res)) {
    body_preview <- tryCatch(
      rawToChar(POST_res$content[seq_len(min(500, length(POST_res$content)))]),
      error = function(e) "<unreadable>"
    )

    if (status == 503) {
      stop("EMISS returned 503 (Service Unavailable). ",
           "The server is overloaded. Try again later or reduce query size.\n",
           "Response: ", body_preview,
           call. = FALSE)
    } else if (status == 403) {
      stop("EMISS returned 403 (Forbidden). ",
           "Your request was blocked by anti-bot protection. ",
           "Try passing custom headers via httr::set_config(httr::add_headers(...)). ",
           "See package README for details.\n",
           "Response: ", body_preview,
           call. = FALSE)
    } else {
      stop("EMISS returned HTTP ", status, ".\n",
           "Response: ", body_preview,
           call. = FALSE)
    }
  }

  # --- Validate response content type ---
  content_type <- POST_res[["headers"]][["content-type"]]

  if (is.null(content_type) ||
      !(content_type %in% c("text/xml", "application/vnd.ms-excel"))) {
    body_text <- tryCatch(
      rawToChar(POST_res$content[seq_len(min(500, length(POST_res$content)))]),
      error = function(e) ""
    )

    if (grepl("CSRF", body_text, ignore.case = TRUE)) {
      stop("EMISS rejected the request: CSRF token validation failed. ",
           "The token may have expired or been already used. ",
           "Call fedstat_get_data_ids() again to obtain a fresh token.",
           call. = FALSE)
    } else if (grepl("<html", body_text, ignore.case = TRUE)) {
      stop("EMISS returned an HTML page instead of data. ",
           "This usually means the request was rejected by the server. ",
           "Content-Type: ", if (is.null(content_type)) "<none>" else content_type, "\n",
           "Response: ", body_text,
           call. = FALSE)
    } else {
      stop("No data found with specified filters or the fedstat is lagging. ",
           "Content-Type: ", if (is.null(content_type)) "<none>" else content_type,
           call. = FALSE)
    }
  }

  return(POST_res[["content"]])
}
