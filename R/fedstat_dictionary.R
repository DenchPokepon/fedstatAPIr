#' Download the code lists (dictionary) of an indicator
#'
#' @description Returns the classifications Fedstat uses in the data of an
#'   indicator: for every coded column, all its codes and their names. The data
#'   from [fedstat_collect()] has a column with names (such as `s_grtov`) and a
#'   column with codes (`s_grtov_code`) for each classification; this table lists
#'   every code, including ones absent from your download.
#'
#'   Fedstat sends these lists only together with data, so the function downloads
#'   a small piece of data and keeps only its code lists. Any piece will do: every
#'   export carries the complete lists. It first tries the selection the website
#'   shows when the indicator page opens, and then small random selections if
#'   that one is empty.
#'
#' @details The code lists come from the SDMX export, not from the indicator
#'   page. Names usually match the filter values on the page, but codes are a
#'   different system from the IDs in [fedstat_get_data_ids()]. Year, period and
#'   unit of measurement are not coded and are not listed.
#'
#'   Each try is one small request (at most 5,000 filter combinations), with a
#'   pause between tries.
#'
#' @param indicator_id Indicator code from its page address, for example `"31074"`.
#' @param filters Optional selection to request, as in [fedstat_download()].
#'   Useful when the automatic selections return no data: choose values that show
#'   data on the website. Then only this selection is tried.
#' @param ... httr settings applied to every request, for example
#'   `httr::user_agent("my-project")`. Arguments after `...` must be named in full.
#' @param attempts Most requests for data, including retries after network errors.
#' @param timeout_seconds Longest wait for one request, in seconds.
#' @param retry_max_times Attempts to open the indicator page, including the first.
#' @param request_pause Seconds between attempts: one number, or two numbers for a
#'   random pause in that range.
#' @param verbose `TRUE` shows short progress messages.
#'
#' @return A data.frame with one row per code: `field_id` (the column name in the
#'   data), `field_title` (the classification name), `value_id` (the code) and
#'   `value_title` (its name).
#' @seealso [fedstat_show_filters()] for the filters of the indicator page.
#' @export
#'
#' @examples
#' \dontrun{
#' dictionary <- fedstat_dictionary("31074")
#' # All codes of one classification
#' dictionary[dictionary$field_id == "s_grtov", c("value_id", "value_title")]
#' }
fedstat_dictionary <- function(indicator_id, filters = NULL, ...,
                               attempts = 5L, timeout_seconds = 180,
                               retry_max_times = 3L, request_pause = c(10, 20),
                               verbose = TRUE) {
    fedstat_check_dots(list(...), "fedstat_dictionary")
    for (name in c("attempts", "timeout_seconds", "retry_max_times")) {
        fedstat_positive(get(name), name, integer = name != "timeout_seconds")
    }
    fedstat_pause_check(request_pause)
    fedstat_flag(verbose, "verbose")
    indicator_id <- fedstat_indicator_id(indicator_id)
    if (!is.null(filters)) filters <- fedstat_filters(filters)
    if (identical(filters, "*")) {
        stop("filters = \"*\" selects the whole indicator; a small selection is enough.", call. = FALSE)
    }
    if (verbose) message("Reading the filters of indicator ", indicator_id, " ...")
    ids <- fedstat_read_ids(indicator_id, ...,
        timeout_seconds = timeout_seconds, retry_max_times = retry_max_times
    )
    session <- fedstat_session(ids)
    candidates <- if (is.null(filters)) {
        fedstat_dictionary_candidates(ids, attempts)
    } else {
        list(fedstat_data_ids_filter(ids, filters))
    }
    index <- 1L
    for (attempt in seq_len(attempts)) {
        if (attempt > 1L) {
            pause <- stats::runif(1L, min(request_pause), max(request_pause))
            Sys.sleep(if (fedstat_retryable(result)) max(pause, fedstat_backoff(attempt - 1L, result)) else pause)
        }
        selected <- fedstat_attach_session(candidates[[index]], session)
        if (verbose) {
            cells <- fedstat_nominal_cells(selected)
            message(
                "Downloading a sample of ", fedstat_count(cells), " filter combination",
                if (cells != 1) "s", " for the code lists (", attempt, "/", attempts, ") ..."
            )
        }
        result <- tryCatch(
            {
                bytes <- fedstat_post_data_ids_filtered(selected, ...,
                    timeout_seconds = timeout_seconds, retry_max_times = retry_max_times
                )
                fedstat_parse_sdmx(bytes, "dictionary", indicator_id = indicator_id)
            },
            error = identity
        )
        if (!inherits(result, "error")) {
            if (verbose) message("Done")
            return(as.data.frame(result))
        }
        # The export may have used up the token; the next request gets a new one.
        session$token <- NULL
        if (inherits(result, "fedstat_redirect") || inherits(result, "fedstat_unexpected_response")) {
            # Fedstat answers a selection without data with a redirect.
            index <- index + 1L
            if (index > length(candidates)) break
            if (verbose) message("Fedstat returned no data for this sample; trying another one.")
        } else if (!fedstat_retryable(result) && !inherits(result, "fedstat_token")) {
            stop(result)
        } else if (verbose) {
            message(conditionMessage(result), " Retrying ...")
        }
    }
    if (inherits(result, "fedstat_redirect") || inherits(result, "fedstat_unexpected_response")) {
        fedstat_error(paste0(
            "Fedstat returned no data for ",
            if (is.null(filters)) "the samples tried" else "this selection",
            ", so the code lists of indicator ", indicator_id, " could not be read. ",
            if (is.null(filters)) {
                paste0(
                    "Pass filters with values that show data on the indicator page: ",
                    "fedstat_dictionary(\"", indicator_id, "\", filters = <your filters>)."
                )
            } else {
                "Choose filter values that show data on the indicator page."
            }
        ), "no_data", "export")
    }
    stop(result)
}

# The website opens the indicator with a table, so its initial selection usually
# has data. Further samples are random, a few values of every filter.
fedstat_dictionary_candidates <- function(data_ids, n, max_cells = 5000) {
    first <- fedstat_shrink(fedstat_default_selection(data_ids), max_cells)
    c(list(first), lapply(seq_len(n - 1L), function(i) fedstat_random_selection(data_ids, max_cells)))
}

# Values marked as selected on the indicator page; a filter without marked
# values gets its first value.
fedstat_default_selection <- function(data_ids) {
    keys <- paste0(data_ids$filter_field_id, "_", data_ids$filter_value_id)
    chosen <- keys %in% attr(data_ids, "fedstat_default_selection")
    fields <- data_ids$filter_field_id
    chosen <- chosen | (!fields %in% fields[chosen] & !duplicated(fields))
    data_ids[chosen, , drop = FALSE]
}

# Halve the filter with the most values until the selection fits.
fedstat_shrink <- function(selection, max_cells) {
    while (fedstat_nominal_cells(selection) > max_cells) {
        counts <- table(selection$filter_field_id)
        rows <- which(selection$filter_field_id == names(counts)[which.max(counts)])
        selection <- selection[-rows[-seq_len(ceiling(length(rows) / 2))], , drop = FALSE]
    }
    selection
}

fedstat_random_selection <- function(data_ids, max_cells) {
    groups <- split(seq_len(nrow(data_ids)), data_ids$filter_field_id)
    many <- max(1L, sum(lengths(groups) > 1L))
    size <- max(1L, floor(max_cells^(1 / many) + 1e-9))
    rows <- unlist(lapply(groups, function(x) x[sample.int(length(x), min(size, length(x)))]))
    data_ids[sort(rows), , drop = FALSE]
}
