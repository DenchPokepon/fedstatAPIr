#' (Deprecated) Download indicator data from fedstat.ru in one request
#'
#' @description **Deprecated: use [fedstat_download()] and [fedstat_collect()].**
#'   This function sends the whole selection to Fedstat as one request, with no
#'   pauses or saved progress. Large selections are slow, often fail and put
#'   heavy load on Fedstat. It is kept so that existing scripts keep working.
#'   Replace `data <- fedstat_data_load_with_filters("31074", filters = filters)`
#'   with `job <- fedstat_download("31074", filters = filters)` and
#'   `data <- fedstat_collect(job)`.
#'
#'   Size is the number of filter combinations: the product of the number of
#'   selected values of every filter. Up to 50,000 combinations, a deprecation
#'   message is shown once per session. Above 50,000, every call warns. Above
#'   100,000, the function stops unless `allow_large_requests = TRUE`, and above
#'   250,000 it always stops: such requests load Fedstat for all its users and
#'   rarely succeed.
#'
#'   This function is a wrapper for the other functions of the package
#'   to provide a simple one function API for fedstat.ru
#'
#'   There are two basic terms in this API: `filter_field` and `filter_value`
#'
#'   The `filter field` reflects the individual property of the data point.
#'   For example, Year, Region, Unit of measurement, etc.
#'   Each filter field has its own title (`filter_field_title`),
#'   it is simply a human-readable word or phrase (e.g. "Year", "Region")
#'   that reflects the essence of the property by which filtering takes place
#'
#'   The `filter value` reflects the individual property specific value of the data point.
#'   (e.g. 2021 for the Year, "Russian Federation" for the region, etc.)
#'   It also has a title (`filter_value_title`) with
#'   the same purpose as `filter_field_title`
#'
#'   `filters` should use `filter_field_title` in names and `filter_value_title` in values as
#'   they are presented on fedstat.ru. If for some reason the specified filters
#'   do not return the expected result, it is worth using
#'   \code{\link{fedstat_get_data_ids}} separately and inspecting possible
#'   filter values in `data_ids` to see if the strings are defined correctly
#'   (e.g. encoding issues, mixing latin and cyrillic symbols)
#'
#'   `filter_value_title` currently supports the following special values:
#'   1. asterix (*), alias for "select all possible filter values for this filter field"
#'
#'   All filter fields with more than one possible value must be explicitly
#'   specified in `filters`. Use `"*"` to select all values.
#'   Unspecified multi-value filters will produce an error.
#'
#'   Internally normalized `filter_field_title` and `filter_value_title` are
#'   used (all lowercase, removed extra whitespaces)
#'   to compare the equality of `data_ids` and `filters`
#'
#' @inheritParams fedstat_get_data_ids
#' @param ... other arguments passed to httr::GET and httr::POST
#' @inheritParams fedstat_data_ids_filter
#' @param timeout_seconds numeric, maximum time before a new GET and POST request is tried
#' @param retry_max_times numeric, maximum pipeline attempts, including the first.
#'   Only transient failures and one session repair are retried. Filter metadata
#'   are retained after a successful GET; retries do not nest.
#' @param loading_steps_verbose logical, print data loading steps to console
#' @inheritParams fedstat_parse_sdmx_to_table
#' @param allow_large_requests logical. `TRUE` allows selections of up to
#'   250,000 filter combinations instead of 100,000. Larger selections are
#'   always refused; use [fedstat_download()] for them.
#'
#' @return data.frame with filtered indicator data from fedstat.ru
#' @export
#'
#' @seealso \code{
#'   \link{fedstat_download},
#'   \link{fedstat_get_data_ids},
#'   \link{fedstat_data_ids_filter},
#'   \link{fedstat_post_data_ids_filtered},
#'   \link{fedstat_parse_sdmx_to_table}
#'   }
#'
#' @examples
#' \dontrun{
#' # Download CPI data
#' # for all goods and services for Russian Federation
#' data <- fedstat_data_load_with_filters(
#'     indicator_id = "31074",
#'     filters = list(
#'         "Territory" = "Russian Federation",
#'         "Year" = "2023",
#'         "Period" = "January",
#'         "Types of goods and services" = "*"
#'     )
#' )
#' # Not actual filter field titles and filter values titles because of ASCII requirement for CRAN
#' }
fedstat_data_load_with_filters <- function(indicator_id,
                                           ...,
                                           filters = list(),
                                           timeout_seconds = 180,
                                           retry_max_times = 3,
                                           disable_warnings = FALSE,
                                           httr_verbose = NULL,
                                           loading_steps_verbose = TRUE,
                                           return_type = c("data", "dictionary"),
                                           try_to_parse_ObsValue = TRUE,
                                           allow_large_requests = FALSE) {
    id <- tryCatch(fedstat_indicator_id(indicator_id), error = function(e) "<indicator code>")
    fedstat_positive(retry_max_times, "retry_max_times", integer = TRUE)
    fedstat_flag(allow_large_requests, "allow_large_requests")
    advice <- fedstat_download_advice(id)
    # Small selections get one friendly notice per session; large ones warn on
    # every call; oversized ones stop before anything is sent.
    deprecation <- function(data_ids) {
        cells <- fedstat_nominal_cells(data_ids)
        if (fedstat_check_size(cells, allow_large_requests, advice)) {
            warning(structure(
                class = c("fedstat_deprecated", "deprecatedWarning", "warning", "condition"),
                list(message = paste0(
                    "fedstat_data_load_with_filters() is deprecated. This selection has ",
                    fedstat_count(cells), " filter combinations, and this function sends them to ",
                    "Fedstat as one request: it is slow, often fails and loads Fedstat heavily. ",
                    advice
                ), call = NULL)
            ))
        } else if (!isTRUE(fedstat_state$deprecation_shown)) {
            fedstat_state$deprecation_shown <- TRUE
            message(structure(
                class = c("fedstat_deprecated", "deprecatedMessage", "message", "condition"),
                list(message = paste0(
                    "fedstat_data_load_with_filters() is deprecated but still works for small ",
                    "selections like this one. ", advice, "\n",
                    "This message is shown once per session.\n"
                ), call = NULL)
            ))
        }
    }
    data_ids <- NULL
    refreshed <- FALSE
    for (attempt in seq_len(retry_max_times)) {
        result <- tryCatch(
            {
                if (is.null(data_ids)) {
                    if (loading_steps_verbose) message("Loading indicator ", indicator_id, " filters ...")
                    ids <- fedstat_get_data_ids(indicator_id, ...,
                        timeout_seconds = timeout_seconds,
                        retry_max_times = 1L, httr_verbose = httr_verbose
                    )
                    data_ids <- fedstat_data_ids_filter(ids, filters, disable_warnings)
                    deprecation(data_ids)
                }
                if (loading_steps_verbose) message("Downloading indicator ", indicator_id, " ...")
                # The size was checked and reported above.
                bytes <- fedstat_post_data_ids_filtered(data_ids, ...,
                    timeout_seconds = timeout_seconds,
                    retry_max_times = 1L, httr_verbose = httr_verbose,
                    allow_large_requests = TRUE
                )
                fedstat_parse_sdmx_to_table(bytes, return_type, try_to_parse_ObsValue)
            },
            error = identity
        )
        if (!inherits(result, "error")) {
            if (loading_steps_verbose) message("Done")
            return(result)
        }
        token <- inherits(result, "fedstat_token") || inherits(result, "fedstat_redirect")
        if (attempt == retry_max_times || (!fedstat_retryable(result) && !(token && !refreshed))) stop(result)
        if (!is.null(data_ids)) {
            # Invalidate session state, keep the already resolved filter selection.
            session <- fedstat_session(data_ids)
            session$token <- NULL
            data_ids <- fedstat_attach_session(data_ids, session)
        }
        if (token) refreshed <- TRUE
        if (loading_steps_verbose) {
            message(
                conditionMessage(result), " Retrying (", attempt + 1L,
                "/", retry_max_times, ") ..."
            )
        }
        Sys.sleep(fedstat_backoff(attempt, result))
    }
}
