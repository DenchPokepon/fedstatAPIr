#' Download subset of indicator data from fedstat.ru by specifying filters in JSON
#'
#' @description This function is a wrapper for the other functions of the package
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
#'   Unspecified filters use asterix as a default
#'   (i.e. all possible filter values are selected and a warning is given)
#'
#'   Internally normalized `filter_field_title` and `filter_value_title` are
#'   used (all lowercase, removed extra whitespaces)
#'   to compare the equality of `data_ids` and `filters`
#'
#' @inheritParams fedstat_get_data_ids
#' @param ... other arguments passed to httr::GET and httr::POST
#' @inheritParams fedstat_data_ids_filter
#' @param timeout_seconds numeric, maximum time before a new GET and POST request is tried
#' @param retry_max_times numeric, maximum number of tries to GET and POST `data_ids`
#' @param loading_steps_verbose logical, print data loading steps to console
#' @inheritParams fedstat_parse_sdmx_to_table
#'
#' @return data.frame with filtered indicator data from fedstat.ru
#' @export
#'
#' @seealso \code{
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
#'   indicator_id = "31074",
#'   filters = list(
#'     "Territory" = "Russian Federation",
#'     "Year" = "2023",
#'     "Period" = "January",
#'     "Types of goods and services" = "*"
#'   )
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
                                           try_to_parse_ObsValue = TRUE) {
  if (loading_steps_verbose) {
    cat(
      "Downloading EMISS internal identificators for the https://www.fedstat.ru/indicator/",
      indicator_id, " ...\n",
      sep = ""
    )
  }
  data_ids <- fedstat_get_data_ids(
    indicator_id,
    ... = ...,
    timeout_seconds = timeout_seconds,
    retry_max_times = retry_max_times,
    httr_verbose = httr_verbose
  )

  data_ids_filtered <- fedstat_data_ids_filter(
    data_ids = data_ids,
    filters = filters,
    disable_warnings = disable_warnings
  )

  if (loading_steps_verbose) cat("Downloading data from EMISS ...\n")
  data_raw <- fedstat_post_data_ids_filtered(
    data_ids = data_ids_filtered,
    ... = ...,
    timeout_seconds = timeout_seconds,
    retry_max_times = retry_max_times,
    httr_verbose = httr_verbose
  )

  if (loading_steps_verbose) cat("Parsing data ...\n")
  data_data_frame <- fedstat_parse_sdmx_to_table(
    data_raw = data_raw, 
    return_type = return_type, 
    try_to_parse_ObsValue = try_to_parse_ObsValue
    )

  if (loading_steps_verbose) cat("Done\n")

  return(data_data_frame)
}
