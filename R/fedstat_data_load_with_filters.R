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
#'   The `filter field` reflects the individual property specific value of the data point.
#'   (e.g. 2021 for Year, "Russian Federation" for Region, etc.)
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
#' @inheritParams fedstat_data_ids_filter
#' @inheritParams fedstat_get_data_ids_special_cases_handle
#'
#' @return data.frame with filtered indicator data from fedstat.ru
#' @export
#'
#' @seealso \code{
#'   \link{fedstat_get_data_ids},
#'   \link{fedstat_get_data_ids_special_cases_handle},
#'   \link{fedstat_data_ids_filter},
#'   \link{fedstat_post_data_ids_filtered},
#'   \link{fedstat_parse_sdmx_to_table}
#'   }
#'
#' @examples
#' \dontrun{
#' # Download weekly goods and services prices data for week 21 and 22 of 2021
#' # for all goods and services for Russian Federation
#' data <- fedstat_data_load_with_filters(
#'   indicator_id = "37426",
#'   filters = list(
#'     "Territory" = "Russian Federation",
#'     "Year" = "2021",
#'     "Period" = c(21, 22),
#'     "Types of goods and services" = "*"
#'   )
#' )
#' # Not actual filter field titles and filter values titles because of ASCII requirement for CRAN
#' }
fedstat_data_load_with_filters <- function(indicator_id,
                                           filters,
                                           filter_value_title_alias_lookup_table = data.frame(
                                             filter_value_title = character(),
                                             filter_value_title_alias = character()
                                           ),
                                           disable_warnings = FALSE,
                                           httr_verbose = httr::verbose(data_out = FALSE)) {
  data <- fedstat_get_data_ids(indicator_id, httr_verbose = httr_verbose) %>%
    fedstat_get_data_ids_special_cases_handle(
      filter_value_title_alias_lookup_table = filter_value_title_alias_lookup_table
    ) %>%
    fedstat_data_ids_filter(
      filters = filters,
      disable_warnings = disable_warnings
    ) %>%
    fedstat_post_data_ids_filtered(httr_verbose = httr_verbose) %>%
    fedstat_parse_sdmx_to_table()

  return(data)
}
