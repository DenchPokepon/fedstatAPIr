#' Loads the data that will be used to compare with the new data to detect data updates
#'
#' @description Used only as a first argument in \link{fedstat_check_data_update}.
#'   Stores the original filters, data_ids, data_df and other arguments.
#'   Combines it all into one object, which ensures that filters and other
#'   arguments are identical for old and new data queries.
#'
#'   You *shouldn't* specify time filters in `filters` list.
#'   This function does this automatically, it takes all PERIODs and the last year.
#'
#'   Please try to specify filters as much as possible
#'   (for example, using filters only for Russia and a certain type of
#'   indicator/good/service, etc.),
#'   it will reduce the query execution time and server load on fedstat.ru.
#'   You don't need all the data to check the data updates anyway,
#'   one observation with the new date is enough. However, keep in mind that you
#'   need to choose observations that are sure to be in the new data release.
#'
#' @inheritParams fedstat_data_load_with_filters
#' @param time_filter_fields_titles character, title names of time filter fields
#'  in `filters` list, currently only default values are allowed
#' @param time_fields_titles_in_df character, title names of time related columns
#'  in `data_df` (result of `fedstat_parse_sdmx_to_table`), currently only default values are allowed
#'
#' @return list with the following fields: indicator_id,
#'  filters,
#'  time_filter_fields_titles,
#'  time_fields_titles_in_df,
#'  filter_value_title_alias_lookup_table,
#'  reference_data_ids_unfiltered_special_cases_handled,
#'  reference_data_df
#'
#' @export
#' @examples
#' \dontrun{
#' reference_data <- fedstat_prepare_reference_data_for_check_data_update(
#'   indicator_id = "37426",
#'   filters = list(
#'     "Territory" = "Russian Federation",
#'     "Types of goods and services" = "Sahar-pesok, kg"
#'   )
#' )
#' # Not actual filter field titles and filter values titles because of ASCII requirement for CRAN
#' }
fedstat_prepare_reference_data_for_check_data_update <- function(indicator_id,
                                                                 ...,
                                                                 filters = list(),
                                                                 time_filter_fields_titles =
                                                                   c(
                                                                     "\u041f\u0435\u0440\u0438\u043e\u0434", # Period in russian
                                                                     "\u0413\u043e\u0434" # Year in russian
                                                                   ),
                                                                 time_fields_titles_in_df = c(
                                                                   "PERIOD",
                                                                   "Time"
                                                                 ),
                                                                 filter_value_title_alias_lookup_table = data.frame(
                                                                   filter_value_title = character(),
                                                                   filter_value_title_alias = character()
                                                                 ),
                                                                 timeout_seconds = 180,
                                                                 retry_max_times = 3,
                                                                 disable_warnings = FALSE,
                                                                 httr_verbose = httr::verbose(data_out = FALSE)) {
  if (!identical(time_filter_fields_titles, c(
    "\u041f\u0435\u0440\u0438\u043e\u0434", # Period in russian
    "\u0413\u043e\u0434" # Year in russian
  ))) {
    stop("time_filter_fields_titles is not configurable right now, please use the default value")
  } else if (!identical(time_fields_titles_in_df, c(
    "PERIOD",
    "Time"
  ))) {
    stop("time_fields_titles_in_df is not configurable right now, please use the default value")
  }

  filters_copy <- filters

  for (i in time_filter_fields_titles) {
    if (i %in% names(filters_copy)) {
      stop("This function automatically uses all PERIODs and last available in data_ids Year, do not specify time filters in filters list")
    } else if (i != "\u0413\u043e\u0434") {
      filters_copy[[i]] <- "*"
    }
  }

  reference_data_ids_unfiltered_special_cases_handled <- fedstat_get_data_ids(
    indicator_id = indicator_id,
    ... = ...,
    timeout_seconds = timeout_seconds,
    retry_max_times = retry_max_times,
    httr_verbose = httr_verbose
  ) %>%
    fedstat_get_data_ids_special_cases_handle(
      filter_value_title_alias_lookup_table = filter_value_title_alias_lookup_table
    )

  last_year <- dplyr::filter(
    reference_data_ids_unfiltered_special_cases_handled,
    .data[["filter_field_id"]] == "3"
  ) %>% # id 3 = Year
    dplyr::pull("filter_value_title") %>%
    as.numeric() %>%
    max()

  filters_copy[["\u0413\u043e\u0434"]] <- last_year

  reference_data_ids_filtered <- fedstat_data_ids_filter(
    data_ids = reference_data_ids_unfiltered_special_cases_handled,
    filters = filters_copy,
    disable_warnings = disable_warnings
  )

  reference_data_df <- fedstat_post_data_ids_filtered(
    data_ids = reference_data_ids_filtered,
    ... = ...,
    data_format = "sdmx",
    timeout_seconds = timeout_seconds,
    retry_max_times = retry_max_times,
    httr_verbose = httr_verbose
  ) %>%
    fedstat_parse_sdmx_to_table()

  if (nrow(reference_data_df) > 100 && !disable_warnings) {
    warning(
      "reference_data_df has ", nrow(reference_data_df), " rows. ",
      "Such requests cause a significant load on the fedstat.ru, please try to specify filters more specifically. ",
      "Try to use data for one region/good/service/type of indicator/etc. as a proxy for data update",
      "This is just a ethical suggestion, the function has not been broken and will continue executing"
    )
  }

  return(
    list(
      "indicator_id" = indicator_id,
      "filters" = filters_copy,
      "time_filter_fields_titles" = time_filter_fields_titles,
      "time_fields_titles_in_df" = time_fields_titles_in_df,
      "filter_value_title_alias_lookup_table" = filter_value_title_alias_lookup_table,
      "reference_data_ids_unfiltered_special_cases_handled" = reference_data_ids_unfiltered_special_cases_handled,
      "reference_data_df" = reference_data_df
    )
  )
}
