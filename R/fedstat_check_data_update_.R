#' Compares reference_data and new_data for any changes
#'
#' @inheritParams fedstat_data_load_with_filters
#' @param reference_data_ids_unfiltered_special_cases_handled data_ids of reference_data
#'  without filtering and with special cases handled
#' @param reference_data_df data_df of reference_data
#' @param time_filter_fields_titles character, title names of time filter fields
#'  in `filters` list, currently only default values are allowed
#' @param time_fields_titles_in_df character, title names of time related columns
#'  in `data_df` (result of `fedstat_parse_sdmx_to_table`), currently only default values are allowed
#'
#' @return bool, TRUE if indicator data has been updated, FALSE if not
fedstat_check_data_update_ <- function(indicator_id,
                                       reference_data_ids_unfiltered_special_cases_handled,
                                       reference_data_df,
                                       ...,
                                       filters = list(),
                                       time_filter_fields_titles =
                                         c(
                                           "\\u041f\\u0435\\u0440\\u0438\\u043e\\u0434", # Period in russian
                                           "\\u0413\\u043e\\u0434" # Year in russian
                                         ),
                                       time_fields_titles_in_df = c(
                                         "PERIOD",
                                         "Time"
                                       ),
                                       filter_value_title_alias_lookup_table = data.frame(
                                         filter_value_title = character(),
                                         filter_value_title_alias = character(),
                                         stringsAsFactors = FALSE
                                       ),
                                       timeout_seconds = 180,
                                       retry_max_times = 3,
                                       disable_warnings = FALSE,
                                       httr_verbose = httr::verbose(data_out = FALSE)) {
  for (i in seq_len(length(time_filter_fields_titles))) {
    time_filter_fields_titles[i] <- eval(parse(text = paste0("'", time_filter_fields_titles[i], "'")))
  }

  if (any(!(time_fields_titles_in_df %in% names(reference_data_df)))) {
    stop(
      "These filters ",
      paste(
        time_fields_titles_in_df[!(time_fields_titles_in_df %in% names(reference_data_df))],
        collapse = ", "
      ),
      " are not in the reference_data_df columns. ",
      "Most likely, this indicator uses special time indicators and currently cannot be processed by this function"
    )
  } else if (!identical(time_filter_fields_titles, c(
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

  new_data_ids_unfiltered_special_cases_handled <- fedstat_get_data_ids(
    indicator_id = indicator_id,
    ... = ...,
    timeout_seconds = timeout_seconds,
    retry_max_times = retry_max_times,
    httr_verbose = httr_verbose
  ) %>%
    fedstat_get_data_ids_special_cases_handle(
      filter_value_title_alias_lookup_table = filter_value_title_alias_lookup_table
    )

  # check by data_ids
  data_ids_filter_ids_diff <- dplyr::setdiff(
    dplyr::distinct(
      new_data_ids_unfiltered_special_cases_handled,
      .data[["filter_field_id"]], .data[["filter_value_id"]]
    ),
    dplyr::distinct(
      reference_data_ids_unfiltered_special_cases_handled,
      .data[["filter_field_id"]], .data[["filter_value_id"]]
    )
  )

  if (as.logical(nrow(data_ids_filter_ids_diff))) {
    return(TRUE)
  }

  last_year <- dplyr::filter(
    new_data_ids_unfiltered_special_cases_handled,
    .data[["filter_field_id"]] == "3"
  ) %>% # id 3 = Year
    dplyr::pull("filter_value_title") %>%
    as.numeric() %>%
    max()

  new_data_ids_filtered <- fedstat_data_ids_filter(
    data_ids = new_data_ids_unfiltered_special_cases_handled,
    filters = filters,
    disable_warnings = disable_warnings
  )

  new_data_df <- fedstat_post_data_ids_filtered(
    data_ids = new_data_ids_filtered,
    ... = ...,
    data_format = "sdmx",
    timeout_seconds = timeout_seconds,
    retry_max_times = retry_max_times,
    httr_verbose = httr_verbose
  ) %>%
    fedstat_parse_sdmx_to_table()

  data_df_time_diff <- dplyr::setdiff(
    dplyr::distinct(
      new_data_df,
      dplyr::across(dplyr::all_of(time_fields_titles_in_df))
    ),
    dplyr::distinct(
      dplyr::filter(reference_data_df, .data[["Time"]] == last_year),
      dplyr::across(dplyr::all_of(time_fields_titles_in_df))
    )
  )

  return(as.logical(nrow(data_df_time_diff)))
}
