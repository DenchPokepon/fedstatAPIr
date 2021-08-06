#' Filters `data_ids` based on `filters` that are given in JSON format
#'
#' @description Filters indicator `data_ids` with given `filters`
#'   taking into account possible `filters` specification errors and default filters.
#'
#'   `filters` should use `filter_field_title` in names and `filter_value_title` in values as
#'   they are presented on fedstat.ru. If for some reason the specified filters
#'   do not return the expected result, it is worth inspecting possible
#'   filter values in `data_ids` to see if the strings are defined correctly
#'   (e.g. encoding issues, mixing latin and cyrillic symbols)
#'
#'   `filter_value_title` currently supports the following special values:
#'   1. asterix (*), it's alias for "select all possible filter values for this filter field"
#'
#'   Unspecified filters use asterix as a default
#'   (i.e. all possible filter values are selected and a warning is given)
#'
#'   Internally normalized `filter_field_title` and `filter_value_title` are
#'   used (all lowercase, removed extra whitespaces)
#'   to compare the equality of `data_ids` and `filters`
#'
#' @param data_ids data.frame, result of `fedstat_get_data_ids`
#'   with or without conjunction with `fedstat_get_data_ids_special_cases_handle`
#' @param filters JSON in R list form. The structure should be like this:
#'
#' \preformatted{
#' {
#'  "filter_field_title1": ["filter_value_title1", "filter_value_title2"],
#'  "filter_field_title2": ["filter_value_title1", "filter_value_title2"],
#'  ...
#' }
#' }
#'
#'   Where for example `filter_field_title1` could be a string "Year" with
#'   `filter_value_title1` equal to 2020 and `filter_field_title2`
#'    could be a string "OKATO" with `filter_value_title1`
#'    equal to "Russian Federation"
#'   Not actual filter field titles and filter values titles
#'    because of ASCII requirement for CRAN
#' @param disable_warnings bool, enables or disables following warnings:
#' 1. About non matched `filter_value_title` in `filters` and `filter_value_title` from `data_ids`;
#' 2. About unspecified `filter_filed_title` in filters.
#'
#' @return data.frame, filtered `data_ids`
#' @export
#'
#' @seealso \code{\link{fedstat_get_data_ids},
#'   \link{fedstat_post_data_ids_filtered}}
#'
#' @importFrom rlang .data
#'
#' @examples
#' \dontrun{
#' # Get data filters identificators for week prices
#' # standardize names for DVFO and extract week numbers
#' # filter the data_ids to get data for week 21 and 22 of 2021
#' # for all goods and services for Russian Federation
#' data_ids_filtered <- fedstat_get_data_ids("37426") %>%
#'   fedstat_get_data_ids_special_cases_handle(
#'     filter_value_title_alias_lookup_table = data.frame(
#'       "filter_value_title" = "Dalnevostochnyj federalnyj okrug ( s 03.11.2018)",
#'       "filter_value_title_alias" = "Dalnevostochnyj federalnyj okrug"
#'     )
#'   ) %>%
#'   fedstat_data_ids_filter(
#'     filters = list(
#'       "Territory" = "Russian Federation",
#'       "Year" = "2021",
#'       "Period" = c(21, 22),
#'       "Types of goods and services" = "*"
#'     )
#'   )
#'
#' # In this example names for Far Eastern Federal District are latinized for CRAN
#' # Not actual filter field titles and filter values titles because of ASCII requirement for CRAN
#' }
fedstat_data_ids_filter <- function(data_ids, filters = list(), disable_warnings = FALSE) {

  ## Preparations
  original_data_ids_columns_order <- names(data_ids)

  str_norm <- function(x) tolower(stringr::str_squish(x))

  replace_str_norm_with_original <- function(data_ids_or_filters, lookup_table) {
    title <- stringr::str_subset(names(lookup_table), "\\.str_norm", negate = TRUE)
    title.str_norm <- paste0(title, ".str_norm")

    data_ids_or_filters_copy <- data_ids_or_filters

    names(data_ids_or_filters_copy)[
      names(data_ids_or_filters_copy) == title
    ] <- title.str_norm

    result <- data_ids_or_filters_copy %>%
      dplyr::left_join(lookup_table,
        by = title.str_norm
      ) %>%
      dplyr::select(-dplyr::all_of(title.str_norm))

    return(result)
  }

  data_ids_norm_filter_field_title <- data_ids %>%
    dplyr::distinct(.data[["filter_field_title"]]) %>%
    dplyr::mutate(
      filter_field_title.str_norm = str_norm(.data[["filter_field_title"]])
    )

  data_ids_norm_filter_value_title <- data_ids %>%
    dplyr::distinct(.data[["filter_value_title"]]) %>%
    dplyr::mutate(
      filter_value_title.str_norm = str_norm(.data[["filter_value_title"]])
    )

  data_ids_norm <- data_ids %>%
    dplyr::mutate(
      filter_field_title = str_norm(.data[["filter_field_title"]]),
      filter_value_title = str_norm(.data[["filter_value_title"]])
    )

  indicator_id_and_title <- data_ids[
    data_ids[["filter_field_id"]] == "0",
    c("filter_value_id", "filter_value_title"),
    drop = TRUE
  ]

  indicator_id <- indicator_id_and_title[["filter_value_id"]]
  indicator_title <- indicator_id_and_title[["filter_value_title"]]

  filters_added_indicator_title <- c(filters, list("Pokazatel" = indicator_title)) %>%
    `names<-`(c(
      names(filters),
      iconv(
        "\u041f\u043e\u043a\u0430\u0437\u0430\u0442\u0435\u043b\u044c",
        "UTF-8",
        "UTF-8"
      )
    )) # Pokazatel' in Russian written in UTF-8 escapes for CRAN

  filters_data_frame <- data.frame(
    filter_field_title = rep(
      names(filters_added_indicator_title),
      sapply(filters_added_indicator_title, length, simplify = TRUE)
    ),
    filter_value_title = unlist(filters_added_indicator_title, use.names = FALSE)
  )


  filters_data_frame_norm_filter_field_title <- filters_data_frame %>%
    dplyr::distinct(.data[["filter_field_title"]]) %>%
    dplyr::mutate(
      filter_field_title.str_norm = str_norm(.data[["filter_field_title"]])
    )

  filters_data_frame_norm_filter_value_title <- filters_data_frame %>%
    dplyr::distinct(.data[["filter_value_title"]]) %>%
    dplyr::mutate(
      filter_value_title.str_norm = str_norm(.data[["filter_value_title"]])
    )

  filters_data_frame_norm <- filters_data_frame %>%
    dplyr::mutate(
      filter_field_title = str_norm(.data[["filter_field_title"]]),
      filter_value_title = str_norm(.data[["filter_value_title"]])
    )


  data_ids_norm_unique_filters <- dplyr::distinct(
    data_ids_norm,
    .data[["filter_field_id"]],
    .data[["filter_field_title"]]
  )

  data_ids_norm_one_value_only_filters <- data_ids_norm %>%
    dplyr::group_by(.data[["filter_field_id"]]) %>%
    dplyr::filter(dplyr::n() == 1L) %>%
    dplyr::ungroup()

  filters_data_frame_norm_added_filters_ids <- filters_data_frame_norm %>%
    dplyr::left_join(
      data_ids_norm_unique_filters,
      by = c("filter_field_title" = "filter_field_title")
    )

  ## End of preparations

  if (any(is.na(filters_data_frame_norm_added_filters_ids[["filter_field_id"]]))) {
    stop(
      "These filters are named incorrectly or do not exist: ",
      paste(
        unique(
          replace_str_norm_with_original(
            dplyr::filter(
              filters_data_frame_norm_added_filters_ids,
              is.na(.data[["filter_field_id"]])
            ),
            filters_data_frame_norm_filter_field_title
          )[["filter_field_title"]]
        ),
        collapse = ", "
      )
    )
  } else if (nrow(
    dplyr::filter(
      dplyr::setdiff(
        dplyr::select(
          filters_data_frame_norm_added_filters_ids,
          dplyr::all_of(c("filter_field_title", "filter_value_title"))
        ),
        dplyr::select(
          data_ids_norm,
          dplyr::all_of(c("filter_field_title", "filter_value_title"))
        )
      ),
      .data[["filter_value_title"]] != "*"
    )
  ) && !disable_warnings
  ) {
    warning(
      "No matching filter values were found in the data_ids for the following filter values: \n",
      replace_str_norm_with_original(
        replace_str_norm_with_original(
          dplyr::filter(
            dplyr::setdiff(
              dplyr::select(
                filters_data_frame_norm_added_filters_ids,
                dplyr::all_of(c("filter_field_title", "filter_value_title"))
              ),
              dplyr::select(
                data_ids_norm,
                dplyr::all_of(c("filter_field_title", "filter_value_title"))
              )
            ),
            .data[["filter_value_title"]] != "*"
          ),
          filters_data_frame_norm_filter_field_title
        ),
        filters_data_frame_norm_filter_value_title
      ) %>%
        dplyr::group_by(.data[["filter_field_title"]]) %>%
        dplyr::summarise(
          no_match_filter_values = paste0(
            "    ",
            .data[["filter_field_title"]][1],
            ": ",
            paste(.data[["filter_value_title"]],
              collapse = ", "
            )
          ),
          .groups = "drop"
        ) %>%
        dplyr::pull("no_match_filter_values") %>%
        paste(collapse = "\n"),
      "\nThe data will not be filtered by them"
    ) # Should be a error?
  }

  # We remove one possible value only filters from filters argument to avoid
  # possible invalid specification of filter_value_title for these filter_fields_titles from user
  filters_data_frame_norm_added_filters_ids_added_missing_filters <-
    filters_data_frame_norm_added_filters_ids %>%
    dplyr::filter(!(.data[["filter_field_id"]]
    %in% data_ids_norm_one_value_only_filters[["filter_field_id"]])) %>%
    dplyr::mutate(filters_specified_by_user = TRUE) %>%
    rbind.data.frame(
      dplyr::mutate(
        dplyr::anti_join(data_ids_norm_unique_filters,
          .,
          by = c("filter_field_id" = "filter_field_id")
        ),
        filter_value_title = "*",
        filters_specified_by_user = FALSE
      )
    )

  unspecified_filter_fields <-
    (filters_data_frame_norm_added_filters_ids_added_missing_filters[["filter_value_title"]] == "*"
    & !filters_data_frame_norm_added_filters_ids_added_missing_filters[["filters_specified_by_user"]]
    & !(
        filters_data_frame_norm_added_filters_ids_added_missing_filters[["filter_field_id"]]
        %in% data_ids_norm_one_value_only_filters[["filter_field_id"]]
      )
    )

  if (any(unspecified_filter_fields) && !disable_warnings) {
    warning(
      "The following filter fields were not specified in filters: ",
      paste(
        replace_str_norm_with_original(
          filters_data_frame_norm_added_filters_ids_added_missing_filters[
            unspecified_filter_fields,
          ],
          data_ids_norm_filter_field_title
        )[["filter_field_title"]],
        collapse = ", "
      ),
      "\nUsing all possible filter values for these filter fields",
      call. = FALSE
    )
  }

  data_ids_norm_filtered_list <- vector(
    "list",
    length(
      unique(
        filters_data_frame_norm_added_filters_ids_added_missing_filters[["filter_field_id"]]
      )
    )
  ) %>%
    `names<-`(
      unique(
        filters_data_frame_norm_added_filters_ids_added_missing_filters[["filter_field_id"]]
      )
    )


  for (i in names(data_ids_norm_filtered_list)) {
    filters_for_i <- dplyr::filter(
      filters_data_frame_norm_added_filters_ids_added_missing_filters,
      .data[["filter_field_id"]] == i
    )

    data_ids_norm_for_i <- dplyr::filter(
      data_ids_norm,
      .data[["filter_field_id"]] == i
    )

    if (nrow(filters_for_i) == 1 && filters_for_i[["filter_value_title"]][1] == "*") {
      data_ids_norm_filtered_list[[i]] <- data_ids_norm_for_i
    } else {
      data_ids_norm_filtered_list[[i]] <- dplyr::filter(
        data_ids_norm_for_i,
        .data[["filter_value_title"]]
        %in% filters_for_i[["filter_value_title"]]
      )
    }
  }

  empty_data_ids_norm_filtered <- sapply(
    data_ids_norm_filtered_list, function(x) nrow(x) == 0,
    simplify = TRUE
  )

  if (any(empty_data_ids_norm_filtered)) {
    stop(
      "No values were found for filters: ",
      paste(
        replace_str_norm_with_original(
          dplyr::filter(
            data_ids_norm_unique_filters,
            .data[["filter_field_id"]]
            %in% names(data_ids_norm_filtered_list)[empty_data_ids_norm_filtered]
          ),
          data_ids_norm_filter_field_title
        )[["filter_field_title"]],
        collapse = ", "
      )
    )
  }

  data_ids_norm_filtered_data_frame <- do.call(
    rbind,
    c(data_ids_norm_filtered_list, make.row.names = FALSE)
  ) %>%
    replace_str_norm_with_original(data_ids_norm_filter_field_title) %>%
    replace_str_norm_with_original(data_ids_norm_filter_value_title)


  return(data_ids_norm_filtered_data_frame[original_data_ids_columns_order])
}
