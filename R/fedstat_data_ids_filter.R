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
#' @examples
#' \dontrun{
#' # Get data filters identificators for CPI
#' # filter the data_ids to get data for january of 2023
#' # for all goods and services for Russian Federation
#' data_ids_filtered <- fedstat_get_data_ids("31074") %>%
#'   fedstat_data_ids_filter(
#'     filters = list(
#'       "Territory" = "Russian Federation",
#'       "Year" = "2023",
#'       "Period" = "January",
#'       "Types of goods and services" = "*"
#'     )
#'   )
#'
#' # Not actual filter field titles and filter values titles because of ASCII requirement for CRAN
#' }
fedstat_data_ids_filter <- function(data_ids, filters = list(), disable_warnings = FALSE) {

  # workaround for `:=` and CMD check
  filter_field_title.str_norm <- filter_field_title <- filter_value_title.str_norm <- filter_value_title <- filter_field_id <-
    count <- filters_specified_by_user <- NULL

  original_columns_order <- names(data_ids)

  str_norm <- function(x) tolower(gsub("\\s+", " ", trimws(x)))

  if (is.character(names(filters))) {
    names(filters) <- enc2utf8(names(filters))
  }
  filters <- lapply(filters, enc2utf8) # encoding should be UTF-8

  data_ids <- data.table::as.data.table(data_ids) %>%
    data.table::copy()

  data_ids_norm <- data_ids %>%
    .[
      ,
      c("filter_field_title.str_norm", "filter_value_title.str_norm") := .(
        str_norm(filter_field_title),
        str_norm(filter_value_title)
      )
    ]

  indicator_title <- data_ids[
    data_ids[["filter_field_id"]] == "0",
    filter_value_title
  ]

  filters_added_indicator_title <- c(filters, list("Pokazatel" = indicator_title)) %>%
    `names<-`(c(
      names(filters),
      "\u041f\u043e\u043a\u0430\u0437\u0430\u0442\u0435\u043b\u044c"
    )) # Pokazatel' in Russian written in UTF-8 escapes for CRAN

  filters_data_frame <- data.table::data.table(
    filter_field_title = rep(
      names(filters_added_indicator_title),
      sapply(filters_added_indicator_title, length, simplify = TRUE)
    ),
    filter_value_title = unlist(filters_added_indicator_title, use.names = FALSE),
    stringsAsFactors = FALSE
  )

  filters_data_frame_norm <- filters_data_frame %>%
    .[
      ,
      c("filter_field_title.str_norm", "filter_value_title.str_norm") := .(
        str_norm(filter_field_title),
        str_norm(filter_value_title)
      )
    ]

  data_ids_norm_unique_filters <- unique(
    data_ids_norm,
    by = c("filter_field_id", "filter_field_title", "filter_field_title.str_norm")
  ) %>%
    .[, .(filter_field_id, filter_field_title, filter_field_title.str_norm)]

  data_ids_norm_one_value_only_filters <- data_ids_norm %>%
    .[, count := .N, by = "filter_field_id"] %>%
    .[count == 1, ] %>%
    .[, count := NULL]

  filters_data_frame_norm_added_filters_ids <- filters_data_frame_norm %>%
    data.table::merge.data.table(
      data_ids_norm_unique_filters[, .(filter_field_title.str_norm, filter_field_id)],
      by = "filter_field_title.str_norm",
      all.x = TRUE
    )

  no_match_filter_values <- anti_join(
    filters_data_frame_norm_added_filters_ids,
    data_ids_norm,
    by = c("filter_field_title.str_norm", "filter_value_title.str_norm")
  ) %>%
    .[filter_value_title.str_norm != "*", ]

  if (any(is.na(filters_data_frame_norm_added_filters_ids[["filter_field_id"]]))) {
    stop(
      "These filters are named incorrectly or do not exist: ",
      filters_data_frame_norm_added_filters_ids[
        is.na(filter_field_id), paste(unique(filter_field_title), collapse = ", ")
      ]
    )
  } else if (nrow(no_match_filter_values) && !disable_warnings) {
    warning(
      "No matching filter values were found in the data source for the following filter values: \n",
      semi_join(
        filters_data_frame_norm_added_filters_ids,
        no_match_filter_values,
        by = c("filter_field_title.str_norm", "filter_value_title.str_norm")
      ) %>%
        .[
          ,
          paste0(
            "    ",
            .BY[[1]],
            ": ",
            paste(.SD[["filter_value_title"]],
              collapse = ", "
            )
          ),
          by = "filter_field_title"
        ] %>%
        .[["V1"]] %>%
        paste(collapse = "\n"),
      "\nThe data will not be filtered by them"
    )
  }

  # We remove one possible value only filters from filters argument to avoid
  # possible invalid specification of filter_value_title for these filter_fields_titles from user
  filters_data_frame_norm_added_filters_ids_added_missing_filters <-
    anti_join(
      filters_data_frame_norm_added_filters_ids,
      data_ids_norm_one_value_only_filters,
      by = "filter_field_id"
    ) %>%
    .[, filters_specified_by_user := TRUE] %>%
    rbind(
      anti_join(data_ids_norm_unique_filters, ., by = "filter_field_id")[
        ,
        c("filter_value_title", "filter_value_title.str_norm", "filters_specified_by_user") := .(
          "*", "*", FALSE
        )
      ],
      use.names = TRUE
    )

  unspecified_filter_fields <-
    (filters_data_frame_norm_added_filters_ids_added_missing_filters[["filter_value_title"]] == "*" &
      !filters_data_frame_norm_added_filters_ids_added_missing_filters[["filters_specified_by_user"]] &
      !(
        filters_data_frame_norm_added_filters_ids_added_missing_filters[["filter_field_id"]]
        %in% data_ids_norm_one_value_only_filters[["filter_field_id"]]
      )
    )

  if (any(unspecified_filter_fields) && !disable_warnings) {
    warning(
      "The following filter fields were not specified in filters: ",
      paste(
        filters_data_frame_norm_added_filters_ids_added_missing_filters[["filter_field_title"]][
          unspecified_filter_fields
        ],
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
    filters_for_i <- filters_data_frame_norm_added_filters_ids_added_missing_filters[
      filter_field_id == i,
    ]

    data_ids_norm_for_i <- data_ids_norm[filter_field_id == i, ]

    if (any(filters_for_i[["filter_value_title"]] == "*") && nrow(filters_for_i) != 1) {
      stop(
        "Special value for ",
        filters_data_frame_norm_added_filters_ids[
          filter_field_id == i, unique(filter_field_title)
        ],
        " field is in a filter vector of length greater than 1"
      )
    }

    if (filters_for_i[["filter_value_title"]][1] == "*") {
      data_ids_norm_filtered_list[[i]] <- data_ids_norm_for_i
    } else {
      data_ids_norm_filtered_list[[i]] <- data_ids_norm_for_i[
        filter_value_title.str_norm %in% filters_for_i[["filter_value_title.str_norm"]],
      ]
    }
  }

  empty_data_ids_norm_filtered <- sapply(
    data_ids_norm_filtered_list, function(x) nrow(x) == 0,
    simplify = TRUE
  )

  if (any(empty_data_ids_norm_filtered)) {
    stop(
      "No filter values were found for filters fields: ",
      filters_data_frame_norm_added_filters_ids[
        filter_field_id %in% names(data_ids_norm_filtered_list)[empty_data_ids_norm_filtered],
        paste(unique(filter_field_title), collapse = ", ")
      ]
    )
  }

  data_ids_norm_filtered_data_frame <- data.table::rbindlist(
    data_ids_norm_filtered_list
  ) %>%
    as.data.frame()


  return(data_ids_norm_filtered_data_frame[, original_columns_order])
}
