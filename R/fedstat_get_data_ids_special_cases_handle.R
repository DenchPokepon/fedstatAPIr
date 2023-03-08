#' Handle special cases strings for `filter_value_title` col in `data_ids`
#'
#' @description Handles special cases strings in `filter_value_title` to standardize them.
#' Currently does only 2 things:
#' 1. Replaces `filter_value_title` with given aliases in `filter_value_title_alias_lookup_table`;
#' 2. Replaces week period titles with week numbers to filter
#'    by it instead of a complex unstandardized "period" string
#'    and adds a new column `filter_value_title_week` for weekly data
#'    for using original week title if needed;
#'
#' Function will be supplemented with new methods for processing special cases as they are found
#'
#' @param data_ids data.frame, result of `fedstat_get_data_ids`
#' @param filter_value_title_alias_lookup_table data.frame with columns `filter_value_title` and `filter_value_title_alias`.
#'   Used to replace `filter_value_title` with standard forms of filter value titles.
#'   It is mainly used to set consistent names.
#'   For example, the Dalnevostochnyj federalnyj okrug ( s 03.11.2018) (transliteration for CRAN) in `filter_value_title`
#'   can be simply replaced with `filter_value_title_alias` as Dalnevostochnyj federalnyj okrug.
#'   In this example in fact, these are two different entities (after the inclusion of Buryatia and Transbaikalia and before),
#'   but in most cases such a replacement is more convenient for loading data, since you need the whole time series.
#'   By default it's empty data.frame, e.g. no replacement for anything.
#'
#' @return data.frame, data_ids with replaced by aliases `filter_value_title` column
#' and a new column `filter_value_title_week`, which consists only of NA in case of non-weekly data
#'
#' @seealso \code{\link{fedstat_get_data_ids}}
#'
#' @export
#'
#' @importFrom rlang .data
#'
#' @examples
#' \dontrun{
#' # Get data filters identificators for week prices
#' # and standardize names for DVFO and extract week numbers
#' data_ids_special_cases_handled <- fedstat_get_data_ids("37426") %>%
#'   fedstat_get_data_ids_special_cases_handle(
#'     filter_value_title_alias_lookup_table = data.frame(
#'       "filter_value_title" = "Dalnevostochnyj federalnyj okrug ( s 03.11.2018)",
#'       "filter_value_title_alias" = "Dalnevostochnyj federalnyj okrug"
#'     )
#'   )
#' # In this example names for Far Eastern Federal District are latinized for CRAN
#' }
fedstat_get_data_ids_special_cases_handle <- function(data_ids,
                                                      filter_value_title_alias_lookup_table = data.frame(
                                                        "filter_value_title" = character(),
                                                        "filter_value_title_alias" = character(),
                                                        stringsAsFactors = FALSE
                                                      )) {
  data_ids_special_cases_handled <- data_ids %>%
    dplyr::left_join(filter_value_title_alias_lookup_table,
      by = c("filter_value_title" = "filter_value_title")
    ) %>%
    dplyr::mutate(
      filter_value_title = dplyr::coalesce(.data[["filter_value_title_alias"]], .data[["filter_value_title"]])
    ) %>%
    dplyr::select(!"filter_value_title_alias")


  if ("33560" %in% data_ids_special_cases_handled[["filter_field_id"]]) {
    grepl_regex_string <- "^\\d+(-\u0430\u044f)? .*\u0433\u043e\u0434\u0430(\\s+)?\\)"
    # "^\\d+(-aia)? .*goda(\\s+)?\\)" in UTF-8 escapes

    extract_regex_string <- "\u005e\u005c\u0064\u002b\u0028\u002d\u0430\u044f\u0029\u003f\u0020"
    # "^\\d+(-aia)? " in UTF-8 escapes

    data_ids_special_cases_handled <- data_ids_special_cases_handled %>%
      dplyr::mutate(
        filter_value_title_week = dplyr::if_else(
          .data[["filter_field_id"]] == "33560" &
            grepl(grepl_regex_string, .data[["filter_value_title"]]),
          .data[["filter_value_title"]],
          NA_character_
        ),
        filter_value_title = dplyr::if_else(
          .data[["filter_field_id"]] == "33560" &
            grepl(grepl_regex_string, .data[["filter_value_title"]]),
          stringr::str_remove( # remove leading zeroes
            stringr::str_extract(
              stringr::str_extract(
                .data[["filter_value_title"]],
                extract_regex_string
              ),
              "\\d+"
            ),
            "^0+"
          ),
          .data[["filter_value_title"]]
        )
      )
  }

  return(data_ids_special_cases_handled)
}