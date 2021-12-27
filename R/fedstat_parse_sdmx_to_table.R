#' Parse sdmx raw bytes to data.frame
#'
#' @description Parses sdmx raw bytes received in response to POST request.
#' This function is a wrapper around `readsdmx::read_sdmx`,
#' in addition to reading data, automatically adds columns with values from lookup tables
#'
#' @param data_raw sdmx raw bytes
#'
#' @return data.frame
#' @export
#'
#' @seealso \code{\link{fedstat_parse_sdmx_to_table}}
#'
#' @importFrom rlang .data
#'
#' @examples
#' \dontrun{
#' # Get data filters identificators for week prices
#' # standardize names for DVFO and extract week numbers
#' # filter the data_ids to get data for week 21 and 22 of 2021
#' # for all goods and services for Russian Federation
#' # POST filters and download data in sdmx format
#' # Parse raw sdmx to data.frame
#' data <- fedstat_get_data_ids("37426") %>%
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
#'   ) %>%
#'   fedstat_post_data_ids_filtered() %>%
#'   fedstat_parse_sdmx_to_table()
#'
#' # In this example names for DVFO are latinized for CRAN
#' # Not actual filter field titles and filter values titles because of ASCII requirement for CRAN
#' }
fedstat_parse_sdmx_to_table <- function(data_raw) {
  data_sdmx <- rawToChar(data_raw)

  tmp_file <- tempfile()
  writeLines(data_sdmx, tmp_file)

  data_sdmx_parsed <- readsdmx::read_sdmx(tmp_file) # much faster read than rsdmx

  xml_parsed <- xml2::read_xml(tmp_file)

  codelist_field_id <- xml2::xml_attr(
    xml2::xml_find_all(xml_parsed, "/*[name()='GenericData']/*[name()='CodeLists']/*[name()='structure:CodeList']"),
    "id"
  )
  codelist_field_title <- xml2::xml_text(
    xml2::xml_find_all(xml_parsed, "/*[name()='GenericData']/*[name()='CodeLists']/*[name()='structure:CodeList']/*[name()='structure:Name']")
  )

  codelist_parsed <- vector("list", length(codelist_field_id))

  for (i in seq_len(length(codelist_field_id))) {
    xml_values <- xml2::xml_find_all(
      xml_parsed,
      paste0(
        "/*[name()='GenericData']/*[name()='CodeLists']/*[name()='structure:CodeList' and @id='",
        codelist_field_id[i], "']/*[name()='structure:Code']"
      )
    )

    codelist_parsed[[i]] <- data.frame(
      field_id = codelist_field_id[i],
      field_title = codelist_field_title[i],
      value_id = xml2::xml_attr(xml_values, "value"),
      value_title = xml2::xml_text(xml_values),
      stringsAsFactors = FALSE
    )
  }

  codelist_parsed_bind <- dplyr::bind_rows(codelist_parsed)

  if (
    any(sapply(
      codelist_parsed_bind,
      function(x) any(is.na(x)),
      simplify = TRUE
    ))
  ) {
    stop("NA in lookup sdmx table")
  }

  if (file.exists(tmp_file)) file.remove(tmp_file)

  data_sdmx_reference_codes <- unique(codelist_parsed_bind[["field_id"]])

  data_sdmx_parsed_joined <- data_sdmx_parsed

  for (i in data_sdmx_reference_codes) {
    by_codes <- c("value_id") %>%
      `names<-`(i)

    codelist_parsed_bind_code <- codelist_parsed_bind[
      codelist_parsed_bind[["field_id"]] == i,
    ]

    data_sdmx_parsed_joined <- dplyr::left_join(
      data_sdmx_parsed_joined,
      dplyr::select(
        codelist_parsed_bind_code,
        dplyr::all_of(c("value_id", "value_title"))
      ),
      by = by_codes
    ) %>%
      dplyr::rename_with(function(x) {
        x_renamed <- x
        code_index <- x == i
        label_index <- x == "value_title"
        x[code_index] <- paste0(i, "_code")
        x[label_index] <- i
        return(x)
      })
  }

  return(data_sdmx_parsed_joined)
}
