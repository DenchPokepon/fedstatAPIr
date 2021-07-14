#' Parse sdmx raw bytes to data.frame
#'
#' @description Parses sdmx raw bytes received in response to POST request.
#' This function is a wrapper around `readsdmx::read_sdmx` and `rsdmx::readSDMX`,
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
  data_sdmx_dsd <- rsdmx::readSDMX(tmp_file, isURL = FALSE)@dsd

  if (file.exists(tmp_file)) file.remove(tmp_file)

  data_sdmx_reference_codes <- sapply(
    methods::slot(
      methods::slot(data_sdmx_dsd, "codelists"),
      "codelists"
    ), function(x) methods::slot(x, "id")
  )

  data_sdmx_parsed_joined <- data_sdmx_parsed

  for (i in data_sdmx_reference_codes) {
    by_codes <- c("id") %>%
      `names<-`(i)

    data_sdmx_parsed_joined <- dplyr::left_join(
      data_sdmx_parsed_joined,
      dplyr::select(
        as.data.frame(methods::slot(data_sdmx_dsd, "codelists"), codelistId = i),
        !"description.ru"
      ),
      by = by_codes
    ) %>%
      dplyr::rename_with(function(x) {
        x_renamed <- x
        code_index <- x == i
        label_index <- x == "label.ru"
        x[code_index] <- paste0(i, "_code")
        x[label_index] <- i
        return(x)
      })
  }

  return(data_sdmx_parsed_joined)
}
