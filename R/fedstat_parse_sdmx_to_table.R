#' Parse sdmx raw bytes to data.frame
#'
#' @description Parses sdmx raw bytes received in response to POST request.
#' This function is a wrapper around `readsdmx::read_sdmx`,
#' in addition to reading data, automatically adds columns with values from lookup tables.
#' Can also return full data codes dictionary for the indicator
#'
#' @param data_raw sdmx raw bytes
#' @param return_type character, "data" or "dicionary", data for actual data,
#' dictionary for sdmx lookup table (full data codes dictionary)
#' @param try_to_parse_ObsValue logical, try to parse ObsValue column from character to R numeric type
#'
#' @return data.frame
#' @export
#'
#' @seealso \code{\link{fedstat_parse_sdmx_to_table}}
#'
#' @examples
#' \dontrun{
#' # Get data filters identificators for CPI
#' # filter the data_ids to get data for january of 2023
#' # for all goods and services for Russian Federation
#' # POST filters and download data in sdmx format
#' # Parse raw sdmx to data.frame
#' data <- fedstat_get_data_ids("31074") %>%
#'   fedstat_data_ids_filter(
#'     filters = list(
#'       "Territory" = "Russian Federation",
#'       "Year" = "2023",
#'       "Period" = "January",
#'       "Types of goods and services" = "*"
#'     )
#'   ) %>%
#'   fedstat_post_data_ids_filtered() %>%
#'   fedstat_parse_sdmx_to_table()
#'
#' # Not actual filter field titles and filter values titles because of ASCII requirement for CRAN
#' }
fedstat_parse_sdmx_to_table <- function(data_raw, return_type = c("data", "dictionary"), try_to_parse_ObsValue = TRUE) {
  # workaround for `:=` and CMD check
  field_id <- ObsValue <- NULL

  if (is.null(data_raw) || methods::is(data_raw, "character")) {
    warning("data_raw is not raw (binary) type")
    return(NULL)
  }

  return_type <- match.arg(return_type, return_type)

  tmp_file <- tempfile()
  writeLines(rawToChar(data_raw), tmp_file)

  xml <- xml2::read_xml(tmp_file)
  data <- readsdmx::read_sdmx(tmp_file) %>% data.table::as.data.table()

  names(data) <- iconv(names(data), "UTF-8", "UTF-8") # repair cyrillic symbols encoding
  names(data) <- sub(x = names(data), "X(\\d+)\\.", "\\1-") # fix readsdmx renaming like "X30.ОКАТО" -> "30-ОКАТО"

  if (file.exists(tmp_file)) file.remove(tmp_file)

  CodeList <- xml2::xml_find_all(xml, "/d1:GenericData/d1:CodeLists/structure:CodeList")

  codelist_id <- CodeList %>%
    xml2::xml_attr("id")

  codelist_title <- CodeList %>%
    xml2::xml_find_all("structure:Name") %>%
    xml2::xml_text()

  codelist_tbl <- mapply(
    CodeList = CodeList, title = codelist_title, id = codelist_id, SIMPLIFY = FALSE,
    function(CodeList, title, id) {
      chldrn <- xml2::xml_find_all(CodeList, "structure:Code")

      data.table::data.table(
        field_id = id,
        field_title = title,
        value_id = xml2::xml_attr(chldrn, "value"),
        value_title = xml2::xml_text(chldrn)
      )
    }
  ) %>% data.table::rbindlist()

  if (any(stats::complete.cases(codelist_tbl) == FALSE)) {
    stop("NA in lookup sdmx table")
  }

  if (return_type == "dictionary") {
    return(codelist_tbl)
  }

  field_ids <- codelist_tbl[["field_id"]] %>%
    unique()

  data_res <- lapply(field_ids, function(x) {
    codelist_tbl[field_id == x][
      ,
      c("value_title", "value_id")
    ][data[, x, with = FALSE], on = c(value_id = x)][["value_title"]]
  }) %>%
    `names<-`(field_ids) %>%
    data.table::as.data.table() %>%
    cbind(
      data %>%
        data.table::setnames(old = field_ids, new = paste0(field_ids, "_code"))
    )

  if (try_to_parse_ObsValue) {
    ObsValue_already_NA <- data_res[is.na(ObsValue) | trimws(ObsValue) == "", which = TRUE]
    data_res[, ObsValue := suppressWarnings(as.numeric(gsub(",", ".", ObsValue)))]
    if (any(is.na(data_res$ObsValues[-ObsValue_already_NA]))) {
      stop(
        "Unable to parse ObsValue from character to numeric type automatically, set 'try_to_parse_ObsValue' to FALSE"
      )
    }
  }

  return(as.data.frame(data_res))
}
