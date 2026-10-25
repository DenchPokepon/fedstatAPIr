#' Parse sdmx raw bytes to data.frame
#'
#' @description Parses sdmx raw bytes received in response to POST request.
#' This function is a wrapper around `readsdmx::read_sdmx`,
#' in addition to reading data, automatically adds columns with values from lookup tables.
#' Can also return full data codes dictionary for the indicator
#'
#' @param data_raw sdmx raw bytes or a path to an existing SDMX XML file
#' @param return_type character, "data" or "dictionary", data for actual data,
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
  fedstat_parse_sdmx(data_raw, return_type, try_to_parse_ObsValue)
}

fedstat_parse_sdmx <- function(data_raw, return_type = c("data", "dictionary"),
                               try_to_parse_ObsValue = TRUE, indicator_id = NULL) {
  # workaround for `:=` and CMD check
  field_id <- ObsValue <- NULL

  if (!is.raw(data_raw) && !(is.character(data_raw) && length(data_raw) == 1L && file.exists(data_raw))) {
    warning("data_raw must be raw bytes or an existing SDMX file path")
    return(NULL)
  }
  return_type <- match.arg(return_type)
  # readsdmx needs a file; avoid copying the whole export into an R string.
  tmp_file <- data_raw
  if (is.raw(data_raw)) {
    tmp_file <- tempfile(fileext = ".xml")
    on.exit(unlink(tmp_file), add = TRUE)
    writeBin(data_raw, tmp_file)
  }
  xml <- tryCatch(xml2::read_xml(tmp_file), error = function(e) {
    fedstat_error("Invalid or incomplete SDMX XML response.", "parse", "parse")
  })
  if (xml2::xml_name(xml) != "GenericData" ||
    !length(xml2::xml_find_all(xml, "./*[local-name()='DataSet']"))) {
    fedstat_error("Expected an SDMX GenericData document with a DataSet.", "parse", "parse")
  }
  if (!is.null(indicator_id)) {
    source <- xml2::xml_attr(xml2::xml_find_first(xml, "//*[local-name()='Indicator']"), "id")
    if (is.na(source) || source != indicator_id) {
      fedstat_error("Export does not identify the requested indicator.", "parse", "parse")
    }
  }
  truncated <- xml2::xml_text(xml2::xml_find_all(xml, "./*[local-name()='Header']/*[local-name()='Truncated']"))
  if (any(tolower(trimws(truncated)) %in% c("true", "1"))) {
    fedstat_error(
      "Fedstat marked this export as truncated; request a smaller selection.",
      "truncated", "export"
    )
  }
  CodeList <- xml2::xml_find_all(xml, "/*[local-name()='GenericData']/*[local-name()='CodeLists']/*[local-name()='CodeList']")

  codelist_id <- CodeList %>%
    xml2::xml_attr("id")

  codelist_title <- CodeList %>%
    xml2::xml_find_first("./*[local-name()='Name']") %>%
    xml2::xml_text()

  codelist_tbl <- mapply(
    CodeList = CodeList, title = codelist_title, id = codelist_id, SIMPLIFY = FALSE,
    function(CodeList, title, id) {
      chldrn <- xml2::xml_find_all(CodeList, "./*[local-name()='Code']")

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

  if (anyDuplicated(codelist_tbl[, c("field_id", "value_id")])) {
    fedstat_error("Duplicate codes in the SDMX dictionary.", "parse", "parse")
  }
  if (return_type == "dictionary") {
    return(codelist_tbl)
  }

  namespaces <- xml2::xml_ns(xml)
  generic <- unname(namespaces[grepl("/generic$", namespaces)])
  observations <- if (length(generic) == 1L) {
    xml2::xml_find_num(xml, "count(//g:Obs)", ns = c(g = generic))
  } else {
    xml2::xml_find_num(xml, "count(//*[local-name()='Obs'])")
  }
  rm(xml, CodeList)
  data <- if (observations) {
    data.table::as.data.table(readsdmx::read_sdmx(tmp_file))
  } else {
    data.table::as.data.table(setNames(
      rep(list(character()), length(unique(codelist_tbl$field_id)) + 1L),
      c(unique(codelist_tbl$field_id), "ObsValue")
    ))
  }
  if (nrow(data) != observations) fedstat_error("SDMX parser lost observations.", "parse", "parse")
  names(data) <- iconv(names(data), "UTF-8", "UTF-8")
  names(data) <- sub("^X(\\d+)\\.", "\\1-", names(data))
  names(data) <- sub("^X(\\d)", "\\1", names(data))

  field_ids <- codelist_tbl[["field_id"]] %>%
    unique()

  if (length(setdiff(field_ids, names(data))) != 0L) {
    stop(
      "Expected columns not found in data: ",
      setdiff(field_ids, names(data)), "\n",
      "there are only these columns in the data: ", paste(names(data), collapse = ", "),
      "\nIt's likely to be an encoding or parsing error, please report this issue on github repository of this package"
    )
  }

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
    raw <- data_res[["ObsValue"]]
    missing <- is.na(raw) | trimws(raw) == ""
    parsed <- suppressWarnings(as.numeric(gsub(",", ".", raw, fixed = TRUE)))
    if (any(!missing & is.na(parsed))) {
      stop("Unable to parse ObsValue as numeric; set try_to_parse_ObsValue = FALSE to preserve source values.",
        call. = FALSE
      )
    }
    data_res[, ObsValue := parsed]
  }

  return(as.data.frame(data_res))
}
