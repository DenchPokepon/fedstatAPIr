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
  # workaround for `:=` and CMD check
  field_id = NULL

  if(is.null(data_raw) | methods::is(data_raw, "character")) return(NULL)

  tmp_file <- tempfile()
  writeLines(rawToChar(data_raw), tmp_file)

  xml <- xml2::read_xml(tmp_file)
  data <- readsdmx::read_sdmx(tmp_file) %>% as.data.table()

  names(data) <- sub(x = names(data), "X(\\d+)\\.", "\\1-") # fix readsdmx renaming like "X30.ОКАТО" -> "30-ОКАТО"

  if(file.exists(tmp_file))file.remove(tmp_file)

  CodeList <- xml2::xml_find_all(xml, '/d1:GenericData/d1:CodeLists/structure:CodeList')

  codelist_id <- CodeList  %>%
    xml2::xml_attr("id")

  codelist_title <- CodeList %>%
    xml2::xml_find_all("structure:Name") %>%
    xml2::xml_text()

  codelist_tbl <- mapply(CodeList = CodeList, title = codelist_title, id = codelist_id, SIMPLIFY = FALSE,
                         function(CodeList, title, id){

                           chldrn <- xml2::xml_find_all(CodeList, "structure:Code")

                           data.table(field_id = id,
                                      field_title = title,
                                      value_id = xml2::xml_attr(chldrn, "value"),
                                      value_title = xml2::xml_text(chldrn))
                         }) %>% rbindlist()

  if(any(stats::complete.cases(codelist_tbl) == FALSE)){stop("NA in lookup sdmx table")}

  field_ids <- codelist_tbl[["field_id"]] %>%
    unique()

  lapply(field_ids, function(x){
    codelist_tbl[field_id == x][, c("value_title", "value_id")][data[, x, with = FALSE], on = c(value_id = x)][["value_title"]]}) %>%
    `names<-`(paste0(field_ids, "_title")) %>%
    as.data.table() %>%
    cbind(data)
}
