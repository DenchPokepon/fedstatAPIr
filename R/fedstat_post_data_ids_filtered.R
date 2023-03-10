#' Post data filters ids to fedstat.ru and download specified subset of data
#'
#' @description Creates a request body from `data_ids` and sends it to
#'   https://www.fedstat.ru/indicator/data.do?format={`data_format`}.
#'   Gets an sdmx or excel with data in binary format.
#'
#'   sdmx raw bytes can be passed to `fedstat_parse_sdmx_to_table` to create a
#'   data.frame or to `rawToChar` and `writeLines` to create an xml file
#'
#'   excel raw bytes can be passed to `writeBin` to create an xls file
#'
#' @param data_ids data.frame, can be a result of `fedstat_get_data_ids` or
#'   `fedstat_get_data_ids_special_cases_handle` to download all available data,
#'   or a result of `fedstat_data_ids_filter` to download subset of available data
#' @param ... other arguments passed to httr::POST
#' @param data_format string, one of sdmx, excel
#' @param timeout_seconds numeric, maximum time before a new POST request is tried
#' @param retry_max_times numeric, maximum number of tries to POST `data_ids`
#' @param httr_verbose `httr::verbose()` or NULL, outputs messages to the console
#' about the processing of the request
#'
#' @return raw bytes (sdmx or excel)
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
#' data <- fedstat_get_data_ids("31074") %>%
#'   fedstat_data_ids_filter(
#'     filters = list(
#'       "Territory" = "Russian Federation",
#'       "Year" = "2023",
#'       "Period" = "January",
#'       "Types of goods and services" = "*"
#'     )
#'   ) %>%
#'   fedstat_post_data_ids_filtered()
#'
#' # Not actual filter field titles and filter values titles because of ASCII requirement for CRAN
#' }
fedstat_post_data_ids_filtered <- function(data_ids,
                                           ...,
                                           data_format = c("sdmx", "excel"),
                                           timeout_seconds = 180,
                                           retry_max_times = 3,
                                           httr_verbose = NULL) {

  # workaround for `:=` and CMD check
  filter_field_id <- filter_value_id <- NULL

  data_ids <- data.table::as.data.table(data_ids)

  data_format <- match.arg(data_format, data_format)
  POST_URL <- paste0("https://www.fedstat.ru/indicator/data.do?format=", data_format)

  indicator <- data_ids[filter_field_id == "0", c("filter_value_id", "filter_value_title")]

  filters <- unique(data_ids, by = "filter_field_id")[, c("filter_field_id", "filter_field_object_ids")]

  filters_list <- as.list(filters[["filter_field_id"]]) %>%
    `names<-`(filters[["filter_field_object_ids"]])

  filter_values <- data_ids[, .(filter_string = paste0(filter_field_id, "_", filter_value_id))][["filter_string"]] %>%
    as.list()

  names(filter_values) <- (rep("selectedFilterIds", length(filter_values)))

  POST_body <- c(
    list(
      "format" = data_format,
      "id" = indicator[["filter_value_id"]],
      "indicator_title" = indicator[["filter_value_title"]]
    ),
    filters_list,
    filter_values
  )

  POST_res <- httr::RETRY("POST", POST_URL, httr_verbose, httr::timeout(timeout_seconds),
    times = retry_max_times, body = POST_body, ...
  )

  if (httr::http_error(POST_res)) {
    httr::http_condition(POST_res, type = "error")
  } else
  if (!(POST_res[["headers"]][["content-type"]] %in% c("text/xml", "application/vnd.ms-excel"))) {
    stop("No data found with specified filters or the fedstat is lagging")
  }

  return(POST_res[["content"]])
}
