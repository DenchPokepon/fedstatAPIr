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
#' @importFrom rlang .data
#'
#' @examples
#' \dontrun{
#' # Get data filters identificators for week prices
#' # standardize names for DVFO and extract week numbers
#' # filter the data_ids to get data for week 21 and 22 of 2021
#' # for all goods and services for Russian Federation
#' # POST filters and download data in sdmx format
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
#'   fedstat_post_data_ids_filtered()
#'
#' # In this example names for DVFO are latinized for CRAN
#' # Not actual filter field titles and filter values titles because of ASCII requirement for CRAN
#' }
fedstat_post_data_ids_filtered <- function(data_ids,
                                           ...,
                                           data_format = c("sdmx", "excel"),
                                           timeout_seconds = 180,
                                           retry_max_times = 3,
                                           httr_verbose = httr::verbose(data_out = FALSE)
                                           ) {
  data_format <- match.arg(data_format, data_format)
  POST_URL <- paste0(FEDSTAT_URL_BASE, "/indicator/data.do?format=", data_format)
  filter_field <- "selectedFilterIds"

  indicator_id_and_title <- data_ids[
    data_ids[["filter_field_id"]] == "0",
    c("filter_value_id", "filter_value_title"),
    drop = TRUE
  ]

  indicator_id <- indicator_id_and_title[["filter_value_id"]]
  indicator_title <- indicator_id_and_title[["filter_value_title"]]

  data_ids_unique_filters <- dplyr::distinct(data_ids,
    .data[["filter_field_id"]],
    .keep_all = TRUE
  ) %>%
    dplyr::arrange(.data[["filter_field_object_ids_order"]])

  object_ids_filters <- as.list(data_ids_unique_filters[["filter_field_id"]]) %>%
    `names<-`(data_ids_unique_filters[["filter_field_object_ids"]])

  data_ids_filtered_POST_body <- data_ids %>%
    dplyr::summarise(
      filter_string = paste0(.data[["filter_field_id"]], "_", .data[["filter_value_id"]])
    ) %>%
    dplyr::pull("filter_string") %>%
    as.list() %>%
    `names<-`(rep(filter_field, length(.)))


  POST_body <- c(
    list(
      "format" = data_format,
      "id" = indicator_id,
      "indicator_title" = indicator_title
    ),
    object_ids_filters,
    data_ids_filtered_POST_body
  )

  POST_res <- httr::RETRY(
    "POST",
    POST_URL,
    httr_verbose,
    httr::timeout(timeout_seconds),
    times = retry_max_times,
    body = POST_body,
    ... = ...
  )

  if (httr::http_error(POST_res)) {
    httr::http_condition(POST_res, type = "error")
  } else if (!(POST_res[["headers"]][["content-type"]]
  %in% c("text/xml", "application/vnd.ms-excel"))) {
    stop(
      "No data found with specified filters"
    )
  }

  return(POST_res[["content"]])
}
