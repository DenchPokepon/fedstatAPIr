#' Get data filters ids from fedstat.ru indicator web page
#'
#' @description
#' To query data from fedstat we need to POST some filters in form of filter numeric identificators.
#' Most filters don't have some rule from which their ids can be generated based on
#' filters titles and values. It seems like these ids are just indexes in the fedstat inner database.
#' So in order to get the data, we first need to get the ids of the filter values by
#' parsing specific part of java script source code on indicator web page.
#'
#' @details
#' It is known that the fedstat lags quite often. Sometimes site never responds at all.
#' This is especially true for the most popular indicators web pages.
#' In this regard, by default, a GET request is sent 3 times with a timeout
#' of 180 seconds and with initially small, but growing exponentially, pauses between requests.
#'
#' As a rule, requests to the indicator web page take much longer than requests
#' to get the data itself. A POST request for data is sent to a single
#' URL https://www.fedstat.ru/indicator/data.do?format=(excel or sdmx)
#' for all indicators and is often quite fast. In this regard, for many indicators,
#' it makes sense to cache `data_ids` to increase the speed of data download.
#' This is not possible for all data, for example, for weekly prices,
#' each new week adds a new filter (new week), the id of which can only be found on the indicator web page.
#' But for most data (e.g. monthly frequency), time filters are trivial.
#' There are 12 months in total with unique ids that do not change
#' and year ids that match their values
#' (that is, `filter_value_id` = `filter_value`, in other words 2020 = 2020)
#'
#' Correct filter_field_object_ids are needed to get data.
#' For the sdmx format, these ids do not change anything,
#' except for the standard data sorting,
#' but their incorrect specification will lead either to incomplete data loading or to no data at all.
#' For the excel format, these ids determine the form of data presentation, as in the data preview on the fedstat site.
#' For now only default filter_field_object_ids are used, which are parsed from java script source code on indicator web page.
#' In theory, it is possible to let the user specify filter_field_object_ids himself,
#' but this will add unnecessary complexity and room for errors on the user side.
#'
#' @param indicator_id character, indicator id/code from indicator URL.
#'   For example for indicator with URL https://www.fedstat.ru/indicator/37426 indicator id will be 37426
#' @param ... other arguments passed to httr::GET
#' @param timeout_seconds numeric, maximum time before a new GET request is tried
#' @param retry_max_times numeric, maximum number of tries to GET `data_ids`
#' @param httr_verbose `httr::verbose()` or NULL, outputs messages to the console
#' about the processing of the request
#'
#' @return data.frame with all character type columns:
#'   1. filter_field_id - id for filter field;
#'   2. filter_field_title - filter field title string representation;
#'   3. filter_value_id - id for filter field value;
#'   4. filter_value_title - filter field value title string representation;
#'   5. filter_field_object_ids - special strings that define the location of the filters fields.
#'   It can take the following values: lineObjectIds (filters in lines),
#'   columnObjectIds (filters in columns), filterObjectIds (hidden filters for all data);
#'   6. filter_field_object_ids_order - sorting for `filter_field_object_ids`, determines the order of the filters fields.
#'
#' @seealso \code{\link{fedstat_get_data_ids_special_cases_handle},
#'   \link{fedstat_data_ids_filter},
#'   \link{fedstat_post_data_ids_filtered}}
#'
#' @export
#'
#' @importFrom rlang .data
#'
#' @examples
#' \dontrun{
#' # Get data filters identificators for week prices
#' data_ids <- fedstat_get_indicator_data_ids("37426")
#' }
fedstat_get_data_ids <- function(indicator_id,
                                 ...,
                                 timeout_seconds = 180,
                                 retry_max_times = 3,
                                 httr_verbose = httr::verbose(data_out = FALSE)
                                 ) {
  java_script_source_code_with_data_ids_html_node_index <- 12 # Empirically determined value,
  # on the fedstat, this node with java script source code with filter ids in it does not have any attributes, id or classes

  indicator_URL <- paste(FEDSTAT_URL_BASE, "indicator", indicator_id, sep = "/")

  GET_res <- httr::RETRY(
    "GET",
    indicator_URL,
    httr_verbose,
    httr::timeout(timeout_seconds),
    times = retry_max_times,
    ... = ...
  )

  if (httr::http_error(GET_res)) {
    httr::http_condition(GET_res, type = "error")
  }

  GET_html <- xml2::read_html(GET_res, encoding = "UTF-8")
  java_script_source_code_with_data_ids <- rvest::html_nodes(
    GET_html, "script"
  )[[java_script_source_code_with_data_ids_html_node_index]] %>%
    rvest::html_text() %>%
    strsplit("\n") %>%
    unlist()

  java_script_data_ids_json_parsed <- fedstat_java_script_data_ids_parse_to_json(
    java_script_source_code_with_data_ids
  )

  java_script_default_data_ids_object_ids_json_parsed <- fedstat_java_script_default_data_ids_object_ids_parse_to_json(
    java_script_source_code_with_data_ids
  )

  java_script_default_data_ids_object_ids_json_parsed[["filterObjectIds"]] <- c(
    java_script_default_data_ids_object_ids_json_parsed[["filterObjectIds"]],
    "0"
  ) # Add special filterObjectIds (indicator id)

  object_ids_filters <- unlist(java_script_default_data_ids_object_ids_json_parsed, use.names = TRUE) %>%
    `names<-`(stringr::str_remove(names(.), "\\d")) %>% # use.names makes unique names (adds numbers for duplicates), but we don't need this
    data.frame(
      "filter_field_id" = .,
      "filter_field_object_ids" = names(.),
      "filter_field_object_ids_order" = seq(1, length(.))
    )

  indicator_title <- java_script_data_ids_json_parsed[["0"]][["values"]][[indicator_id]][["title"]]

  data_ids_list <- vector("list", length(java_script_data_ids_json_parsed))

  for (i in seq_len(length(java_script_data_ids_json_parsed))) {
    filter_field_id <- names(java_script_data_ids_json_parsed)[i]

    data_ids_list[[i]] <- data.frame(
      "filter_field_id" = filter_field_id,
      "filter_field_title" = java_script_data_ids_json_parsed[[filter_field_id]][["title"]],
      "filter_value_id" = names(java_script_data_ids_json_parsed[[filter_field_id]][["values"]]),
      "filter_value_title" = unlist(lapply(
        java_script_data_ids_json_parsed[[filter_field_id]][["values"]],
        function(x) x[["title"]]
      ))
    )
  }

  data_ids_data_frame <- do.call(rbind.data.frame, c(data_ids_list, make.row.names = FALSE)) %>%
    dplyr::mutate(filter_value_title = gsub("&quot;", "\"", .data[["filter_value_title"]]))

  data_ids_data_frame_with_object_filters <- data_ids_data_frame %>%
    dplyr::left_join(object_ids_filters,
      by = c("filter_field_id" = "filter_field_id")
    )

  return(data_ids_data_frame_with_object_filters)
}
