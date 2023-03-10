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
#' Users can specify filter_field_object_ids for each filter_field in resulting data_ids table.
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
#'
#' @seealso \code{\link{fedstat_data_ids_filter},
#'   \link{fedstat_post_data_ids_filtered}}
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Get data filters identificators for CPI
#' data_ids <- fedstat_get_data_ids("31074")
#' }
fedstat_get_data_ids <- function(indicator_id,
                                 ...,
                                 timeout_seconds = 180,
                                 retry_max_times = 3,
                                 httr_verbose = NULL) {

  # workaround for `:=` and CMD check
  filter_value_title <- filter_field_object_ids <- NULL

  indicator_URL <- paste0("https://www.fedstat.ru/indicator/", indicator_id)

  GET_res <- tryCatch(
    expr = httr::RETRY(
      "GET",
      indicator_URL,
      httr_verbose,
      httr::timeout(timeout_seconds),
      times = retry_max_times,
      ... = ...
    ),
    error = function(cond) {
      if (cond[["call"]] == str2lang("f(init, x[[i]])") &&
        cond[["message"]] == "is.request(y) is not TRUE") {
        stop("Passed invalid arguments to ... argument, ",
          "did you accidentally passed filters to ...? ",
          "All arguments after ... must be explicitly named",
          call. = FALSE
        )
      } else {
        stop(cond)
      }
    }
  )

  if (httr::http_error(GET_res)) {
    httr::http_condition(GET_res, type = "error")
  }

  GET_html <- xml2::read_html(GET_res, encoding = "UTF-8")

  js_script <- xml2::xml_find_all(GET_html, ".//script")[[12]] %>% # 12 - Empirically determined value
    xml2::xml_text() %>%
    strsplit("\n") %>%
    unlist()

  filter_list <- parse_js1(js_script)
  object_list <- parse_js2(js_script) %>% unlist()

  if (all(object_list != "0")) {
    object_list[["filterObjectIds"]] <- "0"
  } # Add special filterObjectIds (indicator id)

  object_df <- data.table::data.table(
    filter_field_id = unname(object_list),
    filter_field_object_ids = names(object_list) %>% gsub(x = ., "\\d", ""),
    stringsAsFactors = FALSE
  ) %>%
    unique(by = "filter_field_id")

  filter_id_list <- lapply(filter_list, function(x) {
    filter_values <- x[["values"]]

    if (!length(names(filter_values))) {
      stop(
        "fedstat returned erroneous data_ids. It's probably lagging. There are no filter values in the \"",
        x[["title"]], "\" filter "
      )
    }

    data.frame(
      filter_field_title = x[["title"]],
      filter_value_id = names(filter_values),
      filter_value_title = unlist(lapply(filter_values, function(x) x[["title"]])),
      stringsAsFactors = FALSE, row.names = NULL
    )
  })

  filter_dt <- data.table::rbindlist(filter_id_list, idcol = "filter_field_id", use.names = TRUE)[
    , filter_value_title := gsub("&quot;", "\"", x = filter_value_title)
  ] %>%
    data.table::merge.data.table(object_df, by = "filter_field_id", all.x = TRUE)


  if (data.table::uniqueN(filter_dt) != nrow(filter_dt)) {
    stop("data_ids table with non unique filter_field_id and filter_value_ids pairs")
  }

  filter_dt[, filter_field_object_ids := data.table::fifelse(
    is.na(filter_field_object_ids),
    "lineObjectIds",
    filter_field_object_ids
  )][] %>%
    as.data.frame()
}
