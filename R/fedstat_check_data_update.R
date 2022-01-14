#' Very basic task scheduler for checking the indicator data updates (EXPERIMENTAL)
#'
#' @description In practice fedstat often does not give true
#'   information about when the data was updated. Because of this,
#'   the only universal and accurate way to check for updated data is to try to
#'   download it and compare it to past data. If a query with the same filters
#'   as before returned different `data_ids` or `data_df`
#'   (result of `fedstat_parse_sdmx_to_table`), then the data has been updated.
#'
#'   This function is not tested on real data
#'
#'   Logging to a file is supported
#'
#' @param prepared_reference_data_for_check_data_update result of
#'  \link{fedstat_prepare_reference_data_for_check_data_update}
#' @param ... other arguments passed to httr::GET and httr::POST
#' @param pause_minutes_between_requests numeric, the number of minutes to wait
#'  before the next request, cannot be less than 10 minutes to avoid a heavy load on the fedstat.ru
#'  The time between requests will not be exactly equal to the specified value,
#'  because the countdown starts only after the request is finished
#' @param max_checks numeric, maximum number of checks before returning `FALSE`
#' @param verbose_tries bool, enables or disables verbose messages,
#'  sends messages to the console about the progress of attempts to find out whether the data have been updated
#' @param timeout_seconds numeric, maximum time before a new GET and POST request is tried
#' @param retry_max_times numeric, maximum number of tries to GET and POST `data_ids`
#' @param disable_warnings bool, enables or disables following warnings in `fedstat_data_ids_filter`:
#' 1. About non matched `filter_value_title` in `filters` and `filter_value_title` from `data_ids`;
#' 2. About unspecified `filter_filed_title` in `filters`.
#' @param httr_verbose `httr::verbose()` or NULL, outputs messages to the console
#' about the processing of the request
#' @param logfile the path to the log file, if NULL, the messages are sent to the console
#' @param overwrite_logfile boolean, if true overwrites content of `logfile`
#'
#' @return bool, TRUE if indicator data has been updated, FALSE if max_checks is exceeded
#' @export
#'
#' @examples
#' \dontrun{
#' is_updated <- fedstat_prepare_reference_data_for_check_data_update(
#'   indicator_id = "37426",
#'   filters = list(
#'     "Territory" = "Russian Federation",
#'     "Types of goods and services" = "Sahar-pesok, kg"
#'   )
#' ) %>%
#'   fedstat_check_data_update()
#'
#' if (is_updated) print("Data for the indicator 37426 has been updated!")
#'
#' # Not actual filter field titles and filter values titles because of ASCII requirement for CRAN
#' }
fedstat_check_data_update <- function(prepared_reference_data_for_check_data_update,
                                      ...,
                                      pause_minutes_between_requests = 30,
                                      max_checks = 50,
                                      verbose_tries = TRUE,
                                      timeout_seconds = 180,
                                      retry_max_times = 10,
                                      disable_warnings = TRUE,
                                      httr_verbose = NULL,
                                      logfile = NULL,
                                      overwrite_logfile = FALSE) {
  if (!file.exists(logfile)) {
    file.create(logfile)
  } else if (overwrite_logfile) {
    file.create(logfile)
  }

  if (pause_minutes_between_requests < 10) {
    stop("pause_minutes_between_requests cannot be less than 10 minutes to avoid a heavy load on the fedstat.ru")
  }

  for (check_number in seq_len(max_checks)) {
    if (is.character(logfile)) {
      logfile_con <- file(logfile, open = "a")
      sink(logfile_con, type = "message")
    }

    if (verbose_tries || is.character(logfile)) {
      message(
        "----------------------------------------",
        "Trying to check data updates, timestamp: ",
        as.character(Sys.time()),
        ", this is a ",
        check_number,
        " check try, ",
        max_checks - check_number,
        " checks to go"
      )
    }

    is_updated <- tryCatch(
    expr = fedstat_check_data_update_(
      indicator_id = prepared_reference_data_for_check_data_update[["indicator_id"]],
      reference_data_ids_unfiltered_special_cases_handled = prepared_reference_data_for_check_data_update[["reference_data_ids_unfiltered_special_cases_handled"]],
      reference_data_df = prepared_reference_data_for_check_data_update[["reference_data_df"]],
      ... = ...,
      filters = prepared_reference_data_for_check_data_update[["filters"]],
      time_filter_fields_titles = prepared_reference_data_for_check_data_update[["time_filter_fields_titles"]],
      time_fields_titles_in_df = prepared_reference_data_for_check_data_update[["time_fields_titles_in_df"]],
      filter_value_title_alias_lookup_table = prepared_reference_data_for_check_data_update[["filter_value_title_alias_lookup_table"]],
      timeout_seconds = timeout_seconds,
      retry_max_times = retry_max_times,
      disable_warnings = disable_warnings,
      httr_verbose = httr_verbose
    ),
    error = function(cond) {
      message("Something went wrong when trying to query data,",
      " the error may be due to incorrect specification of the query",
      " by the user or the inaccessibility of the indicator on fedstat.ru.",
      " To check for a potential error on the user side, please try to query the data",
      " with the regular functions from the package")
      return(FALSE)
    }
  )

    if (verbose_tries || is.character(logfile)) {
      if (is_updated) {
        message("Sucess! The data has just been updated")
      } else {
        message(
          "The check was successful, the data has not yet been updated, next try after ",
          round(pause_minutes_between_requests),
          " minutes"
        )
      }
    }

    if (is.character(logfile)) {
      sink(type = "message")
      close(logfile_con)
    }

    if (!is_updated) {
      Sys.sleep(pause_minutes_between_requests * 60)
    } else {
      message("done")
      return(TRUE)
    }

    
  }

  if (is.character(logfile)) {
    logfile_con <- file(logfile, open = "a")
    sink(logfile_con, type = "message")
  }
  message("Max check number exceeded")
  if (is.character(logfile)) {
    sink(type = "message")
    close(logfile_con)
  }
  message("done")
  return(FALSE)
}