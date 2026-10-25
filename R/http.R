fedstat_error <- function(message, kind, stage, status = NULL, retry_after = NULL) {
  stop(structure(
    list(
      message = message, call = NULL, stage = stage,
      status = status, retry_after = retry_after
    ),
    class = c(paste0("fedstat_", kind), "fedstat_error", "error", "condition")
  ))
}

fedstat_positive <- function(x, name, integer = FALSE) {
  if (!is.numeric(x) || length(x) != 1L || !is.finite(x) || x <= 0 ||
    (integer && x != floor(x))) {
    stop(name, " must be a positive ",
      if (integer) "integer" else "number",
      call. = FALSE
    )
  }
}

fedstat_retryable <- function(e) {
  inherits(e, "fedstat_transient") || inherits(e, "fedstat_rate_limit")
}

fedstat_backoff <- function(attempt, error) {
  max(
    stats::runif(1L, 1, 2) * 2^min(attempt - 1L, 6L),
    if (is.null(error$retry_after)) 0 else error$retry_after
  )
}

fedstat_retry_after <- function(value) {
  if (is.null(value)) {
    return(NULL)
  }
  seconds <- suppressWarnings(as.numeric(value))
  if (is.na(seconds)) {
    date <- suppressWarnings(httr::parse_http_date(value))
    seconds <- as.numeric(difftime(date, Sys.time(), units = "secs"))
  }
  if (length(seconds) != 1L || !is.finite(seconds)) {
    return(NULL)
  }
  max(0, seconds)
}

fedstat_response <- function(response, stage) {
  status <- httr::status_code(response)
  delay <- fedstat_retry_after(response$headers[["retry-after"]])
  if (status == 429L) {
    fedstat_error(
      "Fedstat rate limit (HTTP 429); retry later.",
      "rate_limit", stage, status, delay
    )
  }
  if (status == 403L) {
    fedstat_error(
      "Fedstat refused access (HTTP 403). No automatic retry.",
      "blocked", stage, status
    )
  }
  if (status >= 500L) {
    fedstat_error(
      paste0("Fedstat unavailable (HTTP ", status, ")."),
      "transient", stage, status, delay
    )
  }
  if (status >= 300L && status < 400L) {
    fedstat_error(
      paste0(
        "Fedstat redirected the ", stage,
        " request (HTTP ", status, "); session or selection may be invalid."
      ),
      "redirect", stage, status
    )
  }
  if (status >= 400L) {
    fedstat_error(
      paste0("Fedstat returned HTTP ", status, "."),
      "http", stage, status
    )
  }
  response
}

fedstat_request <- function(method, url, ..., handle, timeout_seconds,
                            attempts = 1L, stage, before = function() NULL) {
  fedstat_positive(timeout_seconds, "timeout_seconds")
  fedstat_positive(attempts, "retry_max_times", integer = TRUE)
  # Keep curl interruptible in Rscript too, restoring the caller's option on exit.
  previous <- options(curl_interrupt = TRUE)
  on.exit(options(previous), add = TRUE)
  defaults <- fedstat_default_config()
  agent <- fedstat_user_agent(c(list(defaults), list(...)))
  for (attempt in seq_len(attempts)) {
    before()
    fedstat_event(before, "REQUEST", paste(method, stage), stage = stage, method = method, http_attempt = attempt)
    started <- proc.time()[["elapsed"]]
    result <- tryCatch(
      {
        response <- httr::VERB(method, url, defaults,
          httr::timeout(timeout_seconds), httr::config(followlocation = FALSE),
          ..., agent,
          handle = handle
        )
        fedstat_response(response, stage)
      },
      error = function(e) {
        if (inherits(e, "fedstat_error")) {
          return(e)
        }
        # Configuration and parser errors must never become network retries.
        curl_call <- paste(deparse(conditionCall(e)), collapse = " ")
        if (inherits(e, "curl_error") || grepl("curl_fetch_(memory|disk|stream)", curl_call)) {
          # curl < 6.1 reports cancellation as an ordinary transport error (code 42).
          if (inherits(e, "curl_error_aborted_by_callback") ||
            grepl("aborted by (an application )?callback", conditionMessage(e), ignore.case = TRUE)) {
            stop(structure(list(message = "Download cancelled.", call = NULL, stage = stage),
              class = c("fedstat_interrupt", "interrupt", "condition")
            ))
          }
          temporary <- grepl("timed? ?out|timeout|resolve|connect|receiv|recv|send|partial|empty|reset|slow",
            conditionMessage(e),
            ignore.case = TRUE
          )
          return(structure(
            list(
              message = paste0("Fedstat ", stage, ": ", conditionMessage(e)),
              call = NULL, stage = stage
            ),
            class = c(
              if (temporary) "fedstat_transient" else "fedstat_transport",
              "fedstat_error", "error", "condition"
            )
          ))
        }
        e
      }
    )
    seconds <- round(proc.time()[["elapsed"]] - started, 3)
    if (!inherits(result, "error")) {
      fedstat_event(before, "RESPONSE", paste0(
        "HTTP ", result$status_code, "; ", seconds,
        " s; ", length(result$content), " bytes"
      ),
      stage = stage,
      status = result$status_code, seconds = seconds, bytes = length(result$content)
      )
      return(result)
    }
    fedstat_event(before, "REQUEST_ERROR", conditionMessage(result),
      stage = stage,
      error_class = class(result)[1L], status = result$status, seconds = seconds
    )
    if (attempt == attempts || !fedstat_retryable(result)) stop(result)
    seconds <- fedstat_backoff(attempt, result)
    fedstat_event(before, "RETRY", paste0(
      "Retrying ", stage, "; attempt ", attempt + 1L, "/", attempts,
      " after backoff."
    ), stage = stage, seconds = seconds, reason = class(result)[1L])
    schedule <- attr(before, "backoff")
    if (is.function(schedule)) schedule(seconds) else Sys.sleep(seconds)
  }
}

fedstat_event <- function(before, event, message, ...) {
  report <- attr(before, "report")
  if (is.function(report)) report(event, message, ...)
}
