fedstat_error <- function(message, kind, stage, status = NULL, retry_after = NULL, evidence = NULL) {
  stop(structure(
    list(
      message = message, call = NULL, stage = stage,
      status = status, retry_after = retry_after, evidence = evidence
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
      "Fedstat refused access (HTTP 403).",
      "blocked", stage, status, delay,
      evidence = "http_403"
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
  type <- response$headers[["content-type"]]
  if (!is.null(type) && grepl("text/html", type, fixed = TRUE)) {
    evidence <- fedstat_access_marker(response$content)
    if (!is.null(evidence)) {
      fedstat_error(
        "Fedstat returned a possible access check or refusal; this does not confirm a CAPTCHA.",
        "blocked", stage, status, delay,
        evidence = evidence
      )
    }
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
    time_left <- attr(before, "time_left")
    available <- if (is.function(time_left)) time_left() else Inf
    request_seconds <- timeout_seconds
    deadline <- NULL
    if (is.finite(available)) {
      configs <- Filter(
        function(x) inherits(x, "request"),
        c(list(defaults, httr::timeout(timeout_seconds)), list(...))
      )
      settings <- do.call(c, configs)$options
      configured <- tail(settings[names(settings) %in% c("timeout", "timeout_ms")], 1L)
      request_seconds <- configured[[1L]] / if (names(configured) == "timeout_ms") 1000 else 1
      if (request_seconds == 0) request_seconds <- Inf
      deadline <- httr::timeout(min(request_seconds, available))
    }
    fedstat_event(before, "REQUEST", paste(method, stage), stage = stage, method = method, http_attempt = attempt)
    started <- proc.time()[["elapsed"]]
    result <- tryCatch(
      {
        response <- httr::VERB(method, url, defaults,
          httr::timeout(timeout_seconds), httr::config(followlocation = FALSE),
          ..., agent, deadline,
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
            signalCondition(structure(list(message = "Download cancelled.", call = NULL, stage = stage),
              class = c("fedstat_interrupt", "interrupt", "condition")
            ))
            invokeRestart("abort")
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
    if (is.function(time_left)) {
      if (available <= request_seconds && inherits(result, "fedstat_transient") &&
        grepl("timed? ?out|timeout", conditionMessage(result), ignore.case = TRUE)) {
        fedstat_time_expired(attr(before, "time_limit_seconds"))
      }
      time_left()
    }
    fedstat_event(before, "REQUEST_ERROR", conditionMessage(result),
      stage = stage,
      error_class = class(result)[1L], status = result$status, seconds = seconds, evidence = result$evidence
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
