# File logs are JSON Lines; console messages are a compact view of the same events.
fedstat_logger <- function(path, indicator_id, verbose, context) {
  verbose <- fedstat_verbosity(verbose)
  connection <- NULL
  if (!is.null(path)) {
    if (!is.character(path) || length(path) != 1L || is.na(path) || !nzchar(path) ||
      grepl("[.]rds$", path, ignore.case = TRUE)) {
      stop("log_file must be a log filename (not an RDS checkpoint) or NULL.", call. = FALSE)
    }
    connection <- file(path, open = "at", encoding = "UTF-8")
  }
  started <- proc.time()[["elapsed"]]
  run <- paste0(format(Sys.time(), "%Y%m%dT%H%M%OS6", tz = "UTC"), "-", Sys.getpid())
  report <- function(event, message, ...) {
    # Only allow-listed event fields are supplied by the caller: no HTTP config,
    # URLs, bodies or tokens. Also remove URLs from transport exception messages.
    message <- gsub("https?://[^[:space:]]+", "<URL>", message)
    entry <- c(list(
      time = format(Sys.time(), "%Y-%m-%dT%H:%M:%OS3Z", tz = "UTC"),
      run = run, indicator_id = indicator_id, event = event, message = message,
      elapsed_seconds = round(proc.time()[["elapsed"]] - started, 3)
    ), context())
    extra <- list(...)
    entry[names(extra)] <- extra
    if (!is.null(connection)) {
      writeLines(jsonlite::toJSON(entry, auto_unbox = TRUE, null = "null", digits = NA), connection)
      flush(connection)
    }
    if (verbose == "detailed") {
      message(paste0(
        entry$time, " ", event,
        if (!is.null(entry$part)) paste0(" [part ", entry$part, "]") else "", ": ", message
      ))
    } else if (verbose == "progress") {
      line <- fedstat_console(entry)
      if (!is.null(line)) message(line)
    }
    invisible(NULL)
  }
  attr(report, "close") <- function() if (!is.null(connection)) close(connection)
  report
}

fedstat_progress <- function(manifest) {
  done <- if (is.null(manifest)) list() else Filter(function(p) p$status == "complete", manifest$parts)
  pending <- if (is.null(manifest)) 0L else sum(vapply(manifest$parts, function(p) p$status == "pending", logical(1L)))
  list(
    saved_parts = length(done), pending_parts = pending,
    rows = sum(vapply(done, function(p) as.numeric(p$nrow), numeric(1L)))
  )
}

# Match the existing filter helper's normalization, without changing selection.
fedstat_filters <- function(filters) {
  if (identical(filters, "*")) {
    return(filters)
  }
  if (!is.list(filters) || is.data.frame(filters) ||
    (length(filters) && (is.null(names(filters)) || anyNA(names(filters)) || any(!nzchar(trimws(names(filters))))))) {
    stop("filters must be a named list or the single string '*'.", call. = FALSE)
  }
  normalize <- function(x) tolower(gsub("\\s+", " ", trimws(enc2utf8(as.character(x)))))
  if (!length(filters)) {
    return(list())
  }
  if (any(lengths(filters) == 0L) || any(vapply(filters, anyNA, logical(1L)))) {
    stop("Filter values must be nonempty and cannot contain NA.", call. = FALSE)
  }
  fields <- normalize(names(filters))
  names <- sort(unique(fields), method = "radix")
  result <- lapply(names, function(name) sort(unique(normalize(unlist(filters[fields == name], use.names = FALSE))), method = "radix"))
  stats::setNames(result, names)
}

fedstat_verbosity <- function(verbose) {
  if (identical(verbose, TRUE)) {
    return("progress")
  }
  if (identical(verbose, FALSE)) {
    return("quiet")
  }
  if (!is.character(verbose) || length(verbose) != 1L || is.na(verbose) ||
    !verbose %in% c("quiet", "progress", "detailed")) {
    stop('verbose must be TRUE, FALSE, "quiet", "progress" or "detailed".', call. = FALSE)
  }
  verbose
}

fedstat_console <- function(e) {
  number <- function(n) format(n, big.mark = ",", scientific = FALSE, trim = TRUE)
  switch(e$event,
    START = paste0(
      "==> Downloading indicator ", e$indicator_id, ". Up to ",
      round(e$time_limit_seconds / 60, 1), " minutes this run."
    ),
    PREPARE = "==> Reading available filters...",
    VERIFY = "==> Checking previously saved data...",
    RESUME = paste0("==> Continuing with ", e$saved_parts, " parts already saved."),
    DOWNLOAD = "==> Downloading the next part...",
    SAVED = paste0("  Saved ", number(e$rows), " rows so far in ", e$saved_parts, " parts."),
    RETRY = "  Fedstat did not return the data. Will try again after a pause.",
    RECOVER = if (!identical(e$reason, "fedstat_blocked")) {
      paste0(
        "  Fedstat is still unavailable. Pausing for at least ", round(e$seconds / 60, 1), " minutes before trying again."
      )
    },
    SPLIT = if (!identical(e$reason, "size_limit")) "  This request failed. Trying smaller parts.",
    VALIDATE = "==> Download finished. Checking the saved data...",
    COMPLETE = paste0("==> Ready: ", number(e$rows), " rows in ", e$saved_parts, " saved parts. Use fedstat_collect() to open the table."),
    INTERRUPTED = paste0("  Stopped by you. ", e$saved_parts, " parts are saved; run the same call to continue."),
    STOP = paste0(
      "  Download stopped. ", e$saved_parts, " parts are saved. ",
      if (identical(e$error_class, "fedstat_time_limit")) {
        "Time is up; run the same call to continue."
      } else {
        "See the error below and download.log for details."
      }
    ),
    NULL
  )
}
