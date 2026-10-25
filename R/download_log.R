# File logs are JSON Lines; console messages are a compact view of the same events.
fedstat_logger <- function(path, indicator_id, verbose, context) {
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
    if (verbose) {
      message(paste0(
        entry$time, " ", event, " [", entry$request, "/", entry$max_requests,
        if (!is.null(entry$part)) paste0(" part=", entry$part) else "", "] ", message
      ))
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
