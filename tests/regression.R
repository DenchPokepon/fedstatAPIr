# Offline regression tests. No test framework and no live Fedstat traffic.
library(fedstatAPIr)
ns <- asNamespace("fedstatAPIr")
internal <- function(name) get(name, ns)
expect_error <- function(expr, class = "error", pattern = NULL) {
  e <- tryCatch(
    {
      force(expr)
      NULL
    },
    error = identity
  )
  stopifnot(inherits(e, class))
  if (!is.null(pattern)) stopifnot(grepl(pattern, conditionMessage(e)))
  invisible(e)
}
mock <- function(name, value, code, where = ns) {
  ns <- where
  old <- get(name, ns)
  unlockBinding(name, ns)
  assign(name, value, ns)
  lockBinding(name, ns)
  on.exit({
    unlockBinding(name, ns)
    assign(name, old, ns)
    lockBinding(name, ns)
  })
  force(code)
}
fixture <- function(name) {
  path <- file.path("fixtures", name)
  if (!file.exists(path)) path <- file.path("tests", path)
  path
}
response <- function(content, status = 200L, type = "text/html") {
  structure(list(
    status_code = status, headers = list("content-type" = type),
    content = if (is.raw(content)) content else charToRaw(content)
  ), class = "response")
}
html <- readBin(fixture("indicator.html"), "raw", file.info(fixture("indicator.html"))$size)
xml <- readBin(fixture("generic.xml"), "raw", file.info(fixture("generic.xml"))$size)

# Script discovery does not depend on the twelfth script; metadata/session stay separate.
ids <- mock("fedstat_request", function(...) response(html), fedstat_get_data_ids("31074"))
stopifnot(nrow(ids) == 5L, all(vapply(ids, is.character, logical(1L))))
selected <- fedstat_data_ids_filter(ids, list(Year = "2024", Area = "*"))
stopifnot(identical(attr(selected, "fedstat_session"), attr(ids, "fedstat_session")))
expect_error(internal("fedstat_metadata")(xml2::read_html("<html>Unavailable</html>")), pattern = "Expected one script")
expect_error(fedstat_data_ids_filter(ids, list(Year = "2024")), pattern = "explicitly specified")
expect_error(internal("fedstat_download_selection")(ids, list(Year = c("2024", "typo"), Area = "*")), pattern = "No matching")

# Same public parser output, preserved codes, correct numeric failures, dictionary fast path.
data <- fedstat_parse_sdmx_to_table(xml)
stopifnot(
  nrow(data) == 2L, identical(data$AREA_code, c("001", "001")),
  identical(data$ObsValue, c(123.4, 125))
)
stopifnot(identical(data, fedstat_parse_sdmx_to_table(fixture("generic.xml"))))
marker <- charToRaw(sub('value="123,4"', 'value="..."', rawToChar(xml), fixed = TRUE))
expect_error(fedstat_parse_sdmx_to_table(marker), pattern = "try_to_parse_ObsValue = FALSE")
stopifnot(fedstat_parse_sdmx_to_table(marker, try_to_parse_ObsValue = FALSE)$ObsValue[1] == "...")
stopifnot(nrow(fedstat_parse_sdmx_to_table(marker, return_type = "dictionary")) == 1L)
expect_error(fedstat_parse_sdmx_to_table(charToRaw(sub("false", "true", rawToChar(xml)))), "fedstat_truncated")
expect_error(fedstat_parse_sdmx_to_table(charToRaw("<html/>")), "fedstat_parse")
empty <- charToRaw(sub("<generic:Series>.*</generic:Series>", "", rawToChar(xml)))
stopifnot(nrow(fedstat_parse_sdmx_to_table(empty)) == 0L)

# Trimmed real Fedstat SDMX 1.0: series attributes, raw values and leading zeroes.
real <- fedstat_parse_sdmx_to_table(fixture("fedstat-37089.xml"), try_to_parse_ObsValue = FALSE)
stopifnot(
  identical(real$s_OKATO_code, rep("05000000000", 2L)),
  identical(real$ObsValue, c("211765", "312597,6")),
  identical(real$Time, c("2010", "2011"))
)
expect_error(internal("fedstat_parse_sdmx")(xml, indicator_id = "999"), "fedstat_parse")

# MIME parameters, safe errors, encoded repeated fields and shared token state.
stopifnot(identical(internal("fedstat_export_content")(response(xml, type = "text/xml; charset=UTF-8"), "sdmx"), xml))
expect_error(internal("fedstat_export_content")(response("<html>CSRF secret-token</html>"), "sdmx"), "fedstat_token")
for (status in c(403L, 429L, 503L, 302L)) {
  expect_error(internal("fedstat_response")(response("", status), "export"), "fedstat_error")
}
state <- attr(ids, "fedstat_session")
state$token <- "one&two=three"
body <- internal("fedstat_form")(ids, state, "sdmx")
stopifnot(
  grepl("one%26two%3Dthree", body, fixed = TRUE),
  lengths(regmatches(body, gregexpr("selectedFilterIds=", body, fixed = TRUE))) == nrow(ids)
)

# HTTP configuration reaches BOTH fallback token GET and export POST.
local({
  old <- options(
    httr_config = httr::add_headers("User-Agent" = "global-agent"),
    fedstatAPIr.config = httr::config(connecttimeout = 19)
  )
  on.exit(options(old))
  captured <- list()
  legacy <- ids
  for (name in c("fedstat_session", "fedstat_csrf_token", "fedstat_csrf_token_name", "fedstat_handle")) attr(legacy, name) <- NULL
  verb <- function(method, url, ..., handle) {
    configs <- Filter(function(x) inherits(x, "request"), list(...))
    captured[[length(captured) + 1L]] <<- do.call(c, configs)
    if (method == "GET") response(html) else response(xml, type = "text/xml; charset=UTF-8")
  }
  mock("VERB", verb, fedstat_post_data_ids_filtered(legacy,
    httr::user_agent("per-call-agent"), httr::config(connecttimeout = 7),
    timeout_seconds = 2, retry_max_times = 1
  ), where = asNamespace("httr"))
  stopifnot(length(captured) == 2L)
  for (request in captured) {
    stopifnot(
      request$options$useragent == "per-call-agent",
      request$headers[["User-Agent"]] == "per-call-agent",
      request$options$connecttimeout == 7,
      request$options$timeout_ms == 2000,
      !request$options$followlocation
    )
  }
})
stopifnot(
  internal("fedstat_retry_after")("60") == 60,
  is.null(internal("fedstat_retry_after")("invalid"))
)

# Older curl versions signal simpleError, with the originating HTTP call attached.
mock("VERB", function(...) {
  stop(simpleError("Failure when receiving data from the peer",
    call = quote(curl_fetch_memory(url, handle))
  ))
},
expect_error(internal("fedstat_request")("GET", "http://example.invalid",
  handle = httr::handle("http://example.invalid"), timeout_seconds = 1,
  stage = "metadata"), "fedstat_transient"),
where = asNamespace("httr")
)

# Retry recovery performs one metadata read, refreshes the token, and preserves selection.
gets <- exports <- refreshes <- 0L
mock(
  "fedstat_get_data_ids", function(...) {
    gets <<- gets + 1L
    ids
  },
  mock("fedstat_post_data_ids_filtered", function(data_ids, ...) {
    exports <<- exports + 1L
    if (exports == 1L) internal("fedstat_error")("expired", "token", "export")
    stopifnot(is.null(attr(data_ids, "fedstat_session")$token))
    xml
  }, mock(
    "fedstat_backoff", function(...) 0,
    stopifnot(nrow(fedstat_data_load_with_filters("31074",
      filters = list(Year = "*", Area = "*"),
      loading_steps_verbose = FALSE
    )) == 2L)
  ))
)
stopifnot(gets == 1L, exports == 2L)

# Labels cannot disguise conflicting observations with identical source codes.
expect_error(
  internal("fedstat_check_data")(data.frame(
    AREA = c("old", "new"),
    AREA_code = c("001", "001"), Time = c("2024", "2024"), ObsValue = c("1", "2")
  )),
  pattern = "Duplicate observation keys"
)

# Batch fixtures return the exact Cartesian selection so union/duplicate tests are meaningful.
fake_data <- function(selected) {
  groups <- split(selected$filter_value_id, selected$filter_field_id)
  out <- expand.grid(Year = groups[["1"]], Area_code = groups[["2"]], stringsAsFactors = FALSE)
  out$ObsValue <- "1,5"
  out
}
with_batch <- function(code) {
  mock(
    "fedstat_read_ids", function(..., before = function() NULL) {
      before()
      ids
    },
    mock("fedstat_post_data_ids_filtered", function(data_ids, ...) {
      before <- attr(data_ids, "fedstat_session")$before
      if (!is.null(before)) before()
      serialize(fake_data(data_ids), NULL)
    }, mock("fedstat_parse_sdmx", function(data_raw, ...) unserialize(data_raw), force(code)))
  )
}
path <- tempfile()
with_batch({
  job <- fedstat_download("31074",
    filters = "*", path = path, max_nominal_cells_request = 2,
    request_pause = 0, verbose = FALSE, recovery_max_times = 0
  )
  stopifnot(job$complete, nrow(job$parts) == 2L, nrow(fedstat_collect(job)) == 4L)
  mock(
    "fedstat_read_ids", function(...) stop("Must not make a network call"),
    stopifnot(fedstat_download("31074", filters = "*", path = path, verbose = FALSE, recovery_max_times = 0)$complete)
  )
  expect_error(fedstat_download("31074", filters = list(Year = "2024"), path = path, verbose = FALSE, recovery_max_times = 0), pattern = "different selection")
})
# Integrity checks detect tampering instead of silently accepting damaged checkpoints.
m <- readRDS(file.path(path, "manifest.rds"))
part <- m$parts[[which(vapply(m$parts, function(p) p$status == "complete", logical(1L)))[1L]]]
writeLines("damaged", file.path(path, part$file))
expect_error(fedstat_collect(path), pattern = "missing or changed")
unlink(path, recursive = TRUE)

# Expire the invocation while parsing a successful part; it must still be saved.
expire_after_part <- function(code) {
  clock <- 0
  mock(
    "fedstat_elapsed", function() clock,
    mock("fedstat_parse_sdmx", function(data_raw, ...) {
      clock <<- 3600
      unserialize(data_raw)
    }, force(code))
  )
}

# Time exhaustion preserves the first part; resume fetches only the unfinished part.
path <- tempfile()
with_batch({
  expire_after_part(expect_error(fedstat_download("31074",
    filters = "*", path = path, max_nominal_cells_request = 2,
    request_pause = 0, verbose = FALSE, recovery_max_times = 0
  ), "fedstat_time_limit"))
  m <- readRDS(file.path(path, "manifest.rds"))
  stopifnot(sum(vapply(m$parts, function(p) p$status == "complete", logical(1L))) == 1L)
  stopifnot(fedstat_download("31074",
    filters = "*", path = path, request_pause = 0,
    verbose = FALSE, recovery_max_times = 0
  )$complete)
})
unlink(path, recursive = TRUE)

# A token/metadata-stage timeout must not create subdivisions.
for (stage in c("token", "metadata")) {
  path <- tempfile()
  mock(
    "fedstat_read_ids", function(..., before = function() NULL) {
      before()
      ids
    },
    mock(
      "fedstat_post_data_ids_filtered", function(...) internal("fedstat_error")("timeout", "transient", stage),
      mock(
        "fedstat_backoff", function(...) 0,
        expect_error(fedstat_download("31074",
          filters = "*", path = path, request_pause = 0,
          verbose = FALSE, recovery_max_times = 0
        ), "fedstat_transient")
      )
    )
  )
  stopifnot(length(readRDS(file.path(path, "manifest.rds"))$parts) == 1L)
  unlink(path, recursive = TRUE)
}

# An export that is too large subdivides; persistent failures stop at the circuit limit.
for (outage in c(FALSE, TRUE)) {
  path <- tempfile()
  mock(
    "fedstat_read_ids", function(..., before = function() NULL) {
      before()
      ids
    },
    mock("fedstat_post_data_ids_filtered", function(data_ids, ...) {
      attr(data_ids, "fedstat_session")$before()
      if (outage || nrow(fake_data(data_ids)) > 2L) internal("fedstat_error")("timeout", "transient", "export")
      serialize(fake_data(data_ids), NULL)
    }, mock(
      "fedstat_parse_sdmx", function(data_raw, ...) unserialize(data_raw),
      mock("fedstat_backoff", function(...) 0, {
        run <- function() {
          fedstat_download("31074",
            filters = "*", path = path, request_pause = 0,
            max_failures = 3L, verbose = FALSE, recovery_max_times = 0
          )
        }
        if (outage) expect_error(run(), "fedstat_unavailable") else stopifnot(run()$complete)
      })
    ))
  )
  unlink(path, recursive = TRUE)
}
cat("Offline regression tests passed.\n")

# Resume accepts equivalent filters, while retaining the original frozen scope.
path <- tempfile()
with_batch({
  first <- list(Year = c(2024, 2025), Area = c("First", "Second"))
  reordered <- list(" area " = c("SECOND", "first", "first"), YEAR = c("2025", "2024"))
  job <- fedstat_download("31074",
    filters = first, path = path, max_nominal_cells_request = 2,
    request_pause = 0, verbose = FALSE, recovery_max_times = 0
  )
  mock(
    "fedstat_read_ids", function(...) stop("Unexpected network request"),
    stopifnot(fedstat_download("31074", filters = reordered, path = path, verbose = FALSE, recovery_max_times = 0)$complete)
  )
  expect_error(fedstat_download("31074",
    filters = list(Year = 2024, Area = "First"), path = path,
    verbose = FALSE, recovery_max_times = 0
  ), pattern = "different selection")
  # Old manifests are revalidated once, offline.
  m <- readRDS(file.path(path, "manifest.rds"))
  m$validated <- NULL
  saveRDS(m, file.path(path, "manifest.rds"))
  mock(
    "fedstat_read_ids", function(...) stop("Unexpected network request"),
    stopifnot(fedstat_download("31074", filters = first, path = path, verbose = FALSE, recovery_max_times = 0)$complete)
  )
})
unlink(path, recursive = TRUE)

# A provider ignoring filters must fail final validation, never report completion.
path <- tempfile()
with_batch(mock("fedstat_parse_sdmx", function(...) data.frame(Year = "2024", Area_code = "1", ObsValue = "1"), {
  expect_error(fedstat_download("31074",
    filters = "*", path = path, max_nominal_cells_request = 2,
    request_pause = 0, verbose = FALSE, recovery_max_times = 0
  ), pattern = "across parts")
  m <- readRDS(file.path(path, "manifest.rds"))
  stopifnot(!isTRUE(m$validated), !internal("fedstat_download_result")(m, path)$complete)
  events <- lapply(readLines(file.path(path, "download.log")), jsonlite::fromJSON)
  stopifnot(
    !any(vapply(events, function(e) e$event == "COMPLETE", logical(1L))),
    tail(events, 1)[[1]]$event == "STOP", tail(events, 1)[[1]]$stage == "validation",
    tail(events, 1)[[1]]$saved_parts == 2L
  )
}))
unlink(path, recursive = TRUE)

# Canonical keys distinguish delimiters, missing values and Unicode strings.
keys <- internal("fedstat_keys")(data.frame(
  a = c(NA, "NA", "a|b", "a", "a\nb"),
  b = c("", "", "c", "b|c", "")
), c("a", "b"))
stopifnot(!anyDuplicated(keys))

# Exercise real HTTP orchestration with a fake transport: no live service traffic.
path <- tempfile()
local({
  verb <- function(method, url, ..., handle) {
    if (method == "GET") {
      return(response(html))
    }
    args <- list(...)
    selected_years <- sub("1_", "", regmatches(args$body, gregexpr("1_202[45]", args$body))[[1]])
    document <- xml2::read_xml(xml)
    obs <- xml2::xml_find_all(document, "//*[local-name()='Obs']")
    years <- xml2::xml_text(xml2::xml_find_first(obs, "./*[local-name()='Time']"))
    xml2::xml_remove(obs[!years %in% selected_years])
    response(charToRaw(as.character(document)), type = "text/xml")
  }
  mock("VERB",
    verb,
    {
      job <- fedstat_download("31074",
        filters = list(Year = c("2024", "2025"), Area = "First"),
        path = path, max_nominal_cells_request = 1, token_policy = "refresh", request_pause = 0, verbose = FALSE, recovery_max_times = 0
      )
      stopifnot(job$complete, nrow(fedstat_collect(job)) == 2L)
    },
    where = asNamespace("httr")
  )
  lines <- readLines(file.path(path, "download.log"))
  events <- lapply(lines, jsonlite::fromJSON)
  types <- vapply(events, `[[`, "", "event")
  stopifnot(
    all(c("WAIT", "REQUEST", "RESPONSE", "TOKEN_REFRESH", "SPLIT", "SAVED", "VALIDATED", "COMPLETE") %in% types),
    !any(grepl("fixture-token|struts.token.name|Authorization|JSESSIONID", lines, fixed = FALSE))
  )
  requests <- events[types == "REQUEST"]
  stopifnot(
    identical(vapply(requests, `[[`, "", "stage"), c("metadata", "export", "token", "export")),
    identical(vapply(requests, function(e) as.integer(e$request), integer(1L)), 1:4)
  )
  split <- events[[which(types == "SPLIT")[1L]]]
  stopifnot(split$reason == "size_limit", split$dimension == "Year", sum(split$cardinalities) == 2L)
  # Wait messages precede the next request; the log has actual delays and timestamps.
  for (i in which(types == "WAIT")) stopifnot(types[i + 1L] == "REQUEST", !is.null(events[[i]]$next_request_at))
})
unlink(path, recursive = TRUE)

# Metadata errors and interruptions get a final recovery summary and release locks.
path <- tempfile()
mock("VERB", function(...) response("secret response body", status = 503L),
  expect_error(fedstat_download("31074",
    filters = "*", path = path, retry_max_times = 1,
    verbose = FALSE, recovery_max_times = 0
  ), "fedstat_transient"),
  where = asNamespace("httr")
)
events <- lapply(readLines(file.path(path, "download.log")), jsonlite::fromJSON)
stopifnot(
  tail(events, 1)[[1]]$event == "STOP", tail(events, 1)[[1]]$stage == "metadata",
  !dir.exists(file.path(path, ".lock")), !any(grepl("secret response body", readLines(file.path(path, "download.log"))))
)
unlink(path, recursive = TRUE)
path <- tempfile()
with_batch(mock("fedstat_post_data_ids_filtered", function(...) {
  signalCondition(structure(list(message = "Cancelled", call = NULL), class = c("interrupt", "condition")))
  stop("Interrupt was not handled")
}, stopifnot(tryCatch(
  {
    fedstat_download("31074", filters = "*", path = path, verbose = FALSE, recovery_max_times = 0)
    FALSE
  },
  interrupt = function(e) TRUE
))))
events <- lapply(readLines(file.path(path, "download.log")), jsonlite::fromJSON)
stopifnot(tail(events, 1)[[1]]$event == "INTERRUPTED", !dir.exists(file.path(path, ".lock")))
unlink(path, recursive = TRUE)
cat("Diagnostics and final-validation regression tests passed.\n")

# Temporary metadata outages recover within one invocation, with a bounded cooldown.
path <- tempfile()
local({
  calls <- 0L
  with_batch(mock("fedstat_read_ids", function(..., before = function() NULL) {
    before()
    calls <<- calls + 1L
    if (calls <= 2L) internal("fedstat_error")("empty reply", "transient", "metadata")
    ids
  }, mock("fedstat_backoff", function(...) 0, {
    job <- fedstat_download("31074",
      filters = "*", path = path, request_pause = 0,
      recovery_pause = 0, recovery_max_times = 1, verbose = FALSE
    )
    stopifnot(job$complete, calls == 3L)
  })))
  events <- lapply(readLines(file.path(path, "download.log")), jsonlite::fromJSON)
  stopifnot(
    sum(vapply(events, function(e) e$event == "RECOVER", logical(1L))) == 1L,
    dir.exists(file.path(path, "parts")), !length(list.files(path, pattern = "^part-"))
  )
})
unlink(path, recursive = TRUE)

# Reproduce the reported sequence through HTTP: 302, token GET failure/retry,
# another 302, then successful smaller exports. Never treat redirects as empty.
path <- tempfile()
local({
  gets <- posts <- 0L
  verb <- function(method, url, ..., handle) {
    if (method == "GET") {
      gets <<- gets + 1L
      if (gets == 2L) stop(simpleError("Empty reply from server", quote(curl_fetch_memory(url, handle))))
      return(response(html))
    }
    posts <<- posts + 1L
    body <- list(...)$body
    years <- sub("1_", "", regmatches(body, gregexpr("1_202[45]", body))[[1L]])
    if (length(years) > 1L) {
      return(response("", status = 302L))
    }
    data <- data.frame(Year = years, Area_code = "1", ObsValue = "1")
    response(serialize(data, NULL), type = "application/octet-stream")
  }
  mock("VERB", verb, mock(
    "fedstat_backoff", function(...) 0,
    mock("fedstat_parse_sdmx", function(data_raw, ...) unserialize(data_raw), {
      job <- fedstat_download("31074",
        filters = list(Year = "*", Area = "First"), path = path,
        request_pause = 0, recovery_pause = 0, verbose = FALSE
      )
      stopifnot(job$complete, nrow(fedstat_collect(job)) == 2L, posts == 4L, gets == 4L)
    })
  ), where = asNamespace("httr"))
  events <- lapply(readLines(file.path(path, "download.log")), jsonlite::fromJSON)
  stopifnot(any(vapply(events, function(e) identical(e$reason, "rejected_after_refresh"), logical(1L))))
})
unlink(path, recursive = TRUE)

# Persistent rejections cannot grow an unbounded split tree or report completion.
path <- tempfile()
local({
  calls <- 0L
  with_batch(mock("fedstat_post_data_ids_filtered", function(data_ids, ...) {
    attr(data_ids, "fedstat_session")$before()
    calls <<- calls + 1L
    internal("fedstat_error")("redirect", "redirect", "export", 302L)
  }, mock("fedstat_backoff", function(...) 0, {
    expect_error(fedstat_download("31074",
      filters = "*", path = path, request_pause = 0,
      recovery_pause = 0, recovery_max_times = 1, max_failures = 3, verbose = FALSE
    ), "fedstat_unavailable")
    stopifnot(calls == 6L)
  })))
  events <- lapply(readLines(file.path(path, "download.log")), jsonlite::fromJSON)
  stopifnot(!any(vapply(events, function(e) e$event == "COMPLETE", logical(1L))))
})
unlink(path, recursive = TRUE)

# Existing flat-layout checkpoints resume without moving or refetching saved data.
path <- tempfile()
with_batch({
  expire_after_part(expect_error(fedstat_download("31074",
    filters = "*", path = path, max_nominal_cells_request = 2,
    request_pause = 0, verbose = FALSE
  ), "fedstat_time_limit"))
  m <- readRDS(file.path(path, "manifest.rds"))
  m$parts_dir <- NULL
  for (i in seq_along(m$parts)) {
    if (m$parts[[i]]$status == "complete") {
      file <- m$parts[[i]]$file
      stopifnot(file.rename(file.path(path, file), file.path(path, basename(file))))
      m$parts[[i]]$file <- basename(file)
    }
  }
  saveRDS(m, file.path(path, "manifest.rds"))
  unlink(file.path(path, "parts"), recursive = TRUE)
  job <- fedstat_download("31074",
    filters = "*", path = path,
    request_pause = 0, verbose = FALSE
  )
  stopifnot(job$complete, !dir.exists(file.path(path, "parts")), nrow(fedstat_collect(job)) == 4L)
})
unlink(path, recursive = TRUE)

# Warnings remain R warnings and are also saved immediately with part/stage context.
path <- tempfile()
with_batch(mock("fedstat_parse_sdmx", function(data_raw, ...) {
  warning("fixture parser warning", call. = FALSE)
  unserialize(data_raw)
}, suppressWarnings(fedstat_download("31074",
  filters = "*", path = path,
  request_pause = 0, verbose = FALSE
))))
events <- lapply(readLines(file.path(path, "download.log")), jsonlite::fromJSON)
warnings <- Filter(function(e) e$event == "WARNING", events)
stopifnot(
  length(warnings) == 1L, warnings[[1L]]$stage == "parse",
  warnings[[1L]]$message == "fixture parser warning", warnings[[1L]]$part == "1"
)
unlink(path, recursive = TRUE)
cat("Recovery and storage compatibility regression tests passed.\n")

# Throttling waits for Retry-After; persistent access refusals have bounded cooldowns.
for (status in c(429L, 403L)) {
  local({
    path <- tempfile()
    on.exit(unlink(path, recursive = TRUE))
    posts <- 0L
    waits <- numeric()
    with_batch(mock("fedstat_post_data_ids_filtered", function(data_ids, ...) {
      attr(data_ids, "fedstat_session")$before()
      posts <<- posts + 1L
      if (status == 403L || posts == 1L) {
        internal("fedstat_error")("service response",
          if (status == 429L) "rate_limit" else "blocked", "export", status, 120)
      }
      serialize(fake_data(data_ids), NULL)
    }, mock("Sys.sleep",
      function(time) {
        waits <<- c(waits, time)
      },
      {
        run <- function() {
          fedstat_download("31074",
            filters = "*", path = path,
            request_pause = 0, recovery_pause = 0, verbose = FALSE
          )
        }
        if (status == 403L) {
          suppressWarnings(expect_error(run(), "fedstat_blocked"))
          stopifnot(posts == 4L, identical(waits, c(0, 300, 600, 1200)))
        } else {
          stopifnot(run()$complete, posts == 2L, max(waits) >= 120)
        }
      },
      where = baseenv()
    )))
    stopifnot(length(readRDS(file.path(path, "manifest.rds"))$parts) == 1L)
  })
}
cat("Rate limit and access refusal regression tests passed.\n")

# Expiry at the failure limit must not announce a cooldown it cannot run.
local({
  path <- tempfile()
  on.exit(unlink(path, recursive = TRUE))
  clock <- 0
  calls <- 0L
  with_batch(mock(
    "fedstat_elapsed", function() clock,
    mock("fedstat_post_data_ids_filtered", function(data_ids, ...) {
      attr(data_ids, "fedstat_session")$before()
      calls <<- calls + 1L
      clock <<- 3600
      internal("fedstat_error")("redirect", "redirect", "export", 302L)
    }, expect_error(fedstat_download("31074",
      filters = "*", path = path,
      request_pause = 0, max_failures = 1, verbose = FALSE
    ), "fedstat_time_limit"))
  ))
  events <- lapply(readLines(file.path(path, "download.log")), jsonlite::fromJSON)
  stopifnot(
    calls == 1L,
    !any(vapply(events, function(e) e$event == "RECOVER", logical(1L)))
  )
})

# A 64 KiB sniff can end halfway through a valid UTF-8 character. It only scans
# ASCII markers and must not warn or alter the complete response bytes.
local({
  old <- options(warn = 2)
  on.exit(options(old))
  prefix <- function(start) {
    c(
      charToRaw(start), charToRaw(strrep("a", 65535L - nchar(start))),
      as.raw(c(0xd0, 0xb0)), charToRaw("</root>")
    )
  }
  document <- prefix("<GenericData>")
  stopifnot(identical(internal("fedstat_export_content")(
    response(document, type = "text/xml"), "sdmx"), document))
  expect_error(internal("fedstat_export_content")(
    response(prefix("<html><body><h1>captcha</h1>")), "sdmx"), "fedstat_blocked")
  expect_error(internal("fedstat_export_content")(
    response(prefix("<html>invalid token")), "sdmx"), "fedstat_token")
})

# Old curl callback-abort errors must escape ordinary error/retry handlers as
# interrupts, restore options, release the checkpoint lock, and never restart.
for (typed in c(FALSE, TRUE)) {
  local({
    path <- tempfile()
    on.exit(unlink(path, recursive = TRUE))
    old <- options(curl_interrupt = FALSE)
    on.exit(options(old), add = TRUE)
    calls <- 0L
    verb <- function(...) {
      calls <<- calls + 1L
      stopifnot(isTRUE(getOption("curl_interrupt")))
      e <- simpleError(
        "Operation was aborted by an application callback",
        quote(curl_fetch_memory(url, handle))
      )
      if (typed) class(e) <- c("curl_error_aborted_by_callback", "curl_error", class(e))
      stop(e)
    }
    mock("VERB",
      verb,
      {
        stopifnot(tryCatch(
          {
            fedstat_download("31074", filters = "*", path = path, verbose = FALSE)
            FALSE
          },
          interrupt = function(e) TRUE,
          error = function(e) stop("Cancellation became an error")
        ))
      },
      where = asNamespace("httr")
    )
    events <- lapply(readLines(file.path(path, "download.log")), jsonlite::fromJSON)
    types <- vapply(events, `[[`, "", "event")
    stopifnot(
      calls == 1L, identical(getOption("curl_interrupt"), FALSE),
      !dir.exists(file.path(path, ".lock")), tail(types, 1L) == "INTERRUPTED",
      !any(types %in% c("REQUEST_ERROR", "PART_ERROR", "RETRY", "RECOVER", "STOP"))
    )
  })
}

# Time allowance and partition size are independent; console output names both.
local({
  output <- character()
  for (cells in c(1L, 4L)) {
    path <- tempfile()
    with_batch(withCallingHandlers(
      {
        job <- fedstat_download("31074",
          filters = "*", path = path, max_nominal_cells_request = cells,
          request_pause = 0
        )
        stopifnot(nrow(job$parts) == if (cells == 1L) 4L else 1L)
      },
      message = function(m) {
        output <<- c(output, conditionMessage(m))
        invokeRestart("muffleMessage")
      }
    ))
    events <- lapply(readLines(file.path(path, "download.log")), jsonlite::fromJSON)
    stopifnot(events[[1L]]$max_nominal_cells_request == cells, events[[1L]]$time_limit_seconds == 3600)
    unlink(path, recursive = TRUE)
  }
  stopifnot(
    any(grepl("==> Downloading indicator", output, fixed = TRUE)),
    !any(grepl("HTTP|nominal|REQUEST|token", output))
  )
})
cat("Cancellation, byte-sniffing and progress regression tests passed.\n")

# Time is the default allowance.
stopifnot(
  formals(fedstat_download)$time_limit_seconds == 3600
)
for (value in list(0, NA, Inf, "one hour")) {
  expect_error(fedstat_download("31074", path = tempfile(), time_limit_seconds = value))
}
local({
  path <- tempfile()
  on.exit(unlink(path, recursive = TRUE))
  clock <- 0
  posts <- 0L
  with_batch(mock("fedstat_elapsed", function() clock, {
    mock("fedstat_post_data_ids_filtered", function(data_ids, ...) {
      attr(data_ids, "fedstat_session")$before()
      posts <<- posts + 1L
      clock <<- clock + 2
      serialize(fake_data(data_ids), NULL)
    }, expect_error(fedstat_download("31074",
      filters = "*", path = path,
      max_nominal_cells_request = 1, time_limit_seconds = 1, request_pause = 0,
      verbose = FALSE
    ), "fedstat_time_limit"))
    m <- readRDS(file.path(path, "manifest.rds"))
    stopifnot(
      posts == 1L, sum(vapply(m$parts, function(p) p$status == "complete", logical(1))) == 1L,
      !dir.exists(file.path(path, ".lock"))
    )
    events <- lapply(readLines(file.path(path, "download.log")), jsonlite::fromJSON)
    stopifnot(
      tail(events, 1)[[1]]$error_class == "fedstat_time_limit",
      !any(vapply(events, function(e) e$event %in% c("RETRY", "RECOVER"), logical(1)))
    )
    # A fresh invocation gets a fresh hour and skips the saved part.
    stopifnot(
      fedstat_download("31074",
        filters = "*", path = path,
        max_nominal_cells_request = 1, request_pause = 0, verbose = FALSE
      )$complete,
      nrow(fedstat_collect(path)) == 4L
    )
  }))
})

# Do not shorten pacing/Retry-After to squeeze another request into the allowance.
local({
  path <- tempfile()
  on.exit(unlink(path, recursive = TRUE))
  with_batch(mock(
    "fedstat_elapsed", function() 0,
    mock("Sys.sleep", function(...) stop("Must not sleep"),
      expect_error(
        fedstat_download("31074",
          filters = "*", path = path,
          time_limit_seconds = 2, request_pause = 10, verbose = FALSE
        ),
        "fedstat_time_limit", "next required pause"
      ),
      where = baseenv()
    )
  ))
  events <- lapply(readLines(file.path(path, "download.log")), jsonlite::fromJSON)
  stopifnot(tail(events, 1)[[1]]$request == 1L)
})

# Remaining time caps every real HTTP request, including a user's larger timeout.
local({
  calls <- 0L
  before <- function() NULL
  attr(before, "time_left") <- function() 0.25
  attr(before, "time_limit_seconds") <- 1
  verb <- function(method, url, ..., handle) {
    calls <<- calls + 1L
    configs <- Filter(function(x) inherits(x, "request"), list(...))
    config <- do.call(c, configs)
    stopifnot(config$options$timeout_ms == 250)
    stop(simpleError("Timeout was reached", quote(curl_fetch_memory(url, handle))))
  }
  mock("VERB", verb, expect_error(internal("fedstat_request")(
    "GET", "http://example.invalid", httr::timeout(999),
    handle = httr::handle("http://example.invalid"), timeout_seconds = 300,
    attempts = 3, stage = "token", before = before), "fedstat_time_limit"),
  where = asNamespace("httr")
  )
  stopifnot(calls == 1L)
})
cat("Time allowance and argument compatibility regression tests passed.\n")

# A caller's shorter transfer timeout is preserved under the overall allowance.
local({
  before <- function() NULL
  attr(before, "time_left") <- function() 10
  mock("VERB", function(method, url, ..., handle) {
    configs <- Filter(function(x) inherits(x, "request"), list(...))
    stopifnot(do.call(c, configs)$options$timeout_ms == 500)
    response(xml, type = "text/xml")
  }, internal("fedstat_request")("GET", "http://example.invalid", httr::timeout(0.5),
    handle = httr::handle("http://example.invalid"), timeout_seconds = 300,
    stage = "metadata", before = before), where = asNamespace("httr"))
  invisible(NULL)
})

# A sub-millisecond remainder expires cleanly instead of becoming an invalid timeout.
local({
  path <- tempfile()
  on.exit(unlink(path, recursive = TRUE))
  mock(
    "fedstat_elapsed", function() 0,
    expect_error(fedstat_download("31074",
      path = path, time_limit_seconds = 0.0001,
      verbose = FALSE
    ), "fedstat_time_limit")
  )
})

# HTML detection ignores script/style/comment/hidden-dialog vocabulary.
for (markup in c(
  '<html><script>var captcha = "access denied";</script><body>Indicator</body></html>',
  "<html><style>.captcha{}</style><!-- forbidden --><body>Indicator</body></html>",
  '<html><body><div style="display: none">captcha</div>Indicator</body></html>'
)) {
  stopifnot(is.null(internal("fedstat_access_marker")(charToRaw(markup))))
  expect_error(internal("fedstat_export_content")(response(markup), "sdmx"), "fedstat_unexpected_response")
}
for (markup in c(
  "<html><body>Verify you are human</body></html>",
  '<html><body><input name="captcha" value="secret"></body></html>'
)) {
  e <- expect_error(internal("fedstat_response")(response(markup), "token"), "fedstat_blocked")
  stopifnot(!is.null(e$evidence), !grepl("secret", conditionMessage(e)))
}

# An access check cools down before refreshing the token; no subdivision.
local({
  path <- tempfile()
  on.exit(unlink(path, recursive = TRUE))
  gets <- posts <- 0L
  waits <- numeric()
  notices <- character()
  mock("VERB", function(method, url, ..., handle) {
    if (method == "GET") {
      gets <<- gets + 1L
      return(response(html))
    }
    posts <<- posts + 1L
    if (posts == 1L) {
      return(response("<html><body>Access denied</body></html>"))
    }
    stopifnot(gets == 2L, max(waits) >= 300)
    response(xml, type = "text/xml")
  }, mock("Sys.sleep",
    function(time) waits <<- c(waits, time),
    {
      withCallingHandlers(
        {
          job <- fedstat_download("31074", filters = "*", path = path, request_pause = 0, verbose = FALSE)
          stopifnot(job$complete, nrow(job$parts) == 1L)
        },
        warning = function(w) {
          notices <<- c(notices, conditionMessage(w))
          invokeRestart("muffleWarning")
        }
      )
    },
    where = baseenv()
  ), where = asNamespace("httr"))
  stopifnot(posts == 2L, gets == 2L, length(notices) == 1L, grepl("5 minutes", notices))
  events <- lapply(readLines(file.path(path, "download.log")), jsonlite::fromJSON)
  stopifnot(any(vapply(events, function(e) identical(e$evidence, "access_refusal_text"), logical(1))))
})

# Short console mode, detailed file logs, immediate warnings and caller-owned options.
for (mode in list(TRUE, FALSE, "detailed")) {
  local({
    path <- tempfile()
    on.exit(unlink(path, recursive = TRUE))
    output <- notices <- character()
    original_options <- options()
    with_batch(withCallingHandlers(
      fedstat_download("31074",
        filters = "*", path = path,
        max_nominal_cells_request = 250001, request_pause = 0, verbose = mode
      ),
      message = function(m) {
        output <<- c(output, conditionMessage(m))
        invokeRestart("muffleMessage")
      },
      warning = function(w) {
        notices <<- c(notices, conditionMessage(w))
        invokeRestart("muffleWarning")
      }
    ))
    stopifnot(
      length(notices) == 1L, grepl("100,000", notices), identical(options(), original_options),
      !any(grepl("\\r|\\033", output))
    )
    if (identical(mode, FALSE)) stopifnot(!length(output))
    if (identical(mode, TRUE)) stopifnot(any(grepl("Ready:", output)), !any(grepl("HTTP|START|SPLIT", output)))
    if (identical(mode, "detailed")) stopifnot(any(grepl("START", output)))
    events <- lapply(readLines(file.path(path, "download.log")), jsonlite::fromJSON)
    stopifnot(any(vapply(events, function(e) e$event == "WARNING", logical(1))))
    # The log connection and lock must be released between calls in the same R session.
    with_batch(fedstat_download("31074", filters = "*", path = path, verbose = FALSE))
  })
}

# Cleanup is opt-in, validates first, preserves unrelated files and records deletion.
for (inline in c(TRUE, FALSE)) {
  local({
    path <- tempfile()
    on.exit(unlink(path, recursive = TRUE))
    with_batch({
      job <- fedstat_download("31074", filters = "*", path = path, request_pause = 0, verbose = FALSE)
      writeLines("keep", file.path(path, "notes.txt"))
      data <- fedstat_collect(job, remove_parts = inline)
      if (!inline) {
        stopifnot(length(list.files(file.path(path, "parts"))) == 1L)
        saveRDS(data, file.path(path, "result.rds"))
        fedstat_cleanup(job)
      }
      stopifnot(
        nrow(data) == 4L, !length(list.files(file.path(path, "parts"))),
        file.exists(file.path(path, "notes.txt")), file.exists(file.path(path, "download.log")),
        isTRUE(readRDS(file.path(path, "manifest.rds"))$cleaned), !dir.exists(file.path(path, ".lock"))
      )
      fedstat_cleanup(job)
      expect_error(fedstat_collect(job), pattern = "already been removed")
      expect_error(fedstat_download("31074", filters = "*", path = path, verbose = FALSE), pattern = "removed after collection")
    })
  })
}
local({
  path <- tempfile()
  on.exit(unlink(path, recursive = TRUE))
  with_batch({
    expire_after_part(expect_error(fedstat_download("31074",
      filters = "*", path = path,
      max_nominal_cells_request = 2, request_pause = 0, verbose = FALSE
    ), "fedstat_time_limit"))
    expect_error(fedstat_cleanup(path), pattern = "incomplete")
    expect_error(fedstat_collect(path, remove_parts = TRUE), pattern = "incomplete")
    job <- fedstat_download("31074", filters = "*", path = path, request_pause = 0, verbose = FALSE)
    dir.create(file.path(path, ".lock"))
    expect_error(fedstat_cleanup(job), pattern = "in use")
    unlink(file.path(path, ".lock"), recursive = TRUE)
    manifest <- readRDS(file.path(path, "manifest.rds"))
    i <- which(vapply(manifest$parts, function(p) p$status == "complete", logical(1)))[1]
    part <- file.path(path, manifest$parts[[i]]$file)
    writeLines("damaged", part)
    expect_error(fedstat_cleanup(job), pattern = "missing or changed")
    expect_error(fedstat_collect(job, remove_parts = TRUE), pattern = "missing or changed")
    stopifnot(file.exists(part), !isTRUE(readRDS(file.path(path, "manifest.rds"))$cleaned))
  })
})
cat("Console, access cooldown and cleanup regression tests passed.\n")

# Removed development names fail early rather than silently entering httr's dots.
expect_error(fedstat_download("31074", path = tempfile(), max_cells = 250000), pattern = "Use max_nominal_cells_request")
expect_error(fedstat_download("31074", path = tempfile(), max_combinations = 250000), pattern = "Use max_nominal_cells_request")

# The advisory is strictly above 250,000 and has normal R warning semantics.
local({
  path <- tempfile()
  on.exit(unlink(path, recursive = TRUE))
  with_batch(withCallingHandlers(
    fedstat_download("31074",
      filters = "*", path = path,
      max_nominal_cells_request = 250000, request_pause = 0, verbose = FALSE
    ),
    warning = function(w) stop("Unexpected size warning")
  ))
})

# Access cooldowns stop at the deadline, and interruption during a cooldown is safe.
for (cancel in c(FALSE, TRUE)) {
  local({
    path <- tempfile()
    on.exit(unlink(path, recursive = TRUE))
    calls <- 0L
    with_batch(mock("fedstat_post_data_ids_filtered", function(data_ids, ...) {
      attr(data_ids, "fedstat_session")$before()
      calls <<- calls + 1L
      internal("fedstat_error")("Access refused", "blocked", "export", 403L)
    }, mock("Sys.sleep",
      function(time) {
        if (time > 0) stop(structure(list(message = "Cancelled", call = NULL), class = c("interrupt", "condition")))
      },
      {
        run <- function() {
          suppressWarnings(fedstat_download("31074",
            filters = "*", path = path,
            request_pause = 0, time_limit_seconds = if (cancel) 3600 else 1, verbose = FALSE
          ))
        }
        if (cancel) {
          stopifnot(tryCatch(
            {
              run()
              FALSE
            },
            interrupt = function(e) TRUE
          ))
        } else {
          expect_error(run(), "fedstat_time_limit")
        }
        stopifnot(calls == 1L, !dir.exists(file.path(path, ".lock")))
      },
      where = baseenv()
    )))
  })
}

# Cleanup handles the original flat layout and rejects paths outside the job.
local({
  path <- tempfile()
  outside <- tempfile()
  on.exit(unlink(c(path, outside), recursive = TRUE))
  with_batch({
    job <- fedstat_download("31074", filters = "*", path = path, request_pause = 0, verbose = FALSE)
    manifest <- readRDS(file.path(path, "manifest.rds"))
    part <- manifest$parts[[1L]]
    stopifnot(file.copy(file.path(path, part$file), outside))
    unsafe <- manifest
    unsafe$parts[[1L]]$file <- outside
    expect_error(internal("fedstat_remove_parts")(unsafe, normalizePath(path)), pattern = "Unsafe")
    stopifnot(file.exists(outside), !isTRUE(readRDS(file.path(path, "manifest.rds"))$cleaned))
    stopifnot(file.rename(file.path(path, part$file), file.path(path, basename(part$file))))
    manifest$parts[[1L]]$file <- basename(part$file)
    manifest$parts_dir <- NULL
    saveRDS(manifest, file.path(path, "manifest.rds"))
    stopifnot(
      nrow(fedstat_collect(job, remove_parts = TRUE)) == 4L,
      !file.exists(file.path(path, basename(part$file))), file.exists(outside)
    )
  })
})
