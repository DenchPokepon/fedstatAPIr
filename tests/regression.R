# Offline regression tests. No test framework and no live Fedstat traffic.
library(fedstatAPIr)
ns <- asNamespace("fedstatAPIr")
internal <- function(name) get(name, ns)
expect_error <- function(expr, class = "error", pattern = NULL) {
  e <- tryCatch({ force(expr); NULL }, error = identity)
  stopifnot(inherits(e, class))
  if (!is.null(pattern)) stopifnot(grepl(pattern, conditionMessage(e)))
  invisible(e)
}
mock <- function(name, value, code, where = ns) {
  ns <- where
  old <- get(name, ns)
  unlockBinding(name, ns); assign(name, value, ns); lockBinding(name, ns)
  on.exit({ unlockBinding(name, ns); assign(name, old, ns); lockBinding(name, ns) })
  force(code)
}
fixture <- function(name) {
  path <- file.path("fixtures", name)
  if (!file.exists(path)) path <- file.path("tests", path)
  path
}
response <- function(content, status = 200L, type = "text/html") {
  structure(list(status_code = status, headers = list("content-type" = type),
                 content = if (is.raw(content)) content else charToRaw(content)), class = "response")
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

# Same public parser output, preserved codes, correct numeric failures, dictionary fast path.
data <- fedstat_parse_sdmx_to_table(xml)
stopifnot(nrow(data) == 2L, identical(data$AREA_code, c("001", "001")),
          identical(data$ObsValue, c(123.4, 125)))
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
stopifnot(identical(real$s_OKATO_code, rep("05000000000", 2L)),
          identical(real$ObsValue, c("211765", "312597,6")),
          identical(real$Time, c("2010", "2011")))
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
stopifnot(grepl("one%26two%3Dthree", body, fixed = TRUE),
          lengths(regmatches(body, gregexpr("selectedFilterIds=", body, fixed = TRUE))) == nrow(ids))

# HTTP configuration reaches BOTH fallback token GET and export POST.
local({
  old <- options(httr_config = httr::add_headers("User-Agent" = "global-agent"),
                 fedstatAPIr.config = httr::config(connecttimeout = 19))
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
    timeout_seconds = 2, retry_max_times = 1), where = asNamespace("httr"))
  stopifnot(length(captured) == 2L)
  for (request in captured) stopifnot(request$options$useragent == "per-call-agent",
                                    request$headers[["User-Agent"]] == "per-call-agent",
                                    request$options$connecttimeout == 7,
                                    request$options$timeout_ms == 2000,
                                    !request$options$followlocation)
})
stopifnot(internal("fedstat_retry_after")("60") == 60,
          is.null(internal("fedstat_retry_after")("invalid")))

# Older curl versions signal simpleError, with the originating HTTP call attached.
mock("VERB", function(...) stop(simpleError("Failure when receiving data from the peer",
                                            call = quote(curl_fetch_memory(url, handle)))),
  expect_error(internal("fedstat_request")("GET", "http://example.invalid",
    handle = httr::handle("http://example.invalid"), timeout_seconds = 1,
    stage = "metadata"), "fedstat_transient"), where = asNamespace("httr"))

# Retry recovery performs one metadata read, refreshes the token, and preserves selection.
gets <- exports <- refreshes <- 0L
mock("fedstat_get_data_ids", function(...) { gets <<- gets + 1L; ids },
  mock("fedstat_post_data_ids_filtered", function(data_ids, ...) {
    exports <<- exports + 1L
    if (exports == 1L) internal("fedstat_error")("expired", "token", "export")
    stopifnot(is.null(attr(data_ids, "fedstat_session")$token))
    xml
  }, mock("fedstat_backoff", function(...) 0,
    stopifnot(nrow(fedstat_data_load_with_filters("31074", filters = list(Year = "*", Area = "*"),
                                                 loading_steps_verbose = FALSE)) == 2L))))
stopifnot(gets == 1L, exports == 2L)

cat("HTTP and parser regression tests passed.\n")
