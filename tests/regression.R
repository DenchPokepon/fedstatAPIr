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

# Older direct-export fixtures isolate their existing recovery policy; the actual
# preview transport and empty-selection fallback are exercised below.
real_preview <- internal("fedstat_preview")
unlockBinding("fedstat_preview", ns)
assign("fedstat_preview", function(...) FALSE, ns)
lockBinding("fedstat_preview", ns)

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
        "fedstat_backoff",
        function(...) 0,
        {
            # A small selection gets a friendly deprecation message with a
            # copy-paste replacement; the wrapper otherwise behaves as before.
            assign("deprecation_shown", NULL, envir = internal("fedstat_state"))
            deprecation <- NULL
            data <- withCallingHandlers(
                fedstat_data_load_with_filters("31074",
                    filters = list(Year = "*", Area = "*"),
                    loading_steps_verbose = FALSE
                ),
                fedstat_deprecated = function(m) {
                    deprecation <<- m
                    invokeRestart("muffleMessage")
                }
            )
            stopifnot(
                nrow(data) == 2L, inherits(deprecation, "deprecatedMessage"),
                grepl("fedstat_download(\"31074\"", conditionMessage(deprecation), fixed = TRUE)
            )
        }
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
        filters = "*", cache_dir = path, max_nominal_cells_request = 2,
        request_pause = 0, verbose = FALSE, recovery_max_times = 0
    )
    stopifnot(job$complete, nrow(job$parts) == 2L, nrow(fedstat_collect(job)) == 4L)
    mock(
        "fedstat_read_ids", function(...) stop("Must not make a network call"),
        stopifnot(fedstat_download("31074", filters = "*", cache_dir = path, verbose = FALSE, recovery_max_times = 0)$complete)
    )
    expect_error(fedstat_download("31074", filters = list(Year = "2024"), cache_dir = path, verbose = FALSE, recovery_max_times = 0), pattern = "different selection")
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
        filters = "*", cache_dir = path, max_nominal_cells_request = 2,
        request_pause = 0, verbose = FALSE, recovery_max_times = 0
    ), "fedstat_time_limit"))
    m <- readRDS(file.path(path, "manifest.rds"))
    stopifnot(sum(vapply(m$parts, function(p) p$status == "complete", logical(1L))) == 1L)
    stopifnot(fedstat_download("31074",
        filters = "*", cache_dir = path, request_pause = 0,
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
                    filters = "*", cache_dir = path, request_pause = 0,
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
            if (outage || nrow(fake_data(data_ids)) > 2L) internal("fedstat_error")("timeout", c("timeout", "transient"), "export")
            serialize(fake_data(data_ids), NULL)
        }, mock(
            "fedstat_parse_sdmx", function(data_raw, ...) unserialize(data_raw),
            mock("fedstat_backoff", function(...) 0, {
                run <- function() {
                    fedstat_download("31074",
                        filters = "*", cache_dir = path, request_pause = 0,
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
        filters = first, cache_dir = path, max_nominal_cells_request = 2,
        request_pause = 0, verbose = FALSE, recovery_max_times = 0
    )
    mock(
        "fedstat_read_ids", function(...) stop("Unexpected network request"),
        stopifnot(fedstat_download("31074", filters = reordered, cache_dir = path, verbose = FALSE, recovery_max_times = 0)$complete)
    )
    expect_error(fedstat_download("31074",
        filters = list(Year = 2024, Area = "First"), cache_dir = path,
        verbose = FALSE, recovery_max_times = 0
    ), pattern = "different selection")
    # Unvalidated manifests must also recheck upstream definitions before completion.
    m <- readRDS(file.path(path, "manifest.rds"))
    m$validated <- NULL
    saveRDS(m, file.path(path, "manifest.rds"))
    stopifnot(fedstat_download("31074",
        filters = first, cache_dir = path,
        request_pause = 0, verbose = FALSE, recovery_max_times = 0
    )$complete)
})
unlink(path, recursive = TRUE)

# A provider ignoring filters must fail final validation, never report completion.
path <- tempfile()
with_batch(mock("fedstat_parse_sdmx", function(...) data.frame(Year = "2024", Area_code = "1", ObsValue = "1"), {
    expect_error(fedstat_download("31074",
        filters = "*", cache_dir = path, max_nominal_cells_request = 2,
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
                cache_dir = path, max_nominal_cells_request = 1, token_policy = "refresh", request_pause = 0, verbose = FALSE, recovery_max_times = 0
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
        identical(vapply(requests, `[[`, "", "stage"), c("metadata", "export", "token", "export", "metadata")),
        identical(vapply(requests, function(e) as.integer(e$request), integer(1L)), 1:5)
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
        filters = "*", cache_dir = path, retry_max_times = 1,
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
        fedstat_download("31074", filters = "*", cache_dir = path, verbose = FALSE, recovery_max_times = 0)
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
            filters = "*", cache_dir = path, request_pause = 0,
            recovery_pause = 0, recovery_max_times = 1, verbose = FALSE
        )
        stopifnot(job$complete, calls == 4L)
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
                filters = list(Year = "*", Area = "First"), cache_dir = path,
                request_pause = 0, recovery_pause = 0, verbose = FALSE
            )
            stopifnot(job$complete, nrow(fedstat_collect(job)) == 2L, posts == 4L, gets == 5L)
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
            filters = "*", cache_dir = path, request_pause = 0,
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
        filters = "*", cache_dir = path, max_nominal_cells_request = 2,
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
        filters = "*", cache_dir = path,
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
    filters = "*", cache_dir = path,
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
                        filters = "*", cache_dir = path,
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
            filters = "*", cache_dir = path,
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
                        fedstat_download("31074", filters = "*", cache_dir = path, verbose = FALSE)
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
                    filters = "*", cache_dir = path, max_nominal_cells_request = cells,
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
        stopifnot(events[[1L]]$max_nominal_cells_request == cells, events[[1L]]$total_time_limit_seconds == 3600)
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
    formals(fedstat_download)$total_time_limit_seconds == 3600
)
for (value in list(0, NA, Inf, "one hour")) {
    expect_error(fedstat_download("31074", cache_dir = tempfile(), total_time_limit_seconds = value))
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
            filters = "*", cache_dir = path,
            max_nominal_cells_request = 1, total_time_limit_seconds = 1, request_pause = 0,
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
                filters = "*", cache_dir = path,
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
                    filters = "*", cache_dir = path,
                    total_time_limit_seconds = 2, request_pause = 10, verbose = FALSE
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
    attr(before, "total_time_limit_seconds") <- 1
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
            cache_dir = path, total_time_limit_seconds = 0.0001,
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
                    job <- fedstat_download("31074", filters = "*", cache_dir = path, request_pause = 0, verbose = FALSE)
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
    stopifnot(posts == 2L, gets == 3L, length(notices) == 1L, grepl("5 minutes", notices))
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
                filters = "*", cache_dir = path,
                max_nominal_cells_request = 60000, request_pause = 0, verbose = mode
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
            length(notices) == 1L, grepl("allow_large_requests = TRUE", notices), identical(options(), original_options),
            !any(grepl("\r|\033", output))
        )
        if (identical(mode, FALSE)) stopifnot(!length(output))
        if (identical(mode, TRUE)) stopifnot(any(grepl("Ready:", output)), !any(grepl("HTTP|START|SPLIT", output)))
        if (identical(mode, "detailed")) stopifnot(any(grepl("START", output)))
        events <- lapply(readLines(file.path(path, "download.log")), jsonlite::fromJSON)
        stopifnot(any(vapply(events, function(e) e$event == "WARNING", logical(1))))
        # The log connection and lock must be released between calls in the same R session.
        with_batch(fedstat_download("31074", filters = "*", cache_dir = path, verbose = FALSE))
    })
}

# Cleanup is opt-in, validates first, preserves unrelated files and records deletion.
for (inline in c(TRUE, FALSE)) {
    local({
        path <- tempfile()
        on.exit(unlink(path, recursive = TRUE))
        with_batch({
            job <- fedstat_download("31074", filters = "*", cache_dir = path, request_pause = 0, verbose = FALSE)
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
            expect_error(fedstat_download("31074", filters = "*", cache_dir = path, verbose = FALSE), pattern = "removed after collection")
        })
    })
}
local({
    path <- tempfile()
    on.exit(unlink(path, recursive = TRUE))
    with_batch({
        expire_after_part(expect_error(fedstat_download("31074",
            filters = "*", cache_dir = path,
            max_nominal_cells_request = 2, request_pause = 0, verbose = FALSE
        ), "fedstat_time_limit"))
        expect_error(fedstat_cleanup(path), pattern = "incomplete")
        expect_error(fedstat_collect(path, remove_parts = TRUE), pattern = "incomplete")
        job <- fedstat_download("31074", filters = "*", cache_dir = path, request_pause = 0, verbose = FALSE)
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

# Misspelled or removed argument names fail early rather than silently entering httr's dots.
for (name in c("max_cells", "path", "time_limit_seconds", "prepare_grid", "prepare_before_download")) {
    args <- list(indicator_id = "31074", cache_dir = tempfile())
    args[[name]] <- 1
    expect_error(do.call(fedstat_download, args), pattern = paste0("Unknown argument: ", name))
}

# The size warning is strictly above 50,000 and allow_large_requests silences it.
for (setting in list(list(50000, FALSE), list(100000, TRUE), list(250000, TRUE))) {
    local({
        path <- tempfile()
        on.exit(unlink(path, recursive = TRUE))
        with_batch(withCallingHandlers(
            fedstat_download("31074",
                filters = "*", cache_dir = path, max_nominal_cells_request = setting[[1L]],
                allow_large_requests = setting[[2L]], request_pause = 0, verbose = FALSE
            ),
            warning = function(w) stop("Unexpected size warning")
        ))
    })
}

# Part sizes above 100,000 need allow_large_requests; above 250,000 nothing is
# allowed. Both stop before any folder or request is created.
for (setting in list(list(100001, FALSE, "allow_large_requests = TRUE"), list(250001, TRUE, "at most 250,000"))) {
    path <- tempfile()
    mock("fedstat_read_ids", function(...) stop("No request expected"), expect_error(
        fedstat_download("31074",
            filters = "*", cache_dir = path, max_nominal_cells_request = setting[[1L]],
            allow_large_requests = setting[[2L]], verbose = FALSE
        ),
        pattern = setting[[3L]]
    ))
    stopifnot(!file.exists(path))
}
expect_error(fedstat_download("31074", filters = "*", cache_dir = tempfile(), allow_large_requests = NA), pattern = "TRUE or FALSE")

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
                        filters = "*", cache_dir = path,
                        request_pause = 0, total_time_limit_seconds = if (cancel) 3600 else 1, verbose = FALSE
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
        job <- fedstat_download("31074", filters = "*", cache_dir = path, request_pause = 0, verbose = FALSE)
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

# Progress counts disjoint Cartesian regions, so extra splitting and resume do not reset it.
local({
    path <- tempfile()
    on.exit(unlink(path, recursive = TRUE))
    with_batch({
        expire_after_part(expect_error(fedstat_download("31074",
            filters = "*", cache_dir = path, max_nominal_cells_request = 2,
            request_pause = 0, verbose = FALSE
        ), "fedstat_time_limit"))
        m <- readRDS(file.path(path, "manifest.rds"))
        cache <- new.env(parent = emptyenv())
        expected <- internal("fedstat_progress")(m)
        stopifnot(
            expected$percent_selected == 50,
            identical(internal("fedstat_progress")(m, cache), expected), length(ls(cache)) == 1L,
            identical(internal("fedstat_progress")(m, cache), expected)
        )
        fedstat_download("31074", filters = "*", cache_dir = path, request_pause = 0, verbose = FALSE)
        events <- lapply(readLines(file.path(path, "download.log")), jsonlite::fromJSON)
        saved <- Filter(function(e) e$event %in% c("SAVED", "RESUME", "COMPLETE"), events)
        stopifnot(identical(vapply(saved, `[[`, 0, "percent_selected"), c(50, 50, 100, 100)))
    })
    console <- internal("fedstat_console")
    for (stage in c("metadata", "token", "grid", "export")) {
        line <- console(list(event = "RETRY", stage = stage, saved_parts = 1L))
        stopifnot(grepl(switch(stage,
            metadata = "filters",
            token = "session",
            grid = "preview",
            "part"
        ), line))
    }
    line <- console(list(event = "SAVED", saved_parts = 1L, pending_parts = 1L, rows = 2, percent_selected = 99.999))
    stopifnot(grepl("1 part ", line), grepl("99.9%", line, fixed = TRUE), !grepl("100.0%", line, fixed = TRUE))
})

unlockBinding("fedstat_preview", ns)
assign("fedstat_preview", real_preview, ns)
lockBinding("fedstat_preview", ns)

# Preview preparation shares configuration/session, repeats after token repair,
# and sends exactly the selected filters (never the full indicator).
for (prepare in c(FALSE, TRUE)) {
    local({
        path <- tempfile()
        on.exit(unlink(path, recursive = TRUE))
        calls <- handles <- bodies <- list()
        exports <- 0L
        waits <- numeric()
        verb <- function(method, url, ..., handle, body = NULL) {
            stage <- if (method == "GET") "page" else if (grepl("dataGrid", url)) "grid" else "export"
            calls[[length(calls) + 1L]] <<- stage
            handles[[length(handles) + 1L]] <<- handle
            if (method == "POST") bodies[[length(bodies) + 1L]] <<- body
            configs <- Filter(function(x) inherits(x, "request"), list(...))
            config <- do.call(c, configs)
            stopifnot(config$options$connecttimeout == 7, config$options$useragent == "grid-test", config$options$timeout_ms <= 2000)
            if (stage == "page") {
                return(response(html))
            }
            if (stage == "grid") {
                stopifnot(
                    config$headers[["X-Requested-With"]] == "XMLHttpRequest",
                    grepl("lineObjectIds=0", body, fixed = TRUE), !grepl("token|filterObjectIds|title=", body)
                )
                return(response('{"results":[{}],"__count":"1"}', type = "text/json"))
            }
            exports <<- exports + 1L
            if (exports == 1L) {
                return(response("", 302L))
            }
            response(xml, type = "text/xml")
        }
        mock("VERB", verb, mock("Sys.sleep",
            function(time) waits <<- c(waits, time),
            {
                job <- fedstat_download("31074",
                    filters = "*", cache_dir = path,
                    preview_first = prepare, request_pause = 3, timeout_seconds = 2, verbose = FALSE,
                    httr::user_agent("grid-test"), httr::config(connecttimeout = 7)
                )
                stopifnot(job$complete)
            },
            where = baseenv()
        ), where = asNamespace("httr"))
        expected <- if (prepare) c("page", "grid", "export", "page", "grid", "export") else c("page", "export", "grid", "page", "export")
        stopifnot(
            identical(unlist(calls), c(expected, "page")), all(waits >= 3), length(waits) == length(calls) - 1L,
            all(vapply(handles, identical, logical(1L), handles[[1L]]))
        )
        selections <- lapply(bodies, function(body) {
            fields <- strsplit(body, "&", fixed = TRUE)[[1L]]
            sort(fields[startsWith(fields, "selectedFilterIds=")])
        })
        stopifnot(all(vapply(selections, identical, logical(1L), selections[[1L]])))
        # A nonempty preview is followed by a validated SDMX export.
        stopifnot(nrow(fedstat_collect(job)) == 2L)
    })
}

# Preview failures cannot fall through to an export; access/rate limits never split.
for (status in c(403L, 429L, 503L, 200L)) {
    local({
        path <- tempfile()
        on.exit(unlink(path, recursive = TRUE))
        verb <- function(method, url, ...) {
            if (method == "GET") {
                return(response(html))
            }
            stopifnot(grepl("dataGrid", url))
            response("<html>Unavailable</html>", status)
        }
        mock("VERB", verb, mock("Sys.sleep",
            function(...) NULL,
            {
                expect_error(fedstat_download("31074",
                    filters = "*", cache_dir = path,
                    preview_first = TRUE, request_pause = 0, verbose = FALSE,
                    retry_max_times = 1L, max_failures = 1L, recovery_max_times = 0
                ), "fedstat_error")
            },
            where = baseenv()
        ), where = asNamespace("httr"))
        m <- readRDS(file.path(path, "manifest.rds"))
        stopifnot(length(m$parts) == 1L, m$parts[[1]]$status == "pending")
    })
}
expect_error(fedstat_download("31074", cache_dir = tempfile(), preview_first = NA), pattern = "preview_first")
cat("Preview preparation and nominal progress regression tests passed.\n")

# Cancellation or expiry during preview never starts an SDMX request.
for (cancel in c(FALSE, TRUE)) {
    local({
        path <- tempfile()
        on.exit(unlink(path, recursive = TRUE))
        clock <- 0
        calls <- character()
        verb <- function(method, url, ...) {
            calls <<- c(calls, method)
            if (method == "GET") {
                return(response(html))
            }
            stopifnot(grepl("dataGrid", url))
            if (cancel) stop(simpleError("Operation was aborted by an application callback", quote(curl_fetch_memory(url, handle))))
            clock <<- 3601
            response('{"results":[],"__count":0}', type = "application/json")
        }
        mock("fedstat_elapsed", function() clock, mock("VERB",
            verb,
            {
                action <- function() {
                    fedstat_download("31074",
                        filters = "*", cache_dir = path,
                        preview_first = TRUE, request_pause = 0, verbose = FALSE
                    )
                }
                if (cancel) {
                    stopifnot(tryCatch(
                        {
                            action()
                            FALSE
                        },
                        interrupt = function(e) TRUE
                    ))
                } else {
                    expect_error(action(), "fedstat_time_limit")
                }
            },
            where = asNamespace("httr")
        ))
        stopifnot(identical(calls, c("GET", "POST")), !dir.exists(file.path(path, ".lock")))
        events <- lapply(readLines(file.path(path, "download.log")), jsonlite::fromJSON)
        stopifnot(!any(vapply(events, function(e) e$event %in% c("RETRY", "SPLIT", "RECOVER", "SAVED"), logical(1))))
    })
}

# A 302 alone never confirms emptiness. Exact zero previews complete normal
# zero-row shards in either mode, including resume and cleanup of an all-empty job.
for (prepare in c(FALSE, TRUE)) {
    local({
        path <- tempfile()
        on.exit(unlink(path, recursive = TRUE))
        stages <- character()
        verb <- function(method, url, ...) {
            stage <- if (method == "GET") "page" else if (grepl("dataGrid", url)) "grid" else "export"
            stages <<- c(stages, stage)
            if (stage == "page") {
                return(response(html))
            }
            if (stage == "grid") {
                return(response('{"results":[],"__count":0}', type = "text/json"))
            }
            response("", 302L)
        }
        mock("VERB", verb, mock("Sys.sleep",
            function(...) NULL,
            {
                job <- fedstat_download("31074",
                    filters = "*", cache_dir = path,
                    preview_first = prepare, request_pause = 0, verbose = FALSE
                )
                stopifnot(
                    job$complete, nrow(fedstat_collect(job)) == 0L,
                    identical(names(fedstat_collect(job)), "ObsValue")
                )
            },
            where = baseenv()
        ), where = asNamespace("httr"))
        expected <- if (prepare) c("page", "grid", "export") else c("page", "export", "grid")
        # Only the completion check reads the indicator page again; empty parts do not.
        stopifnot(identical(stages, c(expected, "page")))
        m <- readRDS(file.path(path, "manifest.rds"))
        stopifnot(m$parts[[1L]]$empty_evidence == "preview_zero_after_export_302")
        mock(
            "fedstat_read_ids", function(...) stop("No network on completed resume"),
            stopifnot(fedstat_download("31074", filters = "*", cache_dir = path, verbose = FALSE)$complete)
        )
        fedstat_cleanup(path)
        stopifnot(!length(list.files(file.path(path, "parts"))))
    })
}

# Reject nulls, objects, missing/conflicting counts, error envelopes and wrong media.
for (payload in c(
    "null", "0", '{"results":null,"__count":0}',
    '{"results":{},"__count":0}', '{"results":[]}', '{"results":[],"__count":1}',
    '{"results":[{}],"__count":0}', '{"results":[],"__count":0,"error":"failed"}'
)) {
    mock(
        "fedstat_request", function(...) response(payload, type = "application/json"),
        expect_error(real_preview(ids, internal("fedstat_session")(ids),
            timeout_seconds = 1,
            before = function() NULL
        ), "fedstat_unexpected_response")
    )
}

# An interrupted mixed job resumes from the old manifest layout, skips saved
# observations, confirms the remaining empty sector and collects without extra NA rows.
local({
    path <- tempfile()
    on.exit(unlink(path, recursive = TRUE))
    with_batch(expire_after_part(expect_error(fedstat_download("31074",
        filters = "*",
        cache_dir = path, max_nominal_cells_request = 2, request_pause = 0, verbose = FALSE
    ), "fedstat_time_limit")))
    before <- readRDS(file.path(path, "manifest.rds"))
    complete <- Filter(function(p) p$status == "complete", before$parts)[[1L]]
    reference <- readRDS(file.path(path, complete$file))
    attr(reference, "out.attrs") <- NULL
    exports <- 0L
    mock("VERB",
        function(method, url, ...) {
            if (method == "GET") {
                return(response(html))
            }
            if (grepl("dataGrid", url)) {
                return(response('{"__count":"0","results":[]}', type = "text/json"))
            }
            exports <<- exports + 1L
            response("", 302L)
        },
        {
            job <- fedstat_download("31074", filters = "*", cache_dir = path, request_pause = 0, verbose = FALSE)
            stopifnot(job$complete, exports == 1L, identical(fedstat_collect(job, try_to_parse_ObsValue = FALSE), reference))
        },
        where = asNamespace("httr")
    )
})

# Opaque identifiers keep their spelling and are encoded as a single URL component.
stopifnot(
    internal("fedstat_indicator_id")("001-A") == "001-A",
    internal("fedstat_indicator_id")(31074) == "31074"
)
for (invalid in list(NULL, NA, "", " ", c("a", "b"), list("a"), Inf)) {
    expect_error(internal("fedstat_indicator_id")(invalid), pattern = "indicator_id")
}
local({
    code <- "A/01?x=y"
    markup <- html
    mock("fedstat_request", function(method, url, ...) {
        stopifnot(endsWith(url, "A%2F01%3Fx%3Dy"))
        response(markup)
    }, {
        x <- fedstat_get_data_ids(code)
        stopifnot(internal("fedstat_session")(x)$indicator_id == code)
        internal("fedstat_refresh")(internal("fedstat_session")(x), timeout_seconds = 1)
    })
})
cat("Empty selection, cache resume and opaque identifier tests passed.\n")

# Fallback checks obey cancellation, access refusal and time allowance. None can
# silently turn a failed export into an empty successful shard.
for (failure in c("access", "interrupt", "deadline", "invalid")) {
    local({
        path <- tempfile()
        on.exit(unlink(path, recursive = TRUE))
        clock <- 0
        calls <- character()
        verb <- function(method, url, ...) {
            stage <- if (method == "GET") "page" else if (grepl("dataGrid", url)) "grid" else "export"
            calls <<- c(calls, stage)
            if (stage == "page") {
                return(response(html))
            }
            if (stage == "export") {
                if (failure == "deadline") clock <<- 3601
                return(response("", 302L))
            }
            if (failure == "access") {
                return(response("", 403L))
            }
            if (failure == "interrupt") stop(simpleError("Operation was aborted by an application callback", quote(curl_fetch_memory(url, handle))))
            response('{"results":null,"__count":0}', type = "application/json")
        }
        mock("fedstat_elapsed", function() clock, mock("VERB",
            verb,
            {
                action <- function() {
                    fedstat_download("31074",
                        filters = "*", cache_dir = path,
                        request_pause = 0, retry_max_times = 1, recovery_max_times = 0, verbose = FALSE
                    )
                }
                if (failure == "interrupt") {
                    stopifnot(tryCatch(
                        {
                            action()
                            FALSE
                        },
                        interrupt = function(e) TRUE
                    ))
                } else {
                    expect_error(action(), switch(failure,
                        access = "fedstat_blocked",
                        deadline = "fedstat_time_limit",
                        "fedstat_unexpected_response"
                    ))
                }
            },
            where = asNamespace("httr")
        ))
        m <- readRDS(file.path(path, "manifest.rds"))
        stopifnot(length(m$parts) == 1, m$parts[[1]]$status == "pending", !dir.exists(file.path(path, ".lock")))
        stopifnot(identical(calls, if (failure == "deadline") c("page", "export") else c("page", "export", "grid")))
    })
}

# Filter definitions: IDs identify what is requested, titles are labels, and the
# row/column layout is presentation. Page dates and row positions are ignored.
area_line <- "2: {title: 'Area', values: {1: {title: 'First'}, 2: {title: 'Second'}}}"
page <- function(from = area_line, to = area_line, extra = list()) {
    text <- sub(from, to, rawToChar(html), fixed = TRUE)
    for (pair in extra) text <- sub(pair[[1]], pair[[2]], text, fixed = TRUE)
    charToRaw(enc2utf8(text))
}
local({
    frozen <- internal("fedstat_download_selection")(ids, list(Year = "2024", Area = "First"))
    compare <- internal("fedstat_compare_definitions")
    same <- compare(frozen, ids[nrow(ids):1, ])
    stopifnot(
        !length(same$missing), !length(same$renamed), !length(same$added_fields),
        !length(same$removed_fields), identical(same$layout, frozen$filter_field_object_ids)
    )
    # Unselected values may appear or disappear.
    added <- rbind(ids, transform(ids[ids$filter_value_id == "2025", ], filter_value_id = "2026", filter_value_title = "2026"))
    stopifnot(!length(compare(frozen, added)$missing), !length(compare(frozen, ids[ids$filter_value_id != "2025", ])$missing))
    moved <- ids
    moved$filter_field_object_ids[moved$filter_field_id == "1"] <- "lineObjectIds"
    stopifnot(all(compare(frozen, moved)$layout[frozen$filter_field_id == "1"] == "lineObjectIds"))
    renamed <- ids
    renamed$filter_value_title[renamed$filter_value_id == "2024"] <- "2024 (revised)"
    stopifnot(identical(frozen$filter_value_id[compare(frozen, renamed)$renamed], "2024"))
    stopifnot(identical(frozen$filter_value_id[compare(frozen, ids[ids$filter_value_id != "2024", ])$missing], "2024"))
    extra <- rbind(ids, data.frame(
        filter_field_id = "3", filter_field_title = "Sex", filter_value_id = "1",
        filter_value_title = "All", filter_field_object_ids = "lineObjectIds"
    ))
    stopifnot(
        identical(compare(frozen, extra)$added_fields, "Sex"),
        identical(compare(frozen, ids[ids$filter_field_id != "2", ])$removed_fields, "Area")
    )

    # Token renewal applies the download's policy and keeps the old token on failure.
    session <- internal("fedstat_session")(ids)
    token <- session$token
    session$check_definitions <- function(current) internal("fedstat_dictionary_changed")()
    mock(
        "fedstat_request", function(...) response(html),
        expect_error(internal("fedstat_refresh")(session, timeout_seconds = 1), "fedstat_dictionary_changed")
    )
    stopifnot(identical(session$token, token))
    session$check_definitions <- NULL
})

# Export code lists: codes are identity; a renamed code keeps its newest label.
local({
    check <- internal("fedstat_check_dictionary")
    dictionary <- as.data.frame(fedstat_parse_sdmx_to_table(xml, return_type = "dictionary"))
    stopifnot(identical(check(NULL, dictionary), dictionary))
    later <- dictionary
    later$value_id <- "002"
    later$value_title <- "Another area"
    combined <- check(dictionary, later)
    stopifnot(nrow(combined) == 2L, is.null(attr(combined, "renamed")))
    renamed <- dictionary
    renamed$value_title <- "First (new name)"
    renamed$field_title <- "Area, revised"
    result <- check(combined, renamed)
    stopifnot(
        identical(attr(result, "renamed")$new_title, "First (new name)"), nrow(result) == 2L,
        result$value_title[result$value_id == "001"] == "First (new name)"
    )
    other <- dictionary
    other$field_id <- "SEX"
    expect_error(check(combined, other), "fedstat_dictionary_changed")
    # One export contradicting itself is damaged and retryable, not a definition change.
    expect_error(check(NULL, rbind(dictionary, dictionary)), "fedstat_transient")
    unknown <- charToRaw(sub('concept="AREA" value="001"', 'concept="AREA" value="999"', rawToChar(xml), fixed = TRUE))
    expect_error(internal("fedstat_parse_sdmx")(unknown, check_dictionary = function(x) NULL), "fedstat_inconsistent")
})

# Changes seen at completion: a new filter field stops (also on resume); a new
# layout passes silently; a renamed value passes with a warning.
for (scenario in c("field", "layout", "renamed")) {
    local({
        path <- tempfile()
        on.exit(unlink(path, recursive = TRUE))
        pages <- 0L
        later <- switch(scenario,
            field = page(to = paste0(area_line, ",\n3: {title: 'Sex', values: {1: {title: 'All'}}}")),
            layout = page(extra = list(c("left_columns: [2]", "left_columns: [2, 1]"), c("top_columns: [1]", "top_columns: []"))),
            renamed = page(extra = list(c("title: '2024'", "title: '2024 (revised)'")))
        )
        verb <- function(method, url, ...) {
            if (method == "GET") {
                pages <<- pages + 1L
                return(response(if (pages == 1L) html else later))
            }
            response(xml, type = "text/xml")
        }
        warnings <- character()
        run <- function() {
            withCallingHandlers(
                fedstat_download("31074", filters = "*", cache_dir = path, request_pause = 0, verbose = FALSE),
                warning = function(w) {
                    warnings <<- c(warnings, conditionMessage(w))
                    invokeRestart("muffleWarning")
                }
            )
        }
        mock("VERB",
            verb,
            {
                if (scenario == "field") {
                    expect_error(run(), "fedstat_dictionary_changed")
                    m <- readRDS(file.path(path, "manifest.rds"))
                    stopifnot(!m$validated, m$parts[[1]]$status == "complete")
                    expect_error(run(), "fedstat_dictionary_changed")
                    expect_error(fedstat_collect(path), pattern = "incomplete")
                } else {
                    stopifnot(run()$complete)
                    m <- readRDS(file.path(path, "manifest.rds"))
                    if (scenario == "layout") stopifnot(!length(warnings), is.null(m$renamed))
                    if (scenario == "renamed") {
                        stopifnot(
                            length(warnings) == 1L, grepl("2024 -> 2024 (revised)", warnings, fixed = TRUE),
                            identical(m$renamed$new_title, "2024 (revised)")
                        )
                    }
                }
            },
            where = asNamespace("httr")
        )
    })
}

# A selected value disappears mid-download: unfinished parts continue without it.
local({
    path <- tempfile()
    on.exit(unlink(path, recursive = TRUE))
    current <- ids
    warnings <- character()
    run <- function() {
        withCallingHandlers(
            fedstat_download("31074",
                filters = "*", cache_dir = path, max_nominal_cells_request = 1,
                request_pause = 0, verbose = FALSE, recovery_max_times = 0
            ),
            warning = function(w) {
                warnings <<- c(warnings, conditionMessage(w))
                invokeRestart("muffleWarning")
            }
        )
    }
    requested <- list()
    mock(
        "fedstat_read_ids", function(..., before = function() NULL) {
            before()
            current
        },
        mock("fedstat_post_data_ids_filtered", function(data_ids, ...) {
            attr(data_ids, "fedstat_session")$before()
            requested[[length(requested) + 1L]] <<- data_ids[data_ids$filter_field_id == "2", "filter_value_id"]
            serialize(fake_data(data_ids), NULL)
        }, mock("fedstat_parse_sdmx", function(data_raw, ...) unserialize(data_raw), {
            expire_after_part(expect_error(run(), "fedstat_time_limit"))
            current <- ids[!(ids$filter_field_id == "2" & ids$filter_value_id == "2"), ]
            job <- run()
            stopifnot(job$complete, length(warnings) == 1L, grepl("Area: Second", warnings))
            stopifnot(identical(job$unavailable$value_title, "Second"))
            stopifnot(!any(vapply(requested[-1], function(x) "2" %in% x, logical(1L))))
            m <- readRDS(file.path(path, "manifest.rds"))
            evidence <- vapply(m$parts, function(p) if (is.null(p$empty_evidence)) "" else p$empty_evidence, "")
            stopifnot(sum(evidence == "values_withdrawn") == 1L)
            data <- withCallingHandlers(fedstat_collect(job), message = function(m) {
                stopifnot(grepl("Area: Second", conditionMessage(m)))
                invokeRestart("muffleMessage")
            })
            stopifnot(nrow(data) == 2L, all(data$Area_code == "1"))
            stopifnot(any(grepl("Area: Second", capture.output(print(job)))))
        }))
    )
})
cat("Dictionary drift regression tests passed.\n")

# Fedstat renames a category in both the page and its exports while a download
# is paused, including for checkpoints made before dictionary snapshots existed:
# the job continues and every code gets its newest label in the final table.
local({
    path <- tempfile()
    on.exit(unlink(path, recursive = TRUE))
    exports <- 0L
    renamed <- FALSE
    verb <- function(method, url, ..., body = NULL) {
        if (method == "GET") {
            return(response(if (renamed) page(extra = list(c("title: 'First'", "title: 'Changed area'"))) else html))
        }
        exports <<- exports + 1L
        if (!renamed && exports > 1L) {
            return(response("Unavailable", 503L))
        }
        document <- xml2::read_xml(xml)
        obs <- xml2::xml_find_all(document, "//*[local-name()='Obs']")
        xml2::xml_remove(obs[if (renamed) 1L else 2L])
        if (renamed) xml2::xml_set_text(xml2::xml_find_first(document, "//*[local-name()='Description']"), "Changed area")
        response(charToRaw(as.character(document)), type = "text/xml")
    }
    run <- function() {
        fedstat_download("31074",
            filters = list(Year = "*", Area = "First"), cache_dir = path,
            max_nominal_cells_request = 1, request_pause = 0, retry_max_times = 1,
            recovery_max_times = 0, verbose = FALSE
        )
    }
    mock("VERB",
        verb,
        {
            expect_error(run(), "fedstat_transient")
            m <- readRDS(file.path(path, "manifest.rds"))
            stopifnot(sum(vapply(m$parts, function(p) p$status == "complete", logical(1))) == 1L)
            m$dictionary <- NULL
            saveRDS(m, file.path(path, "manifest.rds"))
            renamed <- TRUE
            warnings <- character()
            job <- withCallingHandlers(run(), warning = function(w) {
                warnings <<- c(warnings, conditionMessage(w))
                invokeRestart("muffleWarning")
            })
            stopifnot(job$complete, length(warnings) == 2L, all(grepl("First -> Changed area", warnings)))
            data <- fedstat_collect(job)
            stopifnot(nrow(data) == 2L, all(data$AREA == "Changed area"), all(data$AREA_code == "001"))
        },
        where = asNamespace("httr")
    )
})

# Completion checks may fail transiently after saving every part. Resume checks
# again without downloading data, then collect/cleanup become available.
local({
    path <- tempfile()
    on.exit(unlink(path, recursive = TRUE))
    pages <- exports <- 0L
    verb <- function(method, url, ...) {
        if (method == "GET") {
            pages <<- pages + 1L
            if (pages == 2L) {
                return(response("Unavailable", 503L))
            }
            return(response(html))
        }
        exports <<- exports + 1L
        response(xml, type = "text/xml")
    }
    run <- function() {
        fedstat_download("31074",
            filters = "*", cache_dir = path,
            request_pause = 0, retry_max_times = 1, verbose = FALSE
        )
    }
    mock("VERB",
        verb,
        {
            expect_error(run(), "fedstat_transient")
            expect_error(fedstat_collect(path), pattern = "incomplete")
            expect_error(fedstat_cleanup(path), pattern = "incomplete")
            stopifnot(run()$complete, exports == 1L, nrow(fedstat_collect(path)) == 2L)
        },
        where = asNamespace("httr")
    )
})
cat("Dictionary checkpoint and final-check recovery tests passed.\n")

# Website IDs (1) and SDMX codes (001) differ: match explicit dimension/category
# names, never numerical equality. A valid but unrequested category is rejected.
local({
    selected <- internal("fedstat_download_selection")(ids, list(Year = "*", Area = "First"))
    parsed <- internal("fedstat_parse_sdmx")(xml, selection = selected)
    stopifnot(nrow(parsed) == 2L)
    stopifnot(is.null(attr(parsed, "unmatched_labels")))
    wrong_part <- internal("fedstat_download_selection")(ids, list(Year = "*", Area = "Second"))
    expect_error(
        internal("fedstat_parse_sdmx")(xml, selection = wrong_part, offered = ids),
        "fedstat_dictionary_changed",
        pattern = "not requested \\(Area: First\\)"
    )
    # Case, spacing, non-breaking spaces and HTML escaping are formatting, not evidence.
    stopifnot(identical(
        internal("fedstat_label_norm")(c("  First  area ", "A &amp; B", "&quot;X&quot;")),
        c("first area", "a & b", "\"x\"")
    ))
    styled <- charToRaw(sub("<structure:Name>Area</structure:Name>", "<structure:Name> AREA </structure:Name>",
        sub(">First<", ">  FIRST <", rawToChar(xml), fixed = TRUE),
        fixed = TRUE
    ))
    stopifnot(nrow(internal("fedstat_parse_sdmx")(styled, selection = selected, offered = ids)) == 2L)
    expect_error(internal("fedstat_parse_sdmx")(styled, selection = wrong_part, offered = ids), "fedstat_dictionary_changed")
    # A label matching no filter value is reported for the log and never stops the download.
    other <- charToRaw(sub(">First<", ">First (revised wording)<", rawToChar(xml), fixed = TRUE))
    for (part in list(selected, wrong_part)) {
        parsed <- internal("fedstat_parse_sdmx")(other, selection = part, offered = ids)
        stopifnot(nrow(parsed) == 2L, identical(attr(parsed, "unmatched_labels")$value_title, "First (revised wording)"))
    }
})

# During a download, unmatched labels are logged once and the part is saved without them as attributes.
local({
    path <- tempfile()
    on.exit(unlink(path, recursive = TRUE))
    other <- charToRaw(sub(">First<", ">First (revised wording)<", rawToChar(xml), fixed = TRUE))
    mock("VERB",
        function(method, url, ...) {
            if (method == "GET") response(html) else response(other, type = "text/xml")
        },
        {
            job <- fedstat_download("31074",
                filters = list(Year = "*", Area = "First"), cache_dir = path,
                request_pause = 0, verbose = FALSE
            )
        },
        where = asNamespace("httr")
    )
    events <- lapply(readLines(file.path(path, "download.log")), jsonlite::fromJSON)
    stopifnot(job$complete, sum(vapply(events, `[[`, "", "event") == "UNMAPPED_LABEL") == 1L)
    m <- readRDS(file.path(path, "manifest.rds"))
    part <- readRDS(file.path(path, m$parts[[1]]$file))
    stopifnot(is.null(attr(part, "unmatched_labels")))
})
cat("Export selection mapping tests passed.\n")

# Size splits use the fewest groups that fit: the CPI shape (119 x 9 x 1105 =
# 1,183,455 cells) becomes 12 parts of at most 99,603, not 16 halves of ~74k.
local({
    shape <- c("0" = 1, "57831" = 119, "57937" = 9, "58273" = 1105, "57956" = 1)
    selection <- do.call(rbind, lapply(names(shape), function(field) {
        data.frame(
            filter_field_id = field, filter_field_title = field,
            filter_value_id = sprintf("%04d", seq_len(shape[[field]])), filter_value_title = "x",
            filter_field_object_ids = "lineObjectIds", stringsAsFactors = FALSE
        )
    }))
    partition <- internal("fedstat_partition")
    check_groups <- function(groups, rows, max_cells) {
        cells <- vapply(groups, function(g) prod(table(selection$filter_field_id[g])), numeric(1L))
        stopifnot(all(cells <= max_cells), sum(cells) == prod(table(selection$filter_field_id[rows])))
        field <- attr(groups, "field")
        values <- unlist(lapply(groups, function(g) selection$filter_value_id[g][selection$filter_field_id[g] == field]))
        stopifnot(!anyDuplicated(values), setequal(values, selection$filter_value_id[rows][selection$filter_field_id[rows] == field]))
        sizes <- lengths(lapply(groups, function(g) which(selection$filter_field_id[g] == field)))
        stopifnot(max(sizes) - min(sizes) <= 1L)
    }
    rows <- seq_len(nrow(selection))
    groups <- partition(selection, rows, 100000)
    stopifnot(length(groups) == 12L, attr(groups, "field") == "58273")
    check_groups(groups, rows, 99603)
    # When one field cannot reach the limit, each value becomes a part to split again.
    groups <- partition(selection, rows, 500)
    stopifnot(length(groups) == 1105L)
    check_groups(groups, rows, 1071)
    stopifnot(length(partition(selection, groups[[1]], 500)) == 3L)
    # Without a limit: halving, the probe after a failed request.
    stopifnot(length(partition(selection, rows)) == 2L)
    # Ties choose the first field ID, so plans are reproducible.
    stopifnot(attr(partition(ids, seq_len(nrow(ids)), 2), "field") == "1")
})

# Failure splits without saved data are rejoined; size splits and splits with saved parts stay.
local({
    part <- function(id, status, reason = NULL) list(id = id, rows = 1L, depth = 0L, status = status, split_reason = reason)
    parts <- list(
        part("1", "split", "size_limit"),
        part("1.1", "split", "timeout"), part("1.1.1", "split", "timeout"),
        part("1.1.1.1", "pending"), part("1.1.1.2", "pending"), part("1.1.2", "pending"),
        part("1.2", "split", "truncated"), part("1.2.1", "complete"), part("1.2.2", "pending"),
        part("1.10", "pending")
    )
    merged <- internal("fedstat_merge_failure_splits")(parts)
    stopifnot(
        identical(attr(merged, "merged"), "1.1"),
        identical(vapply(merged, `[[`, "", "id"), c("1", "1.1", "1.2", "1.2.1", "1.2.2", "1.10")),
        merged[[2]]$status == "pending", is.null(merged[[2]]$split_reason)
    )
})

# Timeouts and gateway timeouts hint at size; refused or dropped connections do not.
local({
    curl_failure <- function(text) {
        tryCatch(mock("VERB", function(...) stop(simpleError(text, quote(curl_fetch_memory(url, handle)))),
            internal("fedstat_request")("GET", "http://example.invalid",
                handle = httr::handle("http://example.invalid"), timeout_seconds = 1, stage = "export"
            ),
            where = asNamespace("httr")
        ), error = identity)
    }
    stopifnot(
        inherits(curl_failure("Timeout was reached"), "fedstat_timeout"),
        inherits(curl_failure("Operation timed out after 300000 milliseconds with 0 bytes received"), "fedstat_timeout"),
        !inherits(curl_failure("Connection timed out after 20001 milliseconds"), "fedstat_timeout"),
        !inherits(curl_failure("Empty reply from server"), "fedstat_timeout"),
        inherits(curl_failure("Empty reply from server"), "fedstat_transient")
    )
    gateway <- tryCatch(internal("fedstat_response")(response("", 504L), "export"), error = identity)
    stopifnot(inherits(gateway, "fedstat_timeout"), inherits(gateway, "fedstat_transient"))
    stopifnot(!inherits(tryCatch(internal("fedstat_response")(response("", 503L), "export"), error = identity), "fedstat_timeout"))
})

# An outage pauses instead of splitting; a later healthy run rejoins earlier timeout splits.
local({
    path <- tempfile()
    on.exit(unlink(path, recursive = TRUE))
    failure <- "outage"
    mock(
        "fedstat_read_ids", function(..., before = function() NULL) {
            before()
            ids
        },
        mock("fedstat_post_data_ids_filtered", function(data_ids, ...) {
            attr(data_ids, "fedstat_session")$before()
            if (failure == "outage") internal("fedstat_error")("Empty reply from server", "transient", "export")
            if (failure == "timeout") internal("fedstat_error")("Timeout was reached", c("timeout", "transient"), "export")
            serialize(fake_data(data_ids), NULL)
        }, mock(
            "fedstat_parse_sdmx", function(data_raw, ...) unserialize(data_raw),
            mock("fedstat_backoff", function(...) 0, {
                run <- function() {
                    fedstat_download("31074",
                        filters = "*", cache_dir = path, request_pause = 0, recovery_pause = 0,
                        max_failures = 20L, verbose = FALSE, recovery_max_times = 1
                    )
                }
                expect_error(run(), "fedstat_transient")
                events <- lapply(readLines(file.path(path, "download.log")), jsonlite::fromJSON)
                types <- vapply(events, `[[`, "", "event")
                stopifnot("RECOVER" %in% types, !"SPLIT" %in% types, length(readRDS(file.path(path, "manifest.rds"))$parts) == 1L)
                failure <- "timeout"
                expect_error(run(), "fedstat_transient")
                m <- readRDS(file.path(path, "manifest.rds"))
                stopifnot(length(m$parts) > 1L, m$parts[[1]]$split_reason == "timeout")
                failure <- "none"
                job <- run()
                events <- lapply(readLines(file.path(path, "download.log")), jsonlite::fromJSON)
                types <- vapply(events, `[[`, "", "event")
                stopifnot(job$complete, identical(job$parts$part, "1"), "MERGE" %in% types, nrow(fedstat_collect(job)) == 4L)
            })
        ))
    )
})
cat("Splitting and outage recovery tests passed.\n")

# Collected values are numbers by default; a non-number stops before any part is removed.
local({
    stopifnot(identical(internal("fedstat_numbers")(c("1,5", " 2 ", "", NA, "3.25")), c(1.5, 2, NA, NA, 3.25)))
    for (marked in c(FALSE, TRUE)) {
        path <- tempfile()
        export <- if (marked) charToRaw(sub('value="123,4"', 'value="..."', rawToChar(xml), fixed = TRUE)) else xml
        mock("VERB", function(method, url, ...) {
            if (method == "GET") response(html) else response(export, type = "text/xml")
        }, job <- fedstat_download("31074", filters = "*", cache_dir = path, request_pause = 0, verbose = FALSE),
        where = asNamespace("httr")
        )
        if (marked) {
            e <- expect_error(fedstat_collect(job, remove_parts = TRUE), pattern = "try_to_parse_ObsValue = FALSE")
            stopifnot(grepl("\"...\"", conditionMessage(e), fixed = TRUE))
            stopifnot(file.exists(file.path(path, "parts", paste0("part-", job$parts$part[1], ".rds"))))
            stopifnot(identical(fedstat_collect(job, try_to_parse_ObsValue = FALSE)$ObsValue, c("...", "125.0")))
        } else {
            stopifnot(identical(fedstat_collect(job)$ObsValue, c(123.4, 125)))
        }
        expect_error(fedstat_collect(job, try_to_parse_ObsValue = NA), pattern = "try_to_parse_ObsValue must be TRUE or FALSE")
        unlink(path, recursive = TRUE)
    }
})
cat("Numeric collection tests passed.\n")

# A folder in use names its owner and explains how to clear a lock left by a crash.
local({
    path <- tempfile()
    on.exit(unlink(path, recursive = TRUE))
    with_batch(job <- fedstat_download("31074", filters = "*", cache_dir = path, request_pause = 0, verbose = FALSE))
    lock <- internal("fedstat_lock")(path, "fedstat_download()")
    calls <- list(
        function() fedstat_download("31074", filters = "*", cache_dir = path, verbose = FALSE),
        function() fedstat_collect(job),
        function() fedstat_cleanup(job)
    )
    for (call in calls) {
        e <- expect_error(call(), pattern = "in use by another R session")
        text <- conditionMessage(e)
        stopifnot(
            grepl("fedstat_download() started", text, fixed = TRUE),
            grepl(paste("process", Sys.getpid()), text, fixed = TRUE),
            grepl(normalizePath(lock, winslash = "/"), text, fixed = TRUE),
            grepl("R crashed or was closed", text, fixed = TRUE)
        )
    }
    # Locks from older versions have no readable owner record.
    writeLines("old format", file.path(lock, "owner"))
    stopifnot(grepl("in use by another R session. Wait", conditionMessage(expect_error(fedstat_collect(job))), fixed = TRUE))
    unlink(lock, recursive = TRUE)
    stopifnot(nrow(fedstat_collect(job)) == 4L)
})
cat("Folder lock message tests passed.\n")

# Every function that requests data shares the size limits. Synthetic selections
# with the given number of values per filter stand in for real filter IDs.
selection_of <- function(...) {
    counts <- c(1, ...)
    data.frame(filter_field_id = rep(as.character(seq_along(counts) - 1L), counts), stringsAsFactors = FALSE)
}
legacy_call <- function(selection, ...) {
    posts <- 0L
    result <- mock("fedstat_get_data_ids", function(...) ids, mock(
        "fedstat_data_ids_filter", function(...) selection,
        mock("fedstat_post_data_ids_filtered", function(...) {
            posts <<- posts + 1L
            xml
        }, tryCatch(fedstat_data_load_with_filters("31074", filters = list(), loading_steps_verbose = FALSE, ...),
            error = identity
        ))
    ))
    list(result = result, posts = posts)
}
local({
    assign("deprecation_shown", NULL, envir = internal("fedstat_state"))
    notices <- list()
    collect <- function(code) {
        withCallingHandlers(code,
            message = function(m) {
                notices[[length(notices) + 1L]] <<- m
                invokeRestart("muffleMessage")
            },
            warning = function(w) {
                notices[[length(notices) + 1L]] <<- w
                invokeRestart("muffleWarning")
            }
        )
    }
    # Up to 50,000 combinations: one friendly message per session, nothing else.
    for (i in 1:2) stopifnot(is.data.frame(collect(legacy_call(selection_of(200, 250)))$result))
    stopifnot(
        length(notices) == 1L, inherits(notices[[1L]], "deprecatedMessage"), inherits(notices[[1L]], "fedstat_deprecated"),
        grepl("once per session", conditionMessage(notices[[1L]]))
    )
    # Above 50,000: a warning on every call.
    notices <- list()
    for (i in 1:2) stopifnot(is.data.frame(collect(legacy_call(selection_of(300, 200)))$result))
    stopifnot(
        length(notices) == 2L, all(vapply(notices, inherits, logical(1L), "deprecatedWarning")),
        grepl("60,000 filter combinations", conditionMessage(notices[[1L]]), fixed = TRUE)
    )
    # Above 100,000: stopped before the data request unless explicitly allowed.
    call <- collect(legacy_call(selection_of(400, 300)))
    stopifnot(
        inherits(call$result, "fedstat_too_large"), call$posts == 0L,
        grepl("allow_large_requests = TRUE", conditionMessage(call$result), fixed = TRUE),
        grepl("everyone else", conditionMessage(call$result), fixed = TRUE),
        grepl("fedstat_download(\"31074\"", conditionMessage(call$result), fixed = TRUE)
    )
    stopifnot(is.data.frame(collect(legacy_call(selection_of(400, 300), allow_large_requests = TRUE))$result))
    # Above 250,000: always stopped.
    call <- collect(legacy_call(selection_of(520, 500), allow_large_requests = TRUE))
    stopifnot(inherits(call$result, "fedstat_too_large"), call$posts == 0L, grepl("limited to 250,000", conditionMessage(call$result)))
})

# The low-level export applies the same limits before sending anything.
local({
    session <- new.env()
    session$indicator_id <- "31074"
    post <- function(selection, ...) {
        warned <- NULL
        result <- mock("fedstat_session", function(...) session, mock(
            "fedstat_refresh", function(...) stop("sent"),
            withCallingHandlers(tryCatch(fedstat_post_data_ids_filtered(selection, ...), error = identity),
                fedstat_large_request = function(w) {
                    warned <<- w
                    invokeRestart("muffleWarning")
                }
            )
        ))
        list(result = result, warned = warned)
    }
    small <- post(selection_of(200, 250))
    stopifnot(conditionMessage(small$result) == "sent", is.null(small$warned))
    medium <- post(selection_of(300, 200))
    stopifnot(conditionMessage(medium$result) == "sent", grepl("allow_large_requests = TRUE", conditionMessage(medium$warned)))
    stopifnot(is.null(post(selection_of(300, 200), allow_large_requests = TRUE)$warned))
    large <- post(selection_of(400, 300))
    stopifnot(inherits(large$result, "fedstat_too_large"))
    stopifnot(conditionMessage(post(selection_of(400, 300), allow_large_requests = TRUE)$result) == "sent")
    stopifnot(inherits(post(selection_of(520, 500), allow_large_requests = TRUE)$result, "fedstat_too_large"))
})
cat("Request size limit tests passed.\n")

# The main three arguments work positionally; unnamed httr settings go to `...`.
stopifnot(
    identical(names(formals(fedstat_download))[1:5], c("indicator_id", "filters", "cache_dir", "...", "max_nominal_cells_request")),
    "allow_large_requests" %in% names(formals(fedstat_download)),
    formals(fedstat_download)$max_nominal_cells_request == 50000
)

# Without cache_dir, the folder is named after the indicator and the filters, so
# the same call resumes in the same folder; equivalent spellings share it.
local({
    home <- tempfile()
    dir.create(home)
    old <- setwd(home)
    on.exit({
        setwd(old)
        unlink(home, recursive = TRUE)
    })
    name <- internal("fedstat_default_cache_dir")
    normalize <- internal("fedstat_filters")
    stopifnot(
        grepl("^fedstat-31074-[0-9a-f]{8}$", name("31074", "*")),
        identical(
            name("31074", normalize(list(Area = "North", Year = c("2024", "2023")))),
            name("31074", normalize(list(year = c("2023", " 2024"), AREA = "north")))
        ),
        !identical(name("31074", normalize(list(Year = "2024"))), name("31074", normalize(list(Year = "2023")))),
        !identical(name("31074", "*"), name("31075", "*"))
    )
    output <- character()
    keep <- function(m) {
        output <<- c(output, conditionMessage(m))
        invokeRestart("muffleMessage")
    }
    job <- with_batch(withCallingHandlers(fedstat_download("31074", "*", request_pause = 0), message = keep))
    folder <- file.path(normalizePath(home), name("31074", "*"))
    stopifnot(
        isTRUE(job$complete), normalizePath(job$path) == normalizePath(folder),
        file.exists(file.path(folder, "download.log")),
        any(grepl("Saving the download in", output, fixed = TRUE))
    )
    # Running the same call again finds the finished download in the same folder.
    again <- with_batch(withCallingHandlers(fedstat_download("31074", "*", request_pause = 0, verbose = FALSE), message = keep))
    stopifnot(normalizePath(again$path) == normalizePath(folder), nrow(fedstat_collect(again)) == 4L)
})
cat("Default folder and argument order tests passed.\n")

# The page marks the values it shows first; data_ids keep their 1.x columns.
marked <- charToRaw(sub("{title: '2025'}", "{title: '2025', checked: true}", rawToChar(html), fixed = TRUE))
page_ids <- mock("fedstat_request", function(...) response(marked), fedstat_get_data_ids("31074"))
stopifnot(
    identical(names(page_ids), names(ids)),
    identical(attr(page_ids, "fedstat_default_selection"), "1_2025"),
    identical(attr(ids, "fedstat_default_selection"), character())
)
opening <- internal("fedstat_default_selection")(page_ids)
stopifnot(identical(opening$filter_value_title, c("Example indicator", "2025", "First")))
cells <- internal("fedstat_nominal_cells")
stopifnot(
    cells(internal("fedstat_shrink")(ids, 2)) == 2,
    cells(internal("fedstat_random_selection")(ids, 1)) == 1,
    cells(internal("fedstat_random_selection")(ids, 4)) == 4
)

# Filters overview: the printed list is valid R and equals the returned one.
printed <- capture.output(shown <- fedstat_show_filters(page_ids))
stopifnot(
    identical(shown, list(Year = "2025", Area = "First")),
    any(grepl("\"Year\": 2 values", printed, fixed = TRUE))
)
template <- printed[which(printed == "filters <- list("):length(printed)]
stopifnot(identical(eval(parse(text = template)), shown))
everything <- page_ids
attr(everything, "fedstat_default_selection") <- c("1_2024", "1_2025", "2_1", "2_2")
invisible(capture.output(all_shown <- fedstat_show_filters(everything)))
stopifnot(identical(all_shown, list(Year = "*", Area = "*")))
expect_error(fedstat_show_filters(data.frame(x = 1)), pattern = "fedstat_get_data_ids")

# Dictionary: the opening selection first, another sample after a redirect, and
# only the code lists of the export are returned.
dictionary_run <- function(answers, ...) {
    posts <- list()
    result <- mock("fedstat_read_ids", function(...) page_ids, mock("fedstat_post_data_ids_filtered", function(data_ids, ...) {
        posts[[length(posts) + 1L]] <<- data_ids
        answer <- answers[[min(length(posts), length(answers))]]
        if (inherits(answer, "condition")) stop(answer)
        answer
    }, mock("fedstat_backoff", function(...) 0, tryCatch(fedstat_dictionary("31074", ..., request_pause = 0, verbose = FALSE), error = identity))))
    list(result = result, posts = posts)
}
error_of <- function(kind) {
    tryCatch(internal("fedstat_error")(kind, kind, "export", 302L), error = identity)
}
expected <- as.data.frame(fedstat_parse_sdmx_to_table(xml, "dictionary"))
run <- dictionary_run(list(error_of("redirect"), xml))
stopifnot(
    identical(run$result, expected), length(run$posts) == 2L,
    identical(run$posts[[1L]]$filter_value_title, opening$filter_value_title),
    is.null(attr(run$posts[[2L]], "fedstat_session")$token)
)
run <- dictionary_run(list(error_of("redirect")), attempts = 3L)
stopifnot(inherits(run$result, "fedstat_no_data"), length(run$posts) == 3L, grepl("filters = <your filters>", conditionMessage(run$result)))
run <- dictionary_run(list(error_of("transient"), xml), filters = list(Year = "2024", Area = "*"))
stopifnot(
    identical(run$result, expected), length(run$posts) == 2L,
    identical(run$posts[[1L]]$filter_value_title, run$posts[[2L]]$filter_value_title),
    identical(sort(run$posts[[1L]]$filter_value_title), sort(c("Example indicator", "2024", "First", "Second")))
)
run <- dictionary_run(list(error_of("redirect")), filters = list(Year = "2024", Area = "*"))
stopifnot(inherits(run$result, "fedstat_no_data"), length(run$posts) == 1L)
run <- dictionary_run(list(error_of("blocked"), xml))
stopifnot(inherits(run$result, "fedstat_blocked"), length(run$posts) == 1L)
expect_error(fedstat_dictionary("31074", filters = "*"), pattern = "small selection")
expect_error(fedstat_dictionary("31074", "Year"), pattern = "filters must be")
expect_error(fedstat_dictionary("31074", NULL, 5), pattern = "Unknown argument")
cat("Dictionary and filter overview tests passed.\n")
