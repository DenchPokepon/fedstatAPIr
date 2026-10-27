# Browser-preview preparation and explicit empty-selection confirmation.
fedstat_prepare_before_download <- function(data_ids, session, ..., timeout_seconds, before) {
    response <- fedstat_request("POST",
        paste0(session$base_url, "/indicator/dataGrid.do?id=", utils::URLencode(session$indicator_id, reserved = TRUE)),
        httr::content_type("application/x-www-form-urlencoded"),
        httr::add_headers("X-Requested-With" = "XMLHttpRequest"), ...,
        body = fedstat_form(data_ids, session, "sdmx", grid = TRUE),
        handle = session$handle, timeout_seconds = timeout_seconds,
        stage = "grid", before = before
    )
    # A successful HTTP status alone can also carry a login/error page.
    preview <- tryCatch(
        jsonlite::fromJSON(rawToChar(response$content), simplifyVector = FALSE),
        error = function(e) NULL
    )
    # simplifyVector=FALSE preserves [] as list() and null as NULL. Require
    # a JSON array, not an error object that happens to contain a zero count.
    results <- if (is.list(preview)) preview[["results"]] else NULL
    count <- if (is.list(preview)) preview[["__count"]] else NULL
    valid_results <- is.list(results) && is.null(names(results))
    zero_count <- (is.numeric(count) && length(count) == 1L && !is.na(count) && count == 0) || identical(count, "0")
    empty_shape <- identical(sort(names(preview)), sort(c("results", "__count"))) &&
        grepl('"results"[[:space:]]*:[[:space:]]*\\[', rawToChar(response$content))
    if (!is.list(preview) || !valid_results ||
        (!length(results) && (!zero_count || !empty_shape)) || (length(results) && zero_count)) {
        fedstat_error(
            "Fedstat returned an invalid or inconsistent preview; absence of observations was not confirmed.",
            "unexpected_response", "grid", response$status_code
        )
    }
    fedstat_event(before, "GRID_READY", "Preview received.",
        stage = "grid", preview_rows = length(preview$results)
    )
    invisible(!length(results) && zero_count)
}
