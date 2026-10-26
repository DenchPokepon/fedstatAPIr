# Optional browser-preview preparation, never a replacement for the SDMX export.
fedstat_prepare_grid <- function(data_ids, session, ..., timeout_seconds, before) {
    response <- fedstat_request("POST",
        paste0(session$base_url, "/indicator/dataGrid.do?id=", session$indicator_id),
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
    if (!is.list(preview) || !"results" %in% names(preview) || !is.list(preview$results)) {
        fedstat_error(
            "Fedstat did not return the expected table preview; no export was sent.",
            "unexpected_response", "grid", response$status_code
        )
    }
    fedstat_event(before, "GRID_READY", "Preview received; requesting the SDMX export.",
        stage = "grid", preview_rows = length(preview$results)
    )
    invisible(length(preview$results) == 0L && identical(as.character(preview[["__count"]]), "0"))
}
