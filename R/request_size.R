# Fedstat evaluates every combination of the selected values, including empty
# ones, so request size is counted in combinations, not rows. These limits
# protect Fedstat and its other users; the hard limit cannot be overridden.
fedstat_size_limits <- c(warn = 50000, allow = 100000, max = 250000)

fedstat_size_reason <- paste(
    "Fedstat checks every combination of the selected values, even empty ones,",
    "so a request this large runs for minutes, often fails and slows Fedstat down",
    "for everyone else. Small parts get the same data more reliably."
)

fedstat_state <- new.env(parent = emptyenv())

fedstat_count <- function(x) format(x, big.mark = ",", scientific = FALSE, trim = TRUE)

fedstat_flag <- function(x, name) {
    if (!is.logical(x) || length(x) != 1L || is.na(x)) stop(name, " must be TRUE or FALSE.", call. = FALSE)
}

fedstat_nominal_cells <- function(data_ids) prod(as.numeric(table(data_ids$filter_field_id)))

fedstat_download_advice <- function(indicator_id) {
    paste0(
        "Use fedstat_download(): it downloads in small parts, pauses between ",
        "requests and continues after interruptions:\n",
        "  job <- fedstat_download(\"", indicator_id, "\", filters = <your filters>)\n",
        "  data <- fedstat_collect(job)"
    )
}

# Stops above the limits; returns TRUE when the request deserves a warning.
fedstat_check_size <- function(cells, allow_large_requests, advice) {
    count <- fedstat_count(cells)
    if (cells > fedstat_size_limits[["max"]]) {
        fedstat_error(paste0(
            "This request has ", count, " filter combinations. Requests to Fedstat ",
            "are limited to 250,000 combinations. ", fedstat_size_reason, "\n", advice
        ), "too_large", "validation")
    }
    if (cells > fedstat_size_limits[["allow"]] && !allow_large_requests) {
        fedstat_error(paste0(
            "This request has ", count, " filter combinations, over the limit of 100,000. ",
            fedstat_size_reason, "\n", advice, "\n",
            "If you really need one request, allow_large_requests = TRUE raises the limit to 250,000."
        ), "too_large", "validation")
    }
    cells > fedstat_size_limits[["warn"]]
}

# fedstat_download() checks its part size before any request; it warns later,
# once its log is open. Returns TRUE when a warning is due.
fedstat_check_part_size <- function(max_nominal_cells_request, allow_large_requests) {
    if (max_nominal_cells_request > fedstat_size_limits[["max"]]) {
        stop(
            "max_nominal_cells_request can be at most 250,000. ", fedstat_size_reason,
            call. = FALSE
        )
    }
    if (max_nominal_cells_request > fedstat_size_limits[["allow"]] && !allow_large_requests) {
        stop(
            "max_nominal_cells_request above 100,000 needs allow_large_requests = TRUE ",
            "(up to 250,000). ", fedstat_size_reason,
            call. = FALSE
        )
    }
    max_nominal_cells_request > fedstat_size_limits[["warn"]] && !allow_large_requests
}

fedstat_part_size_warning <- paste(
    "max_nominal_cells_request is above 50,000: each request is heavier for Fedstat",
    "and more likely to fail. The default of 50,000 is usually best.",
    "Set allow_large_requests = TRUE to turn off this warning."
)

# Same selection, same folder: running the same call again resumes. Equivalent
# spellings of the filters (case, spacing, order) share a folder.
fedstat_default_cache_dir <- function(indicator_id, filters) {
    key <- if (identical(filters, "*")) {
        "*"
    } else {
        paste(names(filters), vapply(filters, paste, "", collapse = "\037"), sep = "\036", collapse = "\035")
    }
    file <- tempfile()
    on.exit(unlink(file))
    writeBin(charToRaw(enc2utf8(key)), file)
    paste0(
        "fedstat-", gsub("[^A-Za-z0-9_-]", "_", indicator_id), "-",
        substr(unname(tools::md5sum(file)), 1L, 8L)
    )
}
