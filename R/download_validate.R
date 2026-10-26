fedstat_key_columns <- function(columns) {
    labels <- sub("_code$", "", grep("_code$", columns, value = TRUE))
    sort(setdiff(columns, c("ObsValue", labels)), method = "radix")
}

# Length-prefix values: delimiters, newlines, Unicode, NA and literal "NA" stay
# distinct. Bucketing is only an index; duplicates are compared using full keys.
fedstat_keys <- function(data, columns) {
    keys <- rep("", nrow(data))
    for (column in columns) {
        value <- if (column %in% names(data)) enc2utf8(as.character(data[[column]])) else rep(NA_character_, nrow(data))
        encoded <- ifelse(is.na(value), "N", paste0("S", nchar(value, type = "bytes"), ":", value))
        keys <- paste0(keys, encoded)
    }
    keys
}

fedstat_validate_parts <- function(manifest, path, report = function(...) NULL) {
    parts <- Filter(function(p) p$status == "complete", manifest$parts)
    if (!length(parts)) stop("No completed parts to validate.", call. = FALSE)
    report("VALIDATE", "Checking saved-part integrity and observation keys; no network requests.")
    fedstat_check_parts(manifest, path)
    scratch <- tempfile(".fedstat-validation-", tmpdir = path)
    if (!dir.create(scratch)) stop("Cannot create validation workspace in ", path, call. = FALSE)
    on.exit(unlink(scratch, recursive = TRUE), add = TRUE)
    columns <- character()
    for (part in parts) {
        data <- readRDS(file.path(path, part$file))
        fedstat_check_data(data)
        columns <- union(columns, names(data))
    }
    columns <- fedstat_key_columns(columns)
    # Keep only one data part and one bucket of keys in memory, not the full data.
    # The weighted checksum distributes keys; collisions never imply duplicates.
    buckets <- 64L
    for (part in parts) {
        data <- readRDS(file.path(path, part$file))
        keys <- fedstat_keys(data, columns)
        bucket <- vapply(keys, function(key) {
            bytes <- as.double(utf8ToInt(key))
            as.integer(sum(bytes * seq_along(bytes)) %% buckets) + 1L
        }, integer(1L))
        for (group in unique(bucket)) {
            connection <- file(file.path(scratch, as.character(group)), "ab")
            tryCatch(serialize(list(keys = keys[bucket == group], part = part$id), connection, version = 2),
                finally = close(connection)
            )
        }
        report("VALIDATE_PART", paste0("Indexed part ", part$id, ": ", nrow(data), " rows."), part = part$id)
    }
    rm(data, keys)
    for (bucket_file in list.files(scratch, full.names = TRUE)) {
        connection <- file(bucket_file, "rb")
        chunks <- tryCatch(
            {
                values <- list()
                size <- file.info(bucket_file)$size
                while (seek(connection) < size) values[[length(values) + 1L]] <- unserialize(connection)
                values
            },
            finally = close(connection)
        )
        keys <- unlist(lapply(chunks, `[[`, "keys"), use.names = FALSE)
        duplicate <- anyDuplicated(keys)
        if (duplicate) {
            first <- match(keys[duplicate], keys)
            owner <- rep(vapply(chunks, `[[`, "", "part"), lengths(lapply(chunks, `[[`, "keys")))
            stop("Duplicate observation keys across parts ", owner[first], " and ", owner[duplicate],
                ". Download is not marked complete; inspect these files before resuming.",
                call. = FALSE
            )
        }
    }
    report("VALIDATED", "Observation keys are unique across all saved parts.")
    invisible(TRUE)
}
