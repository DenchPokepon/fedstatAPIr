#' Download a large selection in resumable, bounded parts
#'
#' @description Resolves filters once, partitions large selections and saves
#'   validated RDS parts. Requests are sequential. Repeating the same call resumes
#'   saved progress. No optional storage packages are required.
#' @param indicator_id One indicator code.
#' @param ... HTTP configuration passed to metadata GET, token GET and export POST,
#'   for example `httr::user_agent()` or `httr::config(connecttimeout = 20)`.
#' @param filters A named list, as in [fedstat_data_ids_filter()]. Use the single
#'   string `"*"` to explicitly select every value of every dimension.
#' @param path Directory for the manifest and verified parts. Use one directory
#'   per selection. New jobs store data in a `parts/` subdirectory; old flat jobs
#'   resume unchanged. Only one process may write to it at a time.
#' @param max_nominal_cells_request Maximum product of selected dimension cardinalities per
#'   initial request. A conservative size proxy, not an expected row count or a
#'   server guarantee. Default: 100,000. Values above 250,000 warn because their
#'   reliability depends on the indicator and service load. Failed exports can
#'   be subdivided further.
#' @param time_limit_seconds Time allowance for this invocation, in seconds;
#'   defaults to 3600 (one hour). Includes requests, pauses and local processing.
#'   Requests are capped by the remaining time. Local validation and saving finish
#'   safely before stopping, so these operations can extend the elapsed time.
#' @param token_policy `"reuse"` reuses a token until rejection; `"refresh"` gets
#'   a fresh token before each subsequent export. Metadata is reused either way.
#' @param prepare_grid If `TRUE`, submit each part's filters to Fedstat's browser
#'   preview (`dataGrid.do`) before each export attempt, using the same session.
#'   Default `FALSE`: this adds an HTTP request and server work per attempt.
#'   Preview data are discarded; only validated SDMX is saved. Both requests
#'   share pacing, timeout configuration and the invocation's time allowance.
#' @param timeout_seconds Maximum duration of each HTTP request, including transfer.
#' @param retry_max_times Maximum data attempts per part before subdivision or
#'   cooldown, including the first. With `prepare_grid = TRUE`, each attempt
#'   prepares the preview before exporting. Metadata and token GETs have their own attempts;
#'   all attempts share the invocation's time allowance.
#' @param request_pause Seconds to wait between HTTP requests: a number or a
#'   two-number range for random pauses. Retry delays and Retry-After can extend it.
#' @param max_depth Maximum binary subdivisions from the original selection.
#' @param max_failures Pause recovery after this many consecutive transient or
#'   ambiguous export/preview failures; this limits subdivision during an outage.
#' @param verbose Console detail: `TRUE` or `"progress"` for short updates (default),
#'   `FALSE` or `"quiet"` to hide updates, `"detailed"` for diagnostic events.
#'   R warnings and errors remain visible in every mode.
#' @param log_file Append-only UTF-8 JSON Lines diagnostic log. Defaults to
#'   `download.log` inside `path`; `NULL` disables file logging. File logging is
#'   independent of `verbose`. Logs contain request stages, timings, counts and
#'   recovery decisions, never HTTP headers, cookies, tokens or response bodies.
#' @param recovery_max_times Maximum automatic cooldown/recovery cycles without
#'   a successfully saved part. Use zero to stop after ordinary retries.
#' @param access_pause Seconds to wait after a suspected access check or refusal;
#'   defaults to 300 (five minutes). Repeated refusals double this pause and share
#'   `recovery_max_times` and the overall time allowance. No challenge is bypassed.
#' @param recovery_pause Initial cooldown in seconds, doubled on each consecutive
#'   recovery (up to 64 times this value). Request pauses and Retry-After still apply.
#' @return A `fedstat_download` result with directory, completion status and part
#'   summary. Data stay on disk; use [fedstat_collect()] to load them. On failure,
#'   an error is raised and verified progress is retained for the next call.
#' @details Wildcards are frozen when the manifest is created. On resume, added
#'   values do not expand that selection; removed or changed selected values and
#'   changed dimension layouts stop the job. Tokens are never saved. Delete a
#'   stale `.lock` directory only after confirming no process is using the job.
#'
#'   Defaults are deliberately conservative: 100,000 filter combinations, 30--60
#'   seconds between requests, two attempts per part, and one hour per invocation.
#'   Three additional cooldown/recovery cycles are allowed without a
#'   successfully saved part, initially waiting 60 seconds and doubling each time.
#'   An export redirect rejected again after token repair may be subdivided, but
#'   is never accepted as empty data. Token/metadata failures do not cause splitting.
#'   There is no universal safe request size. A timeout may leave the
#'   server computing the original export, so retries also wait.
#'
#'   Completion means all planned parts were received and validated, not that
#'   Fedstat published every possible observation. A long download is not an
#'   atomic snapshot of the upstream database. Parts preserve raw ObsValue strings.
#'   Before marking completion, checksums and observation keys are checked across
#'   all parts using temporary disk buckets. This needs additional free disk space
#'   for keys and memory for a key bucket, but never combines the full dataset.
#'   Completed older manifests are validated once on resume without network access.
#'   Resume tolerates reordered filters/values, case/whitespace differences,
#'   numeric versus character labels and repeated identical values.
#' @export
#' @examples
#' \dontrun{
#' job <- fedstat_download("31074", filters = "*", path = "cpi-download")
#' data <- fedstat_collect(job)
#' }
fedstat_download <- function(indicator_id, ..., filters = list(), path,
                             max_nominal_cells_request = 100000, token_policy = c("reuse", "refresh"),
                             timeout_seconds = 180, retry_max_times = 2L,
                             request_pause = c(30, 60),
                             max_depth = 20L, max_failures = 6L, verbose = TRUE,
                             log_file = file.path(path, "download.log"),
                             recovery_max_times = 3L, recovery_pause = 60,
                             time_limit_seconds = 3600, access_pause = 300,
                             prepare_grid = FALSE) {
    obsolete <- intersect(names(match.call(expand.dots = FALSE)$...), c("max_cells", "max_combinations"))
    if (length(obsolete)) stop("Use max_nominal_cells_request instead of ", paste(obsolete, collapse = ", "), ".", call. = FALSE)
    if (!is.logical(prepare_grid) || length(prepare_grid) != 1L || is.na(prepare_grid)) {
        stop("prepare_grid must be TRUE or FALSE.", call. = FALSE)
    }
    started <- fedstat_elapsed()
    token_policy <- match.arg(token_policy)
    verbose <- fedstat_verbosity(verbose)
    fedstat_positive(access_pause, "access_pause")
    for (name in c("max_nominal_cells_request", "time_limit_seconds", "timeout_seconds", "retry_max_times", "max_depth", "max_failures")) {
        fedstat_positive(get(name), name, integer = name %in% c("max_nominal_cells_request", "retry_max_times", "max_depth", "max_failures"))
    }
    time_left <- function() {
        seconds <- time_limit_seconds - (fedstat_elapsed() - started)
        if (seconds < 0.001) fedstat_time_expired(time_limit_seconds)
        seconds
    }
    if (!is.numeric(recovery_max_times) || length(recovery_max_times) != 1L ||
        !is.finite(recovery_max_times) || recovery_max_times < 0 || recovery_max_times != floor(recovery_max_times)) {
        stop("recovery_max_times must be a nonnegative integer.", call. = FALSE)
    }
    if (!is.numeric(recovery_pause) || length(recovery_pause) != 1L ||
        !is.finite(recovery_pause) || recovery_pause < 0) {
        stop("recovery_pause must be a nonnegative number.", call. = FALSE)
    }
    if (!is.numeric(request_pause) || !length(request_pause) %in% 1:2 ||
        any(!is.finite(request_pause)) || any(request_pause < 0) || is.unsorted(request_pause)) {
        stop("request_pause must be a nonnegative number or increasing pair.", call. = FALSE)
    }
    if (!is.character(path) || length(path) != 1L || is.na(path) || !nzchar(path)) {
        stop("path must be a download directory.", call. = FALSE)
    }
    if (length(indicator_id) != 1L || is.na(indicator_id) || !grepl("^[0-9]+$", as.character(indicator_id))) {
        stop("indicator_id must be a single numeric indicator code.", call. = FALSE)
    }
    indicator_id <- as.character(indicator_id)
    filters <- fedstat_filters(filters)
    dir.create(path, recursive = TRUE, showWarnings = FALSE)
    path <- normalizePath(path, mustWork = TRUE)
    lock <- file.path(path, ".lock")
    if (!dir.create(lock, showWarnings = FALSE)) {
        stop("Download directory is locked: ", path,
            ". If the previous process exited unexpectedly, remove .lock after checking it is no longer running.",
            call. = FALSE
        )
    }
    on.exit(unlink(lock, recursive = TRUE), add = TRUE)
    writeLines(paste(Sys.info()[["nodename"]], Sys.getpid(), Sys.time()), file.path(lock, "owner"))
    manifest_path <- file.path(path, "manifest.rds")
    manifest <- NULL
    requests <- 0L
    current_part <- current_attempt <- NULL
    phase <- "startup"
    progress_cache <- new.env(parent = emptyenv())
    report <- fedstat_logger(log_file, indicator_id, verbose, function() {
        c(list(
            request = requests, part = current_part,
            time_limit_seconds = time_limit_seconds,
            remaining_seconds = max(0, time_limit_seconds - (fedstat_elapsed() - started)),
            attempt = current_attempt, stage = phase
        ), fedstat_progress(manifest, progress_cache))
    })
    on.exit(attr(report, "close")(), add = TRUE)
    stopped <- function(e, event) {
        checkpoint <- if (file.exists(manifest_path)) tryCatch(readRDS(manifest_path), error = function(e) NULL) else NULL
        progress <- fedstat_progress(checkpoint)
        if (event == "INTERRUPTED") {
            report(event, paste0(
                "Cancelled; ", progress$saved_parts,
                " saved part(s). Repeat the same call to continue."
            ),
            stage = if (is.null(e$stage)) phase else e$stage
            )
            return(invisible(NULL))
        }
        retry <- fedstat_retryable(e) ||
            any(vapply(
                c("fedstat_budget", "fedstat_unavailable", "fedstat_token", "fedstat_redirect"),
                function(cl) inherits(e, cl), logical(1L)
            ))
        action <- if (inherits(e, "fedstat_time_limit")) {
            "Time allowance used. Repeat the same call to continue with a new time allowance."
        } else if (inherits(e, "fedstat_budget")) {
            "Subdivision limit reached. Increase max_depth and repeat the same call to resume."
        } else if (retry) {
            "Automatic recovery stopped. Saved parts will be skipped on resume; inspect download.log if the same part fails again."
        } else {
            "Inspect the error before resuming; saved files have been retained."
        }
        text <- paste0(
            conditionMessage(e), " Checkpoint records ", progress$saved_parts,
            " saved part(s). ", action, " Directory: ", path
        )
        tryCatch(report(event, text,
            error_class = class(e)[1L],
            stage = if (is.null(e$stage)) phase else e$stage, status = e$status, evidence = e$evidence,
            saved_parts = progress$saved_parts, pending_parts = progress$pending_parts,
            rows = progress$rows, action = action
        ), error = function(log_error) {
            message("Could not append the diagnostic log: ", conditionMessage(log_error), "\n", text)
        })
    }
    withCallingHandlers(
        {
            report("START", paste0(
                "Indicator ", indicator_id, "; max_nominal_cells_request=", format(max_nominal_cells_request, scientific = FALSE, trim = TRUE),
                "; time limit: ", time_limit_seconds, " s (", round(time_limit_seconds / 60, 1), " min); directory: ", path
            ),
            package_version = as.character(utils::packageVersion("fedstatAPIr")),
            r_version = as.character(getRversion()), platform = R.version$platform,
            max_nominal_cells_request = max_nominal_cells_request, token_policy = token_policy, timeout_seconds = timeout_seconds,
            prepare_grid = prepare_grid,
            request_pause = request_pause, retry_max_times = retry_max_times, max_depth = max_depth,
            max_failures = max_failures, log_file = log_file,
            recovery_max_times = recovery_max_times, recovery_pause = recovery_pause, access_pause = access_pause
            )
            if (max_nominal_cells_request > 250000) {
                warning(
                    "Large requests may fail more often: max_nominal_cells_request exceeds 250,000. Try 100,000 for smaller, usually more reliable downloads; the best size depends on the indicator and how busy Fedstat is.",
                    call. = FALSE, immediate. = TRUE
                )
            }
            manifest <- if (file.exists(manifest_path)) readRDS(manifest_path) else NULL
            if (is.null(manifest) && length(list.files(c(path, file.path(path, "parts")), pattern = "^part-[0-9.]+[.]rds$"))) {
                stop("Saved parts exist without a manifest; restore manifest.rds or use a new path.", call. = FALSE)
            }
            save_manifest <- function() {
                previous <- phase
                phase <<- "storage"
                on.exit(phase <<- previous)
                fedstat_save(manifest, manifest_path)
            }
            if (!is.null(manifest)) {
                if (isTRUE(manifest$cleaned)) stop("Saved parts were removed after collection. Use your collected data or choose a new download folder.", call. = FALSE)
                if (!identical(manifest$version, 1L) || !identical(manifest$indicator_id, indicator_id) ||
                    !identical(fedstat_filters(manifest$filters), filters)) {
                    stop("This directory belongs to a different selection or manifest version; use a new path.", call. = FALSE)
                }
                phase <- "validation"
                report("VERIFY", "Checking saved-part checksums.")
                fedstat_check_parts(manifest, path)
                if (all(vapply(manifest$parts, function(p) p$status != "pending", logical(1L)))) {
                    if (!isTRUE(manifest$validated)) {
                        fedstat_validate_parts(manifest, path, report)
                        manifest$validated <- TRUE
                        save_manifest()
                    }
                    report("COMPLETE", paste0("Already complete and verified: ", path))
                    return(fedstat_download_result(manifest, path))
                }
            }
            delay <- 0
            failures <- recoveries <- 0L
            session <- NULL
            recover <- function(error) {
                time_left()
                access <- inherits(error, "fedstat_blocked")
                possible <- access || fedstat_retryable(error) || inherits(error, "fedstat_unavailable") ||
                    (isTRUE(error$stage %in% c("export", "grid")) && any(vapply(
                        c("fedstat_redirect", "fedstat_token", "fedstat_unexpected_response"),
                        function(cl) inherits(error, cl), logical(1L)
                    )))
                if (!possible || recoveries >= recovery_max_times) {
                    return(FALSE)
                }
                recoveries <<- recoveries + 1L
                delay <<- max(delay, (if (access) access_pause else recovery_pause) * 2^min(recoveries - 1L, 6L), fedstat_backoff(recoveries, error))
                failures <<- 0L
                if (!is.null(session)) session$token <- NULL
                if (access) {
                    warning(
                        paste0(
                            "Fedstat may be checking or restricting access. Waiting at least ",
                            round(delay / 60, 1), " minutes before refreshing the download session and trying again. Saved data are safe."
                        ),
                        call. = FALSE, immediate. = TRUE
                    )
                    report("ACCESS_WAIT", "Pausing after an access check or refusal.",
                        seconds = delay,
                        recovery = recoveries, evidence = error$evidence, status = error$status
                    )
                }
                report("RECOVER", paste0(
                    "Automatic recovery ", recoveries, "/", recovery_max_times,
                    "; waiting at least ", round(delay, 1), " s before retrying unfinished work. Saved parts are retained."
                ),
                recovery = recoveries, seconds = delay, reason = class(error)[1L]
                )
                TRUE
            }
            gate <- function() {
                time_left()
                if (requests > 0L) {
                    pause <- max(stats::runif(1L, min(request_pause), max(request_pause)), delay)
                    if (pause >= time_left()) fedstat_time_expired(time_limit_seconds, waiting = TRUE)
                    until <- format(Sys.time() + pause, "%Y-%m-%dT%H:%M:%OS3Z", tz = "UTC")
                    report("WAIT", paste0("Waiting ", round(pause, 1), " s; next request after ", until),
                        seconds = pause, next_request_at = until, reason = if (delay > 0) "retry" else "pacing"
                    )
                    Sys.sleep(pause)
                }
                time_left()
                delay <<- 0
                requests <<- requests + 1L
            }
            attr(gate, "report") <- report
            attr(gate, "time_left") <- time_left
            attr(gate, "time_limit_seconds") <- time_limit_seconds
            attr(gate, "backoff") <- function(seconds) delay <<- max(delay, seconds)
            report("PREPARE", "Reading available filters.")
            phase <- "metadata"
            # Fetch once per invocation; subsequent token refreshes do not reparse metadata.
            metadata <- NULL
            repeat {
                for (attempt in seq_len(retry_max_times)) {
                    current_attempt <- attempt
                    metadata <- tryCatch(fedstat_read_ids(indicator_id, ...,
                        timeout_seconds = timeout_seconds,
                        retry_max_times = 1L, before = gate
                    ), error = identity)
                    if (!inherits(metadata, "error")) break
                    if (!fedstat_retryable(metadata) || attempt == retry_max_times) break
                    delay <- fedstat_backoff(attempt, metadata)
                    report("RETRY", "Metadata unavailable; retrying after backoff.", reason = class(metadata)[1L])
                }
                if (!inherits(metadata, "error")) break
                if (!recover(metadata)) stop(metadata)
            }
            session <- fedstat_session(metadata)
            session$before <- gate
            session$prepare_grid <- prepare_grid
            phase <- "selection"
            if (is.null(manifest)) {
                selection <- fedstat_download_selection(metadata, filters)
                manifest <- list(
                    version = 1L, indicator_id = indicator_id, filters = filters,
                    selection = selection, created_at = format(Sys.time(), tz = "UTC", usetz = TRUE),
                    validated = FALSE, parts_dir = "parts", parts = list(list(id = "1", rows = seq_len(nrow(selection)), depth = 0L, status = "pending"))
                )
                save_manifest()
            } else {
                fedstat_check_selection(manifest$selection, metadata)
                report("RESUME", paste0(
                    "Resuming: ", fedstat_progress(manifest)$saved_parts,
                    " saved parts will be skipped; existing partitions are retained. Continuing in ", path
                ))
            }
            parts_dir <- if (is.null(manifest$parts_dir)) "" else manifest$parts_dir
            if (nzchar(parts_dir)) dir.create(file.path(path, parts_dir), showWarnings = FALSE)
            selection <- manifest$selection
            counts <- table(selection$filter_field_id)
            report("SELECTION", paste0(
                length(counts) - 1L, " dimensions; ",
                format(prod(counts), scientific = FALSE, trim = TRUE), " nominal cells."
            ),
            cardinalities = as.list(counts), nominal_cells = format(prod(counts), scientific = FALSE, trim = TRUE)
            )
            split_part <- function(index, reason) {
                part <- manifest$parts[[index]]
                halves <- fedstat_halves(selection, part$rows)
                if (is.null(halves)) {
                    return(FALSE)
                }
                if (part$depth >= max_depth) {
                    fedstat_error(
                        "Subdivision depth reached; raise max_depth or inspect the selection.",
                        "budget", "schedule"
                    )
                }
                manifest$parts[[index]]$status <<- "split"
                children <- lapply(1:2, function(i) {
                    list(
                        id = paste0(part$id, ".", i), rows = halves[[i]],
                        depth = part$depth + 1L, status = "pending"
                    )
                })
                manifest$parts <<- append(manifest$parts, children, after = index)
                save_manifest()
                field <- attr(halves, "field")
                label <- selection$filter_field_title[match(field, selection$filter_field_id)]
                sizes <- vapply(halves, function(rows) sum(selection$filter_field_id[rows] == field), integer(1L))
                report("SPLIT", paste0(
                    "Split ", label, ": ", sum(sizes), " -> ", paste(sizes, collapse = " + "),
                    "; reason: ", reason
                ),
                part = part$id, dimension_id = field,
                dimension = label, cardinalities = sizes, reason = reason, depth = part$depth + 1L
                )
                TRUE
            }
            repeat {
                pending <- which(vapply(manifest$parts, function(p) p$status == "pending", logical(1L)))
                if (!length(pending)) break
                time_left()
                index <- pending[1L]
                part <- manifest$parts[[index]]
                current_part <- part$id
                current_attempt <- NULL
                phase <- "planning"
                selected <- selection[part$rows, , drop = FALSE]
                log_cells <- sum(log(as.numeric(table(selected$filter_field_id))))
                if (log_cells > log(max_nominal_cells_request) + 1e-10) {
                    if (!split_part(index, "size_limit")) stop("Selection cannot be reduced below max_nominal_cells_request.", call. = FALSE)
                    next
                }
                report("DOWNLOAD", "Downloading the next part.")
                selected <- fedstat_attach_session(selected, session)
                repaired <- FALSE
                for (attempt in seq_len(retry_max_times)) {
                    current_attempt <- attempt
                    phase <- "export"
                    result <- tryCatch(
                        {
                            if (token_policy == "refresh" && isTRUE(session$used)) session$token <- NULL
                            report("PART", paste0(
                                "Preparing part ", part$id, "; attempt ", attempt, "/", retry_max_times,
                                "; filter combinations: ", round(exp(log_cells))
                            ))
                            bytes <- fedstat_post_data_ids_filtered(selected, ...,
                                timeout_seconds = timeout_seconds,
                                retry_max_times = retry_max_times
                            )
                            phase <- "parse"
                            parse_started <- proc.time()[["elapsed"]]
                            data <- fedstat_parse_sdmx(bytes, try_to_parse_ObsValue = FALSE, indicator_id = indicator_id)
                            fedstat_check_data(data)
                            report("PARSED", paste0(nrow(data), " rows validated."),
                                rows = nrow(data),
                                seconds = round(proc.time()[["elapsed"]] - parse_started, 3)
                            )
                            data
                        },
                        error = identity
                    )
                    if (!inherits(result, "error")) break
                    # Ambiguous preview/export rejections can justify a smaller probe only after a
                    # session repair. GETs, explicit CSRF refusals and rate limits cannot split.
                    token <- isTRUE(result$stage %in% c("export", "grid")) && (inherits(result, "fedstat_token") ||
                        inherits(result, "fedstat_redirect") || inherits(result, "fedstat_unexpected_response"))
                    transfer <- inherits(result, "fedstat_transient") && isTRUE(result$stage %in% c("export", "grid"))
                    if (transfer || token) failures <- failures + 1L
                    report("PART_ERROR", conditionMessage(result),
                        error_class = class(result)[1L],
                        stage = if (is.null(result$stage)) phase else result$stage, status = result$status, evidence = result$evidence
                    )
                    if (failures >= max_failures) {
                        result <- tryCatch(fedstat_error(
                            "Repeated data requests failed without progress. Saved parts are safe; try again later or inspect download.log.",
                            "unavailable", "export"
                        ), error = identity)
                        break
                    }
                    if (inherits(result, "fedstat_truncated")) break
                    if (attempt == retry_max_times || (!fedstat_retryable(result) && !(token && !repaired))) break
                    if (token || transfer) session$token <- NULL
                    if (token) repaired <- TRUE
                    delay <- fedstat_backoff(attempt, result)
                    report("RETRY", "Retry scheduled; saved parts are retained.",
                        stage = if (is.null(result$stage)) phase else result$stage, reason = class(result)[1L]
                    )
                }
                if (inherits(result, "error")) {
                    if (repaired && identical(result$evidence, "empty_preview")) {
                        fedstat_error(
                            "Fedstat reports no observations in the preview, but still refuses the SDMX export after session renewal. This part remains unfinished; further splitting would not confirm completeness. Check the selection on the website before resuming.",
                            "empty_export", "export", result$status,
                            evidence = "empty_preview"
                        )
                    }
                    rejected <- repaired && isTRUE(result$stage %in% c("export", "grid")) &&
                        (inherits(result, "fedstat_redirect") || inherits(result, "fedstat_unexpected_response"))
                    splittable <- rejected || inherits(result, "fedstat_truncated") ||
                        (inherits(result, "fedstat_transient") && isTRUE(result$stage %in% c("export", "grid")))
                    if (splittable) {
                        delay <- fedstat_backoff(retry_max_times, result)
                        session$token <- NULL
                        reason <- if (rejected) "rejected_after_refresh" else if (inherits(result, "fedstat_truncated")) "truncated" else "export_failures"
                        if (split_part(index, reason)) next
                    }
                    if (recover(result)) next
                    stop(result)
                }
                failures <- recoveries <- 0L
                file <- paste0("part-", part$id, ".rds")
                if (nzchar(parts_dir)) file <- file.path(parts_dir, file)
                phase <- "storage"
                fedstat_save(result, file.path(path, file))
                manifest$parts[[index]]$status <- "complete"
                manifest$parts[[index]]$file <- file
                manifest$parts[[index]]$checksum <- unname(tools::md5sum(file.path(path, file)))
                manifest$parts[[index]]$nrow <- nrow(result)
                manifest$parts[[index]]$retrieved_at <- format(Sys.time(), tz = "UTC", usetz = TRUE)
                save_manifest()
                report("SAVED", paste0("Saved part ", part$id, ": ", nrow(result), " rows."), file = file)
            }
            phase <- "validation"
            current_part <- current_attempt <- NULL
            fedstat_validate_parts(manifest, path, report)
            manifest$validated <- TRUE
            save_manifest()
            report("COMPLETE", paste0("Complete and validated: ", path, ". Use fedstat_collect() to load the data."))
            fedstat_download_result(manifest, path)
        },
        error = function(e) stopped(e, "STOP"),
        interrupt = function(e) stopped(e, "INTERRUPTED"),
        warning = function(w) report("WARNING", conditionMessage(w), warning_class = class(w)[1L])
    )
}

fedstat_download_selection <- function(metadata, filters) {
    if (identical(filters, "*")) {
        selection <- metadata
    } else {
        # The legacy filter helper warns for partially unmatched values. A batch
        # job must not silently turn that warning into an incomplete download.
        selection <- withCallingHandlers(fedstat_data_ids_filter(metadata, filters),
            warning = function(w) stop(conditionMessage(w), call. = FALSE)
        )
    }
    selection <- as.data.frame(selection)
    attributes(selection) <- attributes(selection)[c("names", "row.names", "class")]
    selection <- selection[order(selection$filter_field_id, selection$filter_value_id, method = "radix"), ]
    rownames(selection) <- NULL
    selection
}

fedstat_check_selection <- function(selection, metadata) {
    fields <- unique(selection$filter_field_id)
    if (!setequal(fields, unique(metadata$filter_field_id))) {
        stop("Indicator dimensions changed; use a new download path.", call. = FALSE)
    }
    key <- function(x) paste(x$filter_field_id, x$filter_value_id, sep = "_")
    current <- metadata[match(key(selection), key(metadata)), names(selection), drop = FALSE]
    if (anyNA(current) || !all(vapply(names(selection), function(n) identical(selection[[n]], current[[n]]), logical(1L)))) {
        stop("Selected filter IDs, labels or layout changed; use a new download path.", call. = FALSE)
    }
}

fedstat_halves <- function(selection, rows) {
    selected <- selection[rows, , drop = FALSE]
    counts <- table(selected$filter_field_id)
    counts <- counts[names(counts) != "0" & counts > 1L]
    if (!length(counts)) {
        return(NULL)
    }
    field <- names(counts)[which.max(counts)]
    ids <- sort(unique(selected$filter_value_id[selected$filter_field_id == field]), method = "radix")
    first <- ids[seq_len(ceiling(length(ids) / 2))]
    in_field <- selected$filter_field_id == field
    structure(list(
        rows[!in_field | selected$filter_value_id %in% first],
        rows[!in_field | !selected$filter_value_id %in% first]
    ), field = field)
}

fedstat_save <- function(object, path) {
    temporary <- tempfile(pattern = ".fedstat-", tmpdir = dirname(path))
    on.exit(unlink(temporary), add = TRUE)
    saveRDS(object, temporary, version = 2)
    if (!file.rename(temporary, path)) stop("Cannot publish download file: ", path, call. = FALSE)
}

fedstat_check_data <- function(data) {
    if (!is.data.frame(data) || !"ObsValue" %in% names(data)) stop("Export has no ObsValue column.", call. = FALSE)
    keys <- fedstat_key_columns(names(data))
    if (nrow(data) && (!length(keys) || anyDuplicated(data[keys]))) {
        stop("Duplicate observation keys in downloaded parts; inspect the source before combining.", call. = FALSE)
    }
    invisible(data)
}

fedstat_check_parts <- function(manifest, path) {
    for (part in manifest$parts) {
        if (part$status == "complete") {
            file <- file.path(path, part$file)
            if (!file.exists(file) || !identical(unname(tools::md5sum(file)), part$checksum)) {
                stop("Saved part is missing or changed: ", file, ". Restore it or use a new download path.", call. = FALSE)
            }
        }
    }
}

fedstat_download_result <- function(manifest, path) {
    parts <- manifest$parts[vapply(manifest$parts, function(p) p$status != "split", logical(1L))]
    summary <- data.frame(
        part = vapply(parts, `[[`, "", "id"),
        status = vapply(parts, `[[`, "", "status"), stringsAsFactors = FALSE
    )
    summary$rows <- vapply(parts, function(p) if (is.null(p$nrow)) NA_integer_ else as.integer(p$nrow), integer(1L))
    structure(list(
        path = path, indicator_id = manifest$indicator_id,
        complete = isTRUE(manifest$validated) && all(summary$status == "complete"), parts = summary
    ), class = "fedstat_download")
}

#' @export
print.fedstat_download <- function(x, ...) {
    cat("Fedstat ", x$indicator_id, ": ", if (x$complete) "complete" else "incomplete", "; ",
        sum(x$parts$status == "complete"), "/", nrow(x$parts), " parts, ",
        sum(x$parts$rows, na.rm = TRUE), " rows\n", x$path, "\n",
        sep = ""
    )
    invisible(x)
}

#' Collect a completed download into memory
#' @param x A [fedstat_download()] result or its directory path.
#' @param remove_parts Remove the saved parts only after successfully collecting
#'   and validating the full table. Default `FALSE`. The log and manifest remain.
#'   The returned table then exists only in memory until you save it. To save first
#'   and remove parts later, use [fedstat_cleanup()].
#' @return A data.frame with the existing parser's columns and raw character
#'   ObsValue values. All parts are loaded into memory; for large jobs read the
#'   individual RDS files listed in `manifest.rds` instead.
#' @export
fedstat_collect <- function(x, remove_parts = FALSE) {
    if (!is.logical(remove_parts) || length(remove_parts) != 1L || is.na(remove_parts)) {
        stop("remove_parts must be TRUE or FALSE.", call. = FALSE)
    }
    path <- fedstat_job_path(x)
    lock <- fedstat_collect_lock(path)
    on.exit(unlink(lock, recursive = TRUE), add = TRUE)
    manifest <- readRDS(file.path(path, "manifest.rds"))
    if (isTRUE(manifest$cleaned)) stop("Saved parts have already been removed. Use the table you collected and saved.", call. = FALSE)
    if (!identical(manifest$version, 1L)) stop("Unsupported download manifest version.", call. = FALSE)
    if (any(vapply(manifest$parts, function(p) p$status == "pending", logical(1L)))) {
        stop("Download is incomplete; resume fedstat_download() before collecting.", call. = FALSE)
    }
    fedstat_check_parts(manifest, path)
    parts <- manifest$parts[vapply(manifest$parts, function(p) p$status == "complete", logical(1L))]
    data <- as.data.frame(data.table::rbindlist(lapply(parts, function(p) readRDS(file.path(path, p$file))),
        use.names = TRUE, fill = TRUE
    ))
    fedstat_check_data(data)
    if (remove_parts) fedstat_remove_parts(manifest, path)
    data
}
