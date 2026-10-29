#' Download Fedstat data in small parts that survive interruptions
#'
#' @description Downloads one indicator for the filter values you choose and
#'   saves it piece by piece in `cache_dir`. Fedstat builds every combination of
#'   the selected values (years x months x territories x products ...), so a large
#'   selection is split into parts of at most `max_nominal_cells_request`
#'   combinations. Parts are requested one at a time, with pauses in between.
#'   This keeps the load on Fedstat light and makes long downloads finish.
#'
#'   If a run stops (time is up, Fedstat is unavailable, R was closed), run the
#'   same call again: saved parts are skipped. When the download is complete,
#'   [fedstat_collect()] returns the table.
#' @param indicator_id Indicator code from its page address, for example `"31074"`
#'   for `https://www.fedstat.ru/indicator/31074`.
#' @param filters A named list: filter names and the values to download, both in
#'   Russian, as shown on Fedstat. `"*"` selects every value of a filter. Every
#'   filter with more than one value must be listed. Case and extra spaces are
#'   ignored. The single string `"*"` instead of a list selects everything.
#' @param cache_dir Folder for the saved parts, created if needed. Use one folder
#'   per download and keep it until you have collected and saved the table. If
#'   omitted, a folder such as `fedstat-31074-1a2b3c4d` is created in the current
#'   folder: its name depends on the indicator and the filters, so running the
#'   same call again continues in the same folder. The full path is shown.
#' @param ... For advanced users: httr settings applied to every request, for
#'   example `httr::user_agent("my-project")` or `httr::config(connecttimeout = 20)`.
#'   Any other unnamed or unknown argument is an error. Arguments after `...`
#'   must be named in full.
#' @param max_nominal_cells_request Largest number of filter combinations in one
#'   request. Default 50,000. Above 50,000 you get a warning, above 100,000
#'   `allow_large_requests = TRUE` is required, and 250,000 is the maximum.
#' @param allow_large_requests `TRUE` turns off the warning for
#'   `max_nominal_cells_request` above 50,000 and allows up to 250,000.
#' @param preview_first If `TRUE`, ask Fedstat to show each part's table first,
#'   as its website does when you press "Apply", and then download it. Fedstat
#'   sometimes only returns data after this step. Default `FALSE`: it adds a
#'   request per attempt and can roughly double the download time.
#' @param total_time_limit_seconds Time for this run in seconds, including
#'   pauses; default one hour. Run the same call again to continue.
#' @param timeout_seconds Longest wait for one request, in seconds.
#' @param retry_max_times Attempts per part before it is split or the download
#'   pauses, including the first.
#' @param request_pause Seconds between requests: one number, or two numbers for a
#'   random pause in that range.
#' @param verbose `TRUE` (default) shows short progress messages, `FALSE` hides
#'   them, `"detailed"` shows every event. Warnings and errors are always shown.
#' @param log_file Diagnostic log, by default `download.log` inside `cache_dir`;
#'   `NULL` turns it off. Attach it when reporting a problem.
#' @param token_policy `"reuse"` (default) keeps the download session's token until
#'   Fedstat rejects it; `"refresh"` gets a new one before every request, which costs
#'   an extra page request each time.
#' @param max_depth How many times a part may be split again.
#' @param max_failures Failed data requests in a row before the download pauses to
#'   let Fedstat recover.
#' @param recovery_max_times How many recovery pauses are allowed since the last
#'   saved part before the run stops. `0` stops after ordinary retries.
#' @param recovery_pause First recovery pause in seconds; it doubles each time.
#' @param access_pause Seconds to wait when Fedstat seems to check or refuse
#'   access; default five minutes, doubling if it happens again. No check is
#'   bypassed.
#' @return A `fedstat_download` object: the folder (`path`), whether the download
#'   is `complete`, a table of `parts`, and the values Fedstat stopped offering
#'   during the download (`unavailable`). The data stay on disk; use
#'   [fedstat_collect()] to load them. If the run stops, an error is raised and
#'   saved parts are kept for the next run.
#' @section Resuming: Run the same call with the same `filters` and `cache_dir`.
#'   Filters may be reordered or differ in case and spacing. A different
#'   selection needs a different folder. `"*"` means the values offered when the
#'   download started: values Fedstat adds later are not included. Each run has its
#'   own time allowance; saving a part that has already arrived may run slightly
#'   over it. Only one R session can use a folder at a time. If R crashed, the
#'   folder can stay marked as in use; the error message says how to clear it.
#'
#' @section Request size: Size is counted in filter combinations, the product of
#'   the number of selected values of every filter, not in rows: many
#'   combinations can be empty. A part over the limit is split along its largest
#'   filter into the fewest groups that fit. Fedstat checks every combination,
#'   even empty ones, so large requests load it for all its users and often
#'   fail. Every function that requests data uses the same limits: a warning
#'   above 50,000 combinations, `allow_large_requests = TRUE` needed above
#'   100,000, and no request above 250,000.
#'
#' @section Reliability: Requests are sequential and paced. A failed request is
#'   retried after a pause. A part is halved only when its size is the likely
#'   cause: a request timeout, a gateway timeout (HTTP 504), an export marked as
#'   truncated, or a refusal that persists after renewing the session. Dropped or
#'   refused connections, server errors and rate limits lead to a recovery pause
#'   instead. Splits made after failures are undone on the next run if none of
#'   their parts was saved. Fedstat answers some empty selections with a redirect;
#'   a part counts as empty only when Fedstat's table preview confirms zero rows.
#'
#' @section When Fedstat changes its filters: The selected filters are compared
#'   with the indicator page at the start of every run, when the session is
#'   renewed and before completion. Filter and value IDs identify what is
#'   requested. If Fedstat adds or removes a whole filter, the download stops:
#'   start again in a new folder. A selected value that Fedstat no longer offers
#'   is left out of unfinished parts, with a warning, and listed in `unavailable`.
#'   Renamed values continue with a warning, and [fedstat_collect()] uses the
#'   newest name for each code. A changed row/column layout on the website is
#'   ignored. If an export contains values that were not requested, the download
#'   stops. A change in meaning that keeps the same IDs and names cannot be
#'   detected.
#'
#' @section Completion: A download is complete when every part has been received
#'   and checked, and the saved parts together contain no duplicate
#'   observations. This check needs some free disk space in `cache_dir` but never
#'   loads the whole table. A completed download opens again without network
#'   access. A long download is not a snapshot of one moment: Fedstat may publish
#'   revisions while it runs. Saved parts keep values exactly as Fedstat sent
#'   them.
#'
#' @section Diagnostic log: `download.log` is a JSON Lines file with request
#'   stages, timings, counts and recovery decisions. It never contains HTTP
#'   headers, cookies, tokens or response bodies.
#' @seealso [fedstat_collect()], [fedstat_cleanup()]
#' @export
#' @examples
#' \dontrun{
#' job <- fedstat_download("31074", filters = "*", cache_dir = "cpi-download")
#' data <- fedstat_collect(job)
#'
#' # Filter names as on the indicator page (English placeholders: CRAN needs ASCII)
#' filters <- list("Year" = "2024", "Period" = "January", "Territory" = "*")
#' # Without cache_dir, a folder is chosen for you and its path is shown
#' job <- fedstat_download("31074", filters)
#' }
# The three main arguments come first so they work positionally; httr settings
# in `...` follow, so unnamed settings never fill later arguments, and the rest
# must be named in full.
fedstat_download <- function(indicator_id, filters = list(), cache_dir = NULL, ...,
                             max_nominal_cells_request = 50000, allow_large_requests = FALSE,
                             preview_first = FALSE, total_time_limit_seconds = 3600,
                             timeout_seconds = 180, retry_max_times = 2L,
                             request_pause = c(30, 60), verbose = TRUE,
                             log_file = file.path(cache_dir, "download.log"),
                             token_policy = c("reuse", "refresh"),
                             max_depth = 20L, max_failures = 6L,
                             recovery_max_times = 3L, recovery_pause = 60,
                             access_pause = 300) {
    fedstat_check_dots(list(...), "fedstat_download")
    if (!is.logical(preview_first) || length(preview_first) != 1L || is.na(preview_first)) {
        stop("preview_first must be TRUE or FALSE.", call. = FALSE)
    }
    started <- fedstat_elapsed()
    token_policy <- match.arg(token_policy)
    verbose <- fedstat_verbosity(verbose)
    fedstat_positive(access_pause, "access_pause")
    for (name in c("max_nominal_cells_request", "total_time_limit_seconds", "timeout_seconds", "retry_max_times", "max_depth", "max_failures")) {
        fedstat_positive(get(name), name, integer = name %in% c("max_nominal_cells_request", "retry_max_times", "max_depth", "max_failures"))
    }
    fedstat_flag(allow_large_requests, "allow_large_requests")
    large_parts <- fedstat_check_part_size(max_nominal_cells_request, allow_large_requests)
    indicator_id <- fedstat_indicator_id(indicator_id)
    filters <- fedstat_filters(filters)
    default_dir <- is.null(cache_dir)
    # Set before log_file's default is evaluated, which refers to cache_dir.
    if (default_dir) cache_dir <- fedstat_default_cache_dir(indicator_id, filters)
    path <- cache_dir
    time_left <- function() {
        seconds <- total_time_limit_seconds - (fedstat_elapsed() - started)
        if (seconds < 0.001) fedstat_time_expired(total_time_limit_seconds)
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
    fedstat_pause_check(request_pause)
    if (!is.character(path) || length(path) != 1L || is.na(path) || !nzchar(path)) {
        stop("cache_dir must be a folder for saved download parts.", call. = FALSE)
    }
    dir.create(path, recursive = TRUE, showWarnings = FALSE)
    path <- normalizePath(path, mustWork = TRUE)
    if (default_dir && verbose != "quiet") {
        message("Saving the download in ", path, ". Running the same call again continues there; set cache_dir to choose the folder.")
    }
    lock <- fedstat_lock(path, "fedstat_download()")
    on.exit(unlink(lock, recursive = TRUE), add = TRUE)
    manifest_path <- file.path(path, "manifest.rds")
    manifest <- NULL
    requests <- 0L
    current_part <- current_attempt <- NULL
    phase <- "startup"
    progress_cache <- new.env(parent = emptyenv())
    report <- fedstat_logger(log_file, indicator_id, verbose, function() {
        c(list(
            request = requests, part = current_part,
            total_time_limit_seconds = total_time_limit_seconds,
            remaining_seconds = max(0, total_time_limit_seconds - (fedstat_elapsed() - started)),
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
                "; time limit: ", total_time_limit_seconds, " s (", round(total_time_limit_seconds / 60, 1), " min); directory: ", path
            ),
            package_version = as.character(utils::packageVersion("fedstatAPIr")),
            r_version = as.character(getRversion()), platform = R.version$platform,
            max_nominal_cells_request = max_nominal_cells_request, allow_large_requests = allow_large_requests,
            token_policy = token_policy, timeout_seconds = timeout_seconds,
            preview_first = preview_first,
            request_pause = request_pause, retry_max_times = retry_max_times, max_depth = max_depth,
            max_failures = max_failures, log_file = log_file,
            recovery_max_times = recovery_max_times, recovery_pause = recovery_pause, access_pause = access_pause
            )
            if (large_parts) warning(fedstat_part_size_warning, call. = FALSE, immediate. = TRUE)
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
                if (isTRUE(manifest$validated) && all(vapply(manifest$parts, function(p) p$status != "pending", logical(1L)))) {
                    report("COMPLETE", paste0("Already complete and verified: ", path))
                    return(fedstat_download_result(manifest, path))
                }
                parts <- fedstat_merge_failure_splits(manifest$parts)
                merged <- attr(parts, "merged")
                if (length(merged)) {
                    attr(parts, "merged") <- NULL
                    manifest$parts <- parts
                    save_manifest()
                    report("MERGE", paste0(
                        "Rejoined ", length(merged), " part(s) split after earlier failures; nothing from them was saved yet."
                    ), merged_parts = merged)
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
                    if (pause >= time_left()) fedstat_time_expired(total_time_limit_seconds, waiting = TRUE)
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
            attr(gate, "total_time_limit_seconds") <- total_time_limit_seconds
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
            session$preview_first <- preview_first
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
                report("RESUME", paste0(
                    "Resuming: ", fedstat_progress(manifest)$saved_parts,
                    " saved parts will be skipped; existing partitions are retained. Continuing in ", path
                ))
            }
            parts_dir <- if (is.null(manifest$parts_dir)) "" else manifest$parts_dir
            if (nzchar(parts_dir)) dir.create(file.path(path, parts_dir), showWarnings = FALSE)
            # The manifest keeps the selection as first resolved; this copy follows
            # Fedstat's current labels and layout for building requests.
            selection <- manifest$selection
            note_renamed <- function(renamed) {
                if (is.null(renamed) || !nrow(renamed)) {
                    return(invisible())
                }
                key <- function(x) fedstat_keys(x, c("field_id", "value_id", "new_title"))
                known <- if (is.null(manifest$renamed)) character() else key(manifest$renamed)
                renamed <- renamed[!key(renamed) %in% known, , drop = FALSE]
                if (!nrow(renamed)) {
                    return(invisible())
                }
                manifest$renamed <<- rbind(manifest$renamed, renamed)
                report("RENAMED", paste0("Fedstat renamed ", nrow(renamed), " value(s)."), renamed = renamed)
                warning(
                    "Fedstat renamed some values during this download: ",
                    fedstat_list_values(paste0(renamed$old_title, " -> ", renamed$new_title)),
                    ". The download continues; the final table uses the newest names.",
                    call. = FALSE, immediate. = TRUE
                )
            }
            apply_definitions <- function(current) {
                changes <- fedstat_compare_definitions(manifest$selection, current)
                if (length(changes$added_fields) || length(changes$removed_fields)) {
                    fedstat_dictionary_changed(paste0(
                        "Fedstat changed the filters of this indicator (",
                        paste(c(
                            if (length(changes$added_fields)) paste("added:", paste(changes$added_fields, collapse = ", ")),
                            if (length(changes$removed_fields)) paste("removed:", paste(changes$removed_fields, collapse = ", "))
                        ), collapse = "; "), ")."
                    ))
                }
                selection$filter_field_object_ids <<- changes$layout
                renamed <- changes$renamed
                selection$filter_value_title[renamed] <<- changes$new_title[renamed]
                note_renamed(data.frame(
                    field_id = manifest$selection$filter_field_id[renamed],
                    value_id = manifest$selection$filter_value_id[renamed],
                    old_title = manifest$selection$filter_value_title[renamed],
                    new_title = changes$new_title[renamed], stringsAsFactors = FALSE
                ))
                missing <- setdiff(changes$missing, manifest$unavailable$row)
                if (length(missing)) {
                    unavailable <- data.frame(
                        row = missing, field_id = manifest$selection$filter_field_id[missing],
                        field_title = manifest$selection$filter_field_title[missing],
                        value_id = manifest$selection$filter_value_id[missing],
                        value_title = manifest$selection$filter_value_title[missing],
                        detected_at = format(Sys.time(), tz = "UTC", usetz = TRUE), stringsAsFactors = FALSE
                    )
                    manifest$unavailable <<- rbind(manifest$unavailable, unavailable)
                    needed <- any(vapply(manifest$parts, function(p) p$status == "pending" && any(p$rows %in% missing), logical(1L)))
                    report("UNAVAILABLE", paste0(
                        "Fedstat no longer offers ", length(missing), " selected value(s)",
                        if (needed) "; unfinished parts continue without them." else "; they were already downloaded."
                    ), values = unavailable[c("field_title", "value_title")])
                    if (needed) {
                        warning(
                            "Fedstat no longer offers some of the values you selected: ",
                            fedstat_list_values(paste0(unavailable$field_title, ": ", unavailable$value_title)),
                            ". The download continues without them; see the 'unavailable' element of the result.",
                            call. = FALSE, immediate. = TRUE
                        )
                    }
                }
                if (length(missing) || length(renamed)) save_manifest()
                offered <<- current
                invisible()
            }
            offered <- metadata
            apply_definitions(metadata)
            # Labels in exports that match no filter value are logged once, never fatal.
            unmatched_seen <- character()
            note_unmatched <- function(unmatched) {
                if (is.null(unmatched)) {
                    return(invisible())
                }
                key <- paste(unmatched$field_title, unmatched$value_title, sep = "\r")
                fresh <- unmatched[!key %in% unmatched_seen, , drop = FALSE]
                unmatched_seen <<- union(unmatched_seen, key)
                if (nrow(fresh)) {
                    report("UNMAPPED_LABEL", paste0(
                        nrow(fresh), " exported label(s) match no filter value on the indicator page; not treated as an error."
                    ), labels = fresh)
                }
            }
            session$check_definitions <- apply_definitions
            dictionary <- manifest$dictionary
            if (is.null(dictionary)) dictionary <- fedstat_saved_dictionary(manifest, path)
            check_dictionary <- function(current) {
                updated <- fedstat_check_dictionary(dictionary, current)
                renamed <- attr(updated, "renamed")
                attr(updated, "renamed") <- NULL
                dictionary <<- updated
                if (!is.null(renamed)) note_renamed(renamed)
            }
            check_metadata <- function() {
                report("CHECK_METADATA", "Checking that the selected filters have not changed.")
                current <- fedstat_read_ids(indicator_id, ...,
                    timeout_seconds = timeout_seconds, retry_max_times = retry_max_times,
                    before = gate, session = session
                )
                apply_definitions(current)
                fresh <- fedstat_session(current)
                session$token <- fresh$token
                session$token_name <- fresh$token_name
                session$used <- FALSE
            }
            counts <- table(selection$filter_field_id)
            report("SELECTION", paste0(
                length(counts) - 1L, " dimensions; ",
                format(prod(counts), scientific = FALSE, trim = TRUE), " nominal cells."
            ),
            cardinalities = as.list(counts), nominal_cells = format(prod(counts), scientific = FALSE, trim = TRUE)
            )
            split_part <- function(index, reason) {
                part <- manifest$parts[[index]]
                groups <- fedstat_partition(selection, part$rows, if (reason == "size_limit") max_nominal_cells_request)
                if (is.null(groups)) {
                    return(FALSE)
                }
                if (part$depth >= max_depth) {
                    fedstat_error(
                        "Subdivision depth reached; raise max_depth or inspect the selection.",
                        "budget", "schedule"
                    )
                }
                manifest$parts[[index]]$status <<- "split"
                manifest$parts[[index]]$split_reason <<- reason
                children <- lapply(seq_along(groups), function(i) {
                    list(
                        id = paste0(part$id, ".", i), rows = groups[[i]],
                        depth = part$depth + 1L, status = "pending"
                    )
                })
                manifest$parts <<- append(manifest$parts, children, after = index)
                save_manifest()
                field <- attr(groups, "field")
                label <- selection$filter_field_title[match(field, selection$filter_field_id)]
                sizes <- vapply(groups, function(rows) sum(selection$filter_field_id[rows] == field), integer(1L))
                report("SPLIT", paste0(
                    "Split ", label, ": ", sum(sizes), " -> ", paste(sizes, collapse = " + "),
                    "; reason: ", reason
                ),
                part = part$id, dimension_id = field,
                dimension = label, cardinalities = sizes, reason = reason, depth = part$depth + 1L
                )
                TRUE
            }
            save_part <- function(index, data, evidence = NULL) {
                part <- manifest$parts[[index]]
                file <- paste0("part-", part$id, ".rds")
                if (nzchar(parts_dir)) file <- file.path(parts_dir, file)
                phase <<- "storage"
                fedstat_save(data, file.path(path, file))
                manifest$parts[[index]]$status <<- "complete"
                manifest$parts[[index]]$file <<- file
                manifest$parts[[index]]$checksum <<- unname(tools::md5sum(file.path(path, file)))
                manifest$parts[[index]]$nrow <<- nrow(data)
                manifest$parts[[index]]$empty_evidence <<- evidence
                manifest$parts[[index]]$retrieved_at <<- format(Sys.time(), tz = "UTC", usetz = TRUE)
                manifest$dictionary <<- dictionary
                save_manifest()
                report("SAVED", paste0("Saved part ", part$id, ": ", nrow(data), " rows."), file = file)
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
                # Values Fedstat no longer offers are left out of requests.
                rows <- setdiff(part$rows, manifest$unavailable$row)
                selected <- selection[rows, , drop = FALSE]
                if (!all(unique(selection$filter_field_id[part$rows]) %in% selected$filter_field_id)) {
                    report("EMPTY", "Every selected value of a filter in this part is no longer offered by Fedstat.", evidence = "values_withdrawn")
                    save_part(index, data.frame(ObsValue = character(), stringsAsFactors = FALSE), "values_withdrawn")
                    next
                }
                log_cells <- sum(log(as.numeric(table(selected$filter_field_id))))
                if (log_cells > log(max_nominal_cells_request) + 1e-10) {
                    if (!split_part(index, "size_limit")) stop("Selection cannot be reduced below max_nominal_cells_request.", call. = FALSE)
                    next
                }
                report("DOWNLOAD", "Downloading the next part.")
                selected <- fedstat_attach_session(selected, session)
                repaired <- outage <- FALSE
                session$empty_confirmed <- FALSE
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
                            # Part size was checked against the limits at the start.
                            bytes <- fedstat_post_data_ids_filtered(selected, ...,
                                timeout_seconds = timeout_seconds,
                                retry_max_times = retry_max_times, allow_large_requests = TRUE
                            )
                            phase <- "parse"
                            parse_started <- proc.time()[["elapsed"]]
                            data <- fedstat_parse_sdmx(bytes,
                                try_to_parse_ObsValue = FALSE, indicator_id = indicator_id,
                                check_dictionary = check_dictionary, selection = selected, offered = offered
                            )
                            note_unmatched(attr(data, "unmatched_labels"))
                            attr(data, "unmatched_labels") <- NULL
                            fedstat_check_data(data)
                            report("PARSED", paste0(nrow(data), " rows validated."),
                                rows = nrow(data),
                                seconds = round(proc.time()[["elapsed"]] - parse_started, 3)
                            )
                            data
                        },
                        error = identity
                    )
                    # Redirects are ambiguous. Only an explicit zero-row preview
                    # for this exact selection can confirm an empty part. Filter
                    # definitions are not re-read here: run start, token renewal
                    # and completion already check them.
                    if (inherits(result, "fedstat_redirect") && identical(result$stage, "export") && identical(result$status, 302L)) {
                        result <- tryCatch(
                            {
                                empty <- isTRUE(session$preview_empty)
                                if (!preview_first) {
                                    report("CHECK_EMPTY", "Checking whether this selection contains observations.")
                                    empty <- fedstat_preview(selected, session, ...,
                                        timeout_seconds = timeout_seconds, before = gate
                                    )
                                }
                                if (empty) {
                                    session$empty_confirmed <- TRUE
                                    report("EMPTY", "Fedstat explicitly reported zero observations for this part.", evidence = "empty_preview")
                                    data.frame(ObsValue = character(), stringsAsFactors = FALSE)
                                } else {
                                    result
                                }
                            },
                            error = identity
                        )
                    }
                    if (!inherits(result, "error")) break
                    # Ambiguous preview/export rejections can justify a smaller probe only after a
                    # session repair. GETs, explicit CSRF refusals and rate limits cannot split.
                    token <- isTRUE(result$stage %in% c("export", "grid")) && (inherits(result, "fedstat_token") ||
                        inherits(result, "fedstat_redirect") || inherits(result, "fedstat_unexpected_response"))
                    transfer <- inherits(result, "fedstat_transient") && isTRUE(result$stage %in% c("export", "grid"))
                    # Only timeouts suggest the request is too big; dropped or refused
                    # connections mean Fedstat is unwell, which a pause handles better.
                    if (transfer && !inherits(result, "fedstat_timeout")) outage <- TRUE
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
                    rejected <- repaired && isTRUE(result$stage %in% c("export", "grid")) &&
                        (inherits(result, "fedstat_redirect") || inherits(result, "fedstat_unexpected_response"))
                    splittable <- rejected || inherits(result, "fedstat_truncated") ||
                        (!outage && inherits(result, "fedstat_timeout") && isTRUE(result$stage %in% c("export", "grid")))
                    if (splittable) {
                        delay <- fedstat_backoff(retry_max_times, result)
                        session$token <- NULL
                        reason <- if (rejected) "rejected_after_refresh" else if (inherits(result, "fedstat_truncated")) "truncated" else "timeout"
                        if (split_part(index, reason)) next
                    }
                    if (recover(result)) next
                    stop(result)
                }
                failures <- recoveries <- 0L
                save_part(index, result, if (isTRUE(session$empty_confirmed)) "preview_zero_after_export_302")
            }
            phase <- "validation"
            current_part <- current_attempt <- NULL
            check_metadata()
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

# Compare the frozen selection with the page's current filters. IDs identify
# what is requested; titles are labels and rows/columns are presentation.
fedstat_compare_definitions <- function(selection, metadata) {
    fields <- unique(selection$filter_field_id)
    current_fields <- unique(metadata$filter_field_id)
    key <- function(x) fedstat_keys(x, c("filter_field_id", "filter_value_id"))
    at <- match(key(selection), key(metadata))
    found <- !is.na(at)
    title <- function(ids, x) unique(x$filter_field_title[x$filter_field_id %in% ids])
    layout <- metadata$filter_field_object_ids[match(selection$filter_field_id, metadata$filter_field_id)]
    list(
        added_fields = title(setdiff(current_fields, fields), metadata),
        removed_fields = title(setdiff(fields, current_fields), selection),
        missing = which(!found),
        renamed = which(found & selection$filter_value_title != metadata$filter_value_title[at]),
        new_title = metadata$filter_value_title[at],
        layout = ifelse(is.na(layout), selection$filter_field_object_ids, layout)
    )
}

# Shorten long value lists in user-facing messages.
fedstat_list_values <- function(values, limit = 5L) {
    values <- unique(values)
    shown <- paste(utils::head(values, limit), collapse = "; ")
    if (length(values) > limit) shown <- paste0(shown, "; and ", length(values) - limit, " more")
    shown
}

# Split the field with the most values into contiguous groups of its sorted
# value IDs. With max_cells, use the fewest groups that bring every part within
# the limit (one value per group when even that is not enough; those parts are
# split again on another field). Without it, halve: the probe after a failure.
fedstat_partition <- function(selection, rows, max_cells = NULL) {
    selected <- selection[rows, , drop = FALSE]
    all_counts <- table(selected$filter_field_id)
    counts <- all_counts[names(all_counts) != "0" & all_counts > 1L]
    if (!length(counts)) {
        return(NULL)
    }
    field <- names(counts)[which.max(counts)]
    size <- as.numeric(counts[[field]])
    groups <- 2
    if (!is.null(max_cells)) {
        other <- prod(as.numeric(all_counts[names(all_counts) != field]))
        per_group <- floor(max_cells / other)
        groups <- if (per_group < 1) size else max(2, ceiling(size / per_group))
    }
    ids <- sort(unique(selected$filter_value_id[selected$filter_field_id == field]), method = "radix")
    # Sizes of contiguous groups differ by at most one value.
    group <- floor((seq_along(ids) - 1) * groups / length(ids)) + 1
    in_field <- selected$filter_field_id == field
    structure(lapply(seq_len(groups), function(k) {
        rows[!in_field | selected$filter_value_id %in% ids[group == k]]
    }), field = field)
}

# Undo splits made after failures when none of their parts was saved: an
# outage must not leave the job with more, smaller requests on later runs.
fedstat_merge_failure_splits <- function(parts) {
    merged <- character()
    index <- 1L
    while (index <= length(parts)) {
        part <- parts[[index]]
        reason <- part$split_reason
        if (part$status == "split" && !is.null(reason) && reason != "size_limit") {
            ids <- vapply(parts, `[[`, "", "id")
            below <- which(startsWith(ids, paste0(part$id, ".")))
            if (!any(vapply(parts[below], function(p) p$status == "complete", logical(1L)))) {
                parts[[index]]$status <- "pending"
                parts[[index]]$split_reason <- NULL
                parts <- parts[-below]
                merged <- c(merged, part$id)
            }
        }
        index <- index + 1L
    }
    structure(parts, merged = merged)
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
    unavailable <- manifest$unavailable
    if (!is.null(unavailable)) unavailable <- unavailable[c("field_title", "value_title", "detected_at")]
    structure(list(
        path = path, indicator_id = manifest$indicator_id,
        complete = isTRUE(manifest$validated) && all(summary$status == "complete"), parts = summary,
        unavailable = unavailable
    ), class = "fedstat_download")
}

#' @export
print.fedstat_download <- function(x, ...) {
    cat("Fedstat ", x$indicator_id, ": ", if (x$complete) "complete" else "incomplete", "; ",
        sum(x$parts$status == "complete"), "/", nrow(x$parts), " parts, ",
        sum(x$parts$rows, na.rm = TRUE), " rows\n", x$path, "\n",
        sep = ""
    )
    if (length(x$unavailable) && nrow(x$unavailable)) {
        cat("No longer offered by Fedstat, so not downloaded: ",
            fedstat_list_values(paste0(x$unavailable$field_title, ": ", x$unavailable$value_title)), "\n",
            sep = ""
        )
    }
    invisible(x)
}

# A code keeps one label in the final table: the newest name Fedstat used.
fedstat_relabel <- function(data, dictionary) {
    if (is.null(dictionary) || !nrow(data)) {
        return(data)
    }
    for (column in grep("_code$", names(data), value = TRUE)) {
        id <- sub("_code$", "", column)
        entries <- dictionary[dictionary$field_id == id, , drop = FALSE]
        if (!id %in% names(data) || !nrow(entries)) next
        label <- entries$value_title[match(data[[column]], entries$value_id)]
        data[[id]] <- ifelse(is.na(label), data[[id]], label)
    }
    data
}

#' Get the table from a finished download
#'
#' @description Combines the saved parts of a complete [fedstat_download()] into
#'   one table. It works offline and can be repeated until you remove the parts
#'   with [fedstat_cleanup()].
#'
#'   Columns: the indicator value in `ObsValue`; one column per filter with its
#'   name, and a matching `_code` column with Fedstat's classification code; time
#'   columns such as `Time`. If Fedstat renamed a category during the download,
#'   every row uses the newest name. If Fedstat stopped offering some selected
#'   values, a message lists them.
#' @param x A [fedstat_download()] result or its folder.
#' @param remove_parts If `TRUE`, delete the saved parts after the table has been
#'   built and checked. Default `FALSE`. The table then exists only in memory
#'   until you save it; to save first and remove parts later, use [fedstat_cleanup()].
#' @param try_to_parse_ObsValue If `TRUE` (default), return `ObsValue` as numbers:
#'   decimal commas are read as decimal points and empty values become `NA`. If
#'   any other value is not a number, collection stops with examples and nothing
#'   is removed; use `FALSE` to get every value as text.
#' @return A data.frame. The whole table is loaded into memory; for very large
#'   downloads read the individual part files listed in `manifest.rds` instead.
#'   Saved parts keep the original text values.
#' @seealso [fedstat_download()], [fedstat_cleanup()]
#' @export
#' @examples
#' \dontrun{
#' data <- fedstat_collect("cpi-download")
#' saveRDS(data, "cpi.rds")
#' }
fedstat_collect <- function(x, remove_parts = FALSE, try_to_parse_ObsValue = TRUE) {
    for (name in c("remove_parts", "try_to_parse_ObsValue")) {
        value <- get(name)
        if (!is.logical(value) || length(value) != 1L || is.na(value)) {
            stop(name, " must be TRUE or FALSE.", call. = FALSE)
        }
    }
    path <- fedstat_job_path(x)
    lock <- fedstat_lock(path, "fedstat_collect()")
    on.exit(unlink(lock, recursive = TRUE), add = TRUE)
    manifest <- readRDS(file.path(path, "manifest.rds"))
    if (isTRUE(manifest$cleaned)) stop("Saved parts have already been removed. Use the table you collected and saved.", call. = FALSE)
    if (!identical(manifest$version, 1L)) stop("Unsupported download manifest version.", call. = FALSE)
    if (!isTRUE(manifest$validated) || any(vapply(manifest$parts, function(p) p$status == "pending", logical(1L)))) {
        stop("Download is incomplete; resume fedstat_download() before collecting.", call. = FALSE)
    }
    fedstat_check_parts(manifest, path)
    parts <- manifest$parts[vapply(manifest$parts, function(p) p$status == "complete", logical(1L))]
    data <- as.data.frame(data.table::rbindlist(lapply(parts, function(p) readRDS(file.path(path, p$file))),
        use.names = TRUE, fill = TRUE
    ))
    fedstat_check_data(data)
    data <- fedstat_relabel(data, manifest$dictionary)
    if (!is.null(manifest$unavailable) && nrow(manifest$unavailable)) {
        message(
            "Fedstat stopped offering some selected values during the download, so they are not in the table: ",
            fedstat_list_values(paste0(manifest$unavailable$field_title, ": ", manifest$unavailable$value_title))
        )
    }
    if (try_to_parse_ObsValue) data$ObsValue <- fedstat_numbers(data$ObsValue)
    if (remove_parts) fedstat_remove_parts(manifest, path)
    data
}

# Decimal commas become points and empty values NA. Any other non-number stops
# collection, so no value is silently turned into NA.
fedstat_numbers <- function(raw) {
    raw <- as.character(raw)
    missing <- is.na(raw) | !nzchar(trimws(raw))
    parsed <- suppressWarnings(as.numeric(gsub(",", ".", trimws(raw), fixed = TRUE)))
    bad <- unique(raw[!missing & is.na(parsed)])
    if (length(bad)) {
        stop("Some ObsValue values are not numbers, for example: ",
            paste0("\"", utils::head(bad, 5L), "\"", collapse = ", "),
            ". Nothing was removed. To get every value as text, run ",
            "fedstat_collect(job, try_to_parse_ObsValue = FALSE).",
            call. = FALSE
        )
    }
    parsed
}
