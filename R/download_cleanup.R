#' Remove the saved parts of a completed download
#'
#' @param x A [fedstat_download()] result or its directory path.
#' @return Invisibly, the download directory. Only files named in the manifest
#'   are removed; the manifest, diagnostic log and unrelated files remain.
#' @details Save the collected table before calling this function. The download
#'   cannot be resumed or collected again after cleanup. Incomplete, changed or
#'   actively used downloads are not cleaned. Cleanup can be repeated if interrupted.
#' @export
#' @examples
#' \dontrun{
#' data <- fedstat_collect("cpi-download")
#' saveRDS(data, "cpi.rds")
#' fedstat_cleanup("cpi-download")
#' }
fedstat_cleanup <- function(x) {
    path <- fedstat_job_path(x)
    lock <- fedstat_collect_lock(path)
    on.exit(unlink(lock, recursive = TRUE), add = TRUE)
    manifest <- readRDS(file.path(path, "manifest.rds"))
    if (!identical(manifest$version, 1L)) stop("Unsupported download manifest version.", call. = FALSE)
    if (!isTRUE(manifest$cleaned)) {
        if (any(vapply(manifest$parts, function(p) p$status == "pending", logical(1L)))) {
            stop("Download is incomplete; saved parts are needed to resume.", call. = FALSE)
        }
        fedstat_check_parts(manifest, path)
        if (!isTRUE(manifest$validated)) fedstat_validate_parts(manifest, path)
    }
    fedstat_remove_parts(manifest, path)
    invisible(path)
}

fedstat_job_path <- function(x) {
    path <- if (inherits(x, "fedstat_download")) x$path else x
    if (!is.character(path) || length(path) != 1L || is.na(path) || !dir.exists(path)) {
        stop("Use a download result or an existing download folder.", call. = FALSE)
    }
    normalizePath(path, winslash = "/", mustWork = TRUE)
}

fedstat_collect_lock <- function(path) {
    lock <- file.path(path, ".lock")
    if (!dir.create(lock, showWarnings = FALSE)) {
        stop("This download folder is in use. Wait for the other operation to finish.", call. = FALSE)
    }
    lock
}

fedstat_remove_parts <- function(manifest, path) {
    parts <- Filter(function(p) p$status == "complete", manifest$parts)
    # Validate every target before removing anything, including old flat layouts.
    files <- vapply(parts, function(p) {
        name <- p$file
        if (!is.character(name) || length(name) != 1L || is.na(name) ||
            !grepl("^(parts/)?part-[0-9.]+[.]rds$", name)) {
            stop("Unsafe saved-part path; nothing was removed.", call. = FALSE)
        }
        file <- file.path(path, name)
        parent <- normalizePath(dirname(file), winslash = "/", mustWork = TRUE)
        expected <- if (dirname(name) == "parts") paste0(path, "/parts") else path
        link <- Sys.readlink(file)
        if (!identical(parent, expected) || (!is.na(link) && nzchar(link)) || dir.exists(file)) {
            stop("Saved part points to another location; nothing was removed.", call. = FALSE)
        }
        if (file.exists(file) && !identical(unname(tools::md5sum(file)), p$checksum)) {
            stop("Saved part changed; nothing was removed.", call. = FALSE)
        }
        file
    }, "")
    # Mark first so an interrupted deletion cannot masquerade as resumable data.
    manifest$cleaned <- TRUE
    manifest$validated <- TRUE
    fedstat_save(manifest, file.path(path, "manifest.rds"))
    unlink(files)
    if (any(file.exists(files))) {
        warning("Some saved parts could not be removed. Your collected table is unaffected; repeat fedstat_cleanup() to retry.",
            call. = FALSE, immediate. = TRUE
        )
    }
    invisible(NULL)
}
