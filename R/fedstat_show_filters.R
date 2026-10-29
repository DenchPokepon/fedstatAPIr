#' Show the filters of an indicator, ready to copy into your code
#'
#' @description Prints every filter of an indicator with the number of its
#'   values and a few examples, followed by a `filters <- list(...)` you can copy
#'   into [fedstat_download()]. That list repeats the selection the website shows
#'   when the indicator page opens; edit it to choose what you need.
#'
#' @param indicator_id Indicator code from its page address, for example
#'   `"31074"`, or the result of [fedstat_get_data_ids()].
#' @param ... httr settings applied to the page request, for example
#'   `httr::user_agent("my-project")`. Arguments after `...` must be named in full.
#' @param timeout_seconds Longest wait for the page, in seconds.
#' @param retry_max_times Attempts to open the page, including the first.
#'
#' @return Invisibly, the printed `filters` list. Filters with only one value
#'   are left out: they are selected automatically.
#' @seealso [fedstat_dictionary()] for the codes used in the data.
#' @export
#'
#' @examples
#' \dontrun{
#' filters <- fedstat_show_filters("31074")
#' job <- fedstat_download("31074", filters)
#'
#' # All values of one filter
#' ids <- fedstat_get_data_ids("31074")
#' unique(ids[c("filter_field_title", "filter_value_title")])
#' }
fedstat_show_filters <- function(indicator_id, ..., timeout_seconds = 180, retry_max_times = 3L) {
    if (is.data.frame(indicator_id)) {
        data_ids <- indicator_id
        required <- c("filter_field_id", "filter_field_title", "filter_value_id", "filter_value_title")
        if (!all(required %in% names(data_ids)) || !nrow(data_ids)) {
            stop("indicator_id must be an indicator code or the result of fedstat_get_data_ids().", call. = FALSE)
        }
    } else {
        fedstat_check_dots(list(...), "fedstat_show_filters")
        data_ids <- fedstat_read_ids(fedstat_indicator_id(indicator_id), ...,
            timeout_seconds = timeout_seconds, retry_max_times = retry_max_times
        )
    }
    width <- max(40L, getOption("width", 80L))
    literal <- function(x) paste0("\"", gsub("([\"\\\\])", "\\\\\\1", x), "\"")
    fit <- function(x) ifelse(nchar(x, "width") > width, paste0(strtrim(x, width - 3L), "..."), x)
    indicator <- data_ids[data_ids$filter_field_id == "0", , drop = FALSE]
    fields <- data_ids[data_ids$filter_field_id != "0", , drop = FALSE]
    titles <- split(fields$filter_value_title, factor(fields$filter_field_id, unique(fields$filter_field_id)))
    names(titles) <- fields$filter_field_title[match(names(titles), fields$filter_field_id)]
    many <- lengths(titles) > 1L

    if (nrow(indicator)) {
        cat(fit(paste0("Indicator ", indicator$filter_value_id[1L], ": ", indicator$filter_value_title[1L])), "\n\n", sep = "")
    }
    cat("Filters and their values (use them in Russian, as written here):\n", sep = "")
    for (name in names(titles)[order(!many)]) {
        values <- titles[[name]]
        shown <- if (length(values) > 4L) c(literal(values[1:3]), "...", literal(values[length(values)])) else literal(values)
        count <- if (length(values) > 1L) paste0(fedstat_count(length(values)), " values: ") else "one value, selected automatically: "
        cat(fit(paste0("  ", literal(name), ": ", count, paste(shown, collapse = ", "))), "\n", sep = "")
    }

    selected <- fedstat_default_selection(data_ids)
    filters <- lapply(names(titles)[many], function(name) {
        rows <- fields$filter_field_title == name
        chosen <- selected$filter_value_title[selected$filter_field_title == name]
        if (length(chosen) == sum(rows)) "*" else chosen
    })
    names(filters) <- names(titles)[many]
    if (!length(filters)) {
        cat("\nEvery filter has one value: use filters = list().\n")
        return(invisible(list()))
    }
    entry <- function(name, values) {
        line <- paste0("    ", literal(name), " = ", if (length(values) == 1L) literal(values) else paste0("c(", paste(literal(values), collapse = ", "), ")"))
        if (length(values) == 1L || nchar(line, "width") <= width) {
            return(line)
        }
        c(paste0("    ", literal(name), " = c("), paste0("        ", literal(values), c(rep(",", length(values) - 1L), "")), "    )")
    }
    lines <- lapply(names(filters), function(name) entry(name, filters[[name]]))
    last <- length(lines)
    lines[-last] <- lapply(lines[-last], function(x) {
        x[length(x)] <- paste0(x[length(x)], ",")
        x
    })
    cat("\n# The selection the website shows first (", fedstat_count(fedstat_nominal_cells(selected)),
        " filter combinations). \"*\" selects every value.\n",
        sep = ""
    )
    cat("filters <- list(\n", paste0(unlist(lines), "\n"), ")\n", sep = "")
    invisible(filters)
}
