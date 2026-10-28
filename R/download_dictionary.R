# Compare definitions, never the page's update date. Website filter IDs and
# SDMX classification codes use different namespaces; do not equate them.
fedstat_dictionary_changed <- function() {
    fedstat_error(
        paste0(
            "Fedstat changed or returned inconsistent filter or classification definitions. ",
            "Your saved parts are retained. Start a new download in another cache_dir ",
            "to avoid mixing definitions."
        ), "dictionary_changed", "validation"
    )
}

fedstat_check_dictionary <- function(previous, current) {
    if (anyNA(current[c("field_id", "value_id", "value_title")]) ||
        anyDuplicated(current[c("field_id", "value_id")])) {
        fedstat_dictionary_changed()
    }
    if (is.null(previous)) {
        return(current)
    }
    if (!setequal(previous$field_id, current$field_id)) fedstat_dictionary_changed()
    fields <- match(previous$field_id, current$field_id)
    old_titles <- previous$field_title
    new_titles <- current$field_title[fields]
    if (any(!is.na(old_titles) & (is.na(new_titles) | old_titles != new_titles))) fedstat_dictionary_changed()
    key <- function(x) fedstat_keys(x, c("field_id", "value_id"))
    index <- match(key(previous), key(current))
    shared <- which(!is.na(index))
    for (column in c("field_title", "value_title")) {
        old <- previous[[column]][shared]
        new <- current[[column]][index[shared]]
        if (any(!is.na(old) & (is.na(new) | old != new))) fedstat_dictionary_changed()
    }
    # Preserve definitions absent from this export, so reappearing codes are
    # still checked. An export need not contain every value of a classification.
    rbind(current, previous[is.na(index), , drop = FALSE])
}

# Existing checkpoints predate the dictionary snapshot. Recover the code/label
# pairs they actually contain; field titles were not stored in their tables.
fedstat_saved_dictionary <- function(manifest, path) {
    dictionary <- NULL
    for (part in manifest$parts) {
        if (part$status != "complete") next
        data <- readRDS(file.path(path, part$file))
        if (!nrow(data)) next
        columns <- grep("_code$", names(data), value = TRUE)
        current <- lapply(columns, function(column) {
            id <- sub("_code$", "", column)
            if (!id %in% names(data)) {
                return(NULL)
            }
            unique(data.frame(
                field_id = id, field_title = NA_character_,
                value_id = as.character(data[[column]]),
                value_title = as.character(data[[id]]), stringsAsFactors = FALSE
            ))
        })
        current <- do.call(rbind, current)
        if (!is.null(current)) dictionary <- fedstat_check_dictionary(dictionary, current)
    }
    dictionary
}

# Use explicit dimension names and category labels to bridge the two code
# systems where possible. Never guess a mapping for absent/ambiguous names.
fedstat_check_requested_codes <- function(data, dictionary, selection) {
    for (id in unique(dictionary$field_id)) {
        entries <- dictionary[dictionary$field_id == id, , drop = FALSE]
        fields <- unique(selection$filter_field_id[
            trimws(selection$filter_field_title) == trimws(entries$field_title[1L])
        ])
        if (length(fields) != 1L) next
        allowed <- trimws(selection$filter_value_title[selection$filter_field_id == fields])
        returned <- trimws(entries$value_title[match(data[[id]], entries$value_id)])
        if (anyNA(returned) || any(!returned %in% allowed)) fedstat_dictionary_changed()
    }
    invisible(TRUE)
}
