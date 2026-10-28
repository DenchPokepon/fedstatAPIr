# Compare definitions, never the page's update date. Website filter IDs and
# SDMX classification codes use different namespaces; do not equate them.
fedstat_dictionary_changed <- function(what = "Fedstat changed the filters or classifications of this indicator.") {
    fedstat_error(
        paste(
            what, "Parts saved so far can't be continued safely and are kept.",
            "Start a new download in another folder (cache_dir)."
        ), "dictionary_changed", "validation"
    )
}

# One export contradicting itself is a damaged response, not a definition change.
fedstat_inconsistent_export <- function() {
    fedstat_error(
        "Fedstat returned an inconsistent export (codes missing from its own code lists).",
        c("inconsistent", "transient"), "parse"
    )
}

# Codes identify categories; their labels may be renamed. A changed set of
# classifications changes the table itself. Field titles are not compared.
fedstat_check_dictionary <- function(previous, current) {
    if (anyNA(current[c("field_id", "value_id", "value_title")]) ||
        anyDuplicated(current[c("field_id", "value_id")])) {
        fedstat_inconsistent_export()
    }
    if (is.null(previous)) {
        return(current)
    }
    if (!setequal(previous$field_id, current$field_id)) fedstat_dictionary_changed()
    key <- function(x) fedstat_keys(x, c("field_id", "value_id"))
    index <- match(key(previous), key(current))
    shared <- which(!is.na(index))
    old <- previous$value_title[shared]
    new <- current$value_title[index[shared]]
    changed <- shared[!is.na(old) & old != new]
    # Preserve definitions absent from this export, so reappearing codes are
    # still compared. An export need not contain every value of a classification.
    result <- rbind(current, previous[is.na(index), , drop = FALSE])
    if (length(changed)) {
        attr(result, "renamed") <- data.frame(
            field_id = previous$field_id[changed], value_id = previous$value_id[changed],
            old_title = previous$value_title[changed], new_title = current$value_title[index[changed]],
            stringsAsFactors = FALSE
        )
    }
    result
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
        if (!is.null(current)) {
            dictionary <- fedstat_check_dictionary(dictionary, current)
            attr(dictionary, "renamed") <- NULL
        }
    }
    dictionary
}

# Website and SDMX labels can differ in case, spacing and HTML escaping.
fedstat_label_norm <- function(x) {
    x <- enc2utf8(as.character(x))
    entities <- c("&quot;" = "\"", "&#39;" = "'", "&#039;" = "'", "&lt;" = "<", "&gt;" = ">", "&nbsp;" = " ", "&amp;" = "&")
    for (entity in names(entities)) x <- gsub(entity, entities[[entity]], x, fixed = TRUE)
    x <- gsub(intToUtf8(160L), " ", x, fixed = TRUE) # non-breaking space
    tolower(trimws(gsub("[[:space:]]+", " ", x)))
}

# Website filter IDs and SDMX codes are different systems; bridge them by
# explicit filter names and labels only, never by guessing. Stop only on
# positive evidence: a label naming another, unrequested value of the same
# filter. Labels that match nothing are returned for logging, not treated as errors.
fedstat_check_requested_codes <- function(data, dictionary, selection, offered = selection) {
    unmatched <- NULL
    for (id in unique(dictionary$field_id)) {
        entries <- dictionary[dictionary$field_id == id, , drop = FALSE]
        fields <- unique(selection$filter_field_id[
            fedstat_label_norm(selection$filter_field_title) == fedstat_label_norm(entries$field_title[1L])
        ])
        if (length(fields) != 1L) next
        requested <- fedstat_label_norm(selection$filter_value_title[selection$filter_field_id == fields])
        available <- fedstat_label_norm(offered$filter_value_title[offered$filter_field_id == fields])
        labels <- entries$value_title[match(unique(data[[id]]), entries$value_id)]
        returned <- fedstat_label_norm(labels)
        other <- labels[returned %in% setdiff(available, requested)]
        if (length(other)) {
            fedstat_dictionary_changed(paste0(
                "Fedstat returned data for values that were not requested (",
                entries$field_title[1L], ": ", fedstat_list_values(other), "). ",
                "This usually means Fedstat changed its filters."
            ))
        }
        missing <- labels[!returned %in% available]
        if (length(missing)) {
            unmatched <- rbind(unmatched, data.frame(
                field_title = entries$field_title[1L], value_title = missing, stringsAsFactors = FALSE
            ))
        }
    }
    invisible(unmatched)
}
