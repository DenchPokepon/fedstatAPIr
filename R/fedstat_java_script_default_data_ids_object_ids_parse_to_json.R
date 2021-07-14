#' Get default data ids object ids from java script source
#'
#' @param java_script_source_code character, java script source code with data ids
#'   and default object ids in it
#'
#' @return json in form of list with 3 character vectors for
#' lineObjectIds, columnObjectIds, filterObjectIds, which consist of `filters_id`
#'
fedstat_java_script_default_data_ids_object_ids_parse_to_json <- function(java_script_source_code) {
  default_data_ids_object_ids_start <- stringr::str_which(java_script_source_code, "left_columns: \\[")

  default_data_ids_object_ids_end <- stringr::str_which(java_script_source_code, "grid\\.init\\(\\);")

  java_script_default_data_ids_object_ids_indexed <- java_script_source_code[
    seq(
      default_data_ids_object_ids_start,
      default_data_ids_object_ids_end - 2
    )
  ]

  java_script_default_data_ids_object_ids_json <- paste0(
    "{",
    paste(
      gsub(
        "'",
        "\"",
        gsub("\\b(?=([^']*'[^']*')*[^']*$)",
          "'",
          java_script_default_data_ids_object_ids_indexed,
          perl = TRUE
        )
      ),
      collapse = "\n"
    ),
    "}"
  )

  java_script_default_data_ids_object_ids_json_parsed <- jsonlite::fromJSON(
    java_script_default_data_ids_object_ids_json,
    simplifyVector = FALSE
  )

  for (i in names(java_script_default_data_ids_object_ids_json_parsed)) {
    names(java_script_default_data_ids_object_ids_json_parsed)[
      names(java_script_default_data_ids_object_ids_json_parsed) == i
    ] <- switch(i,
      "left_columns" = "lineObjectIds",
      "top_columns" = "columnObjectIds",
      "groups" = "lineObjectIds",
      i
    )
  }

  return(java_script_default_data_ids_object_ids_json_parsed)
}
