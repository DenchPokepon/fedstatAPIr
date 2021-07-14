#' Get data ids from java script source
#'
#' @param java_script_source_code character, java script source code with data ids
#'
#' @return json in form of list with data ids
#'
fedstat_java_script_data_ids_parse_to_json <- function(java_script_source_code) {
  data_ids_start <- stringr::str_which(java_script_source_code, "filters: \\{")

  data_ids_end <- stringr::str_which(java_script_source_code, "left_columns: \\[")

  java_script_data_ids_indexed <- java_script_source_code[
    seq(
      data_ids_start + 1,
      data_ids_end - 2
    )
  ]

  java_script_data_ids_json <- paste0(
    "{",
    paste(
      gsub(
        "'",
        "\"",
        gsub("\\b(?=([^']*'[^']*')*[^']*$)",
          "'",
          java_script_data_ids_indexed,
          perl = TRUE
        )
      ),
      collapse = "\n"
    ),
    "}"
  )

  java_script_data_ids_json_parsed <- jsonlite::fromJSON(java_script_data_ids_json,
    simplifyVector = FALSE
  )

  return(java_script_data_ids_json_parsed)
}
