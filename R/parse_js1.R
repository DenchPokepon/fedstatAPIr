#' Get data ids from java script source
#'
#' @param script character, java script source code with data ids
#'
#' @return json in form of list with data ids
#'
parse_js1 <- function(script){

  script_indexed <- script[seq(grep("filters: \\{", script) + 1, grep("left_columns: \\[", script) - 2)]

  gsub("\\b(?=([^']*'[^']*')*[^']*$)", "'", script_indexed, perl = TRUE) %>%
    gsub("'", "\"", x = .)  %>%
    paste(collapse = "\n") %>%
    paste0("{", ... = ., "}") %>%
    jsonlite::fromJSON(simplifyVector = FALSE)
}
