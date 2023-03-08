#' Get default data ids object ids from java script source
#'
#' @param script character, java script source code with data ids
#'   and default object ids in it
#'
#' @return json in form of list with 3 character vectors for
#' lineObjectIds, columnObjectIds, filterObjectIds, which consist of `filters_id`
#'
parse_js2 <- function(script) {
  script_indexed <- script[seq(grep("left_columns: \\[", script), grep("grid\\.init\\(\\);", script) - 2)]

  js_prep <- gsub("\\b(?=([^']*'[^']*')*[^']*$)", "'", script_indexed, perl = TRUE) %>%
    gsub("'", "\"", x = .) %>%
    paste(collapse = "\n") %>%
    paste0("{", ... = ., "}") %>%
    jsonlite::fromJSON(simplifyVector = FALSE)

  new_names <- c("left_columns" = "lineObjectIds",
                 "top_columns" = "columnObjectIds",
                 "groups" = "lineObjectIds",
                 "filterObjectIds" = "lineObjectIds")

  names(js_prep) <- new_names[names(js_prep)]

  return(js_prep)
}
