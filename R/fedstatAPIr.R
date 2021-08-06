#' Unofficial API for fedstat (EMISS) for automatic and efficient data queries
#'
#' @importFrom utils globalVariables
"_PACKAGE"

FEDSTAT_URL_BASE <- "https://www.fedstat.ru"

globalVariables(c(
  ".",
  FEDSTAT_URL_BASE
))
