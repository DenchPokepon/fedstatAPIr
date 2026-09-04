#' Database of all indicator names presented on the fedstat.ru with hierarchical grouping
#'
#' Allows researchers to search for interesting indicators more easily.
#' The database can be refreshed by the user via \code{fedstat_indicator_info()}.
#'
#' The version shipped with the package was last refreshed on 2026-09-04.
#'
#' @format A data frame with the following variables:
#' \describe{
#'   \item{department}{the name of the department from which the data comes}
#'   \item{group}{thematic grouping of the indicator}
#'   \item{id}{indicator id (used in URL and API calls)}
#'   \item{title}{indicator name/title}
#'   \item{hidden}{boolean, TRUE if indicator is hidden on the website}
#' }
#' @source \url{https://fedstat.ru/organizations/}
"fedstat_indicators_names_database"
