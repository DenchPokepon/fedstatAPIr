#' @importFrom magrittr %>%
#' @export
magrittr::`%>%`

anti_join <- function(x, y, by) {
    if (!inherits(x, "data.table") || !inherits(y, "data.table")) {
        stop("anti_join expects x and y to be data.tables")
    }

    return(x[!y, on = by])
}


semi_join <- function(x, y, by) {
    if (!inherits(x, "data.table") || !inherits(y, "data.table")) {
        stop("semi_join expects x and y to be data.tables")
    }

    w <- unique(x[y, on = by, nomatch = 0, which = TRUE, allow.cartesian = TRUE])
    return(x[w])
}
