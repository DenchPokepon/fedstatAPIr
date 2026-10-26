fedstat_token <- function(html, indicator_id, handle, base_url) {
    holder <- xml2::xml_find_first(html, ".//div[@id='downloadTokenHolder']")
    inputs <- xml2::xml_find_all(holder, ".//input")
    names <- xml2::xml_attr(inputs, "name")
    values <- xml2::xml_attr(inputs, "value")
    name <- values[match("struts.token.name", names)]
    token <- values[match(name, names)]
    if (is.na(name) || is.na(token) || !nzchar(name) || !nzchar(token)) {
        fedstat_error(
            "No download token on the indicator page; the page may be unavailable or have changed.",
            "metadata", "token"
        )
    }
    session <- new.env(parent = emptyenv())
    session$token_name <- name
    session$token <- token
    session$handle <- handle
    session$base_url <- base_url
    session$indicator_id <- as.character(indicator_id)
    session$used <- FALSE
    session
}

fedstat_attach_session <- function(data_ids, session) {
    attr(data_ids, "fedstat_session") <- session
    attr(data_ids, "fedstat_csrf_token") <- session$token
    attr(data_ids, "fedstat_csrf_token_name") <- session$token_name
    attr(data_ids, "fedstat_indicator_id") <- session$indicator_id
    attr(data_ids, "fedstat_handle") <- session$handle
    attr(data_ids, "fedstat_base_url") <- session$base_url
    data_ids
}

fedstat_session <- function(data_ids) {
    session <- attr(data_ids, "fedstat_session")
    if (is.environment(session)) {
        return(session)
    }
    session <- new.env(parent = emptyenv())
    session$token <- attr(data_ids, "fedstat_csrf_token")
    session$token_name <- attr(data_ids, "fedstat_csrf_token_name")
    session$indicator_id <- attr(data_ids, "fedstat_indicator_id")
    if (is.null(session$indicator_id)) {
        session$indicator_id <- as.character(data_ids$filter_value_id[data_ids$filter_field_id == "0"])
    }
    if (length(session$indicator_id) != 1L || is.na(session$indicator_id)) {
        stop("data_ids must identify exactly one indicator.", call. = FALSE)
    }
    session$base_url <- attr(data_ids, "fedstat_base_url")
    if (is.null(session$base_url)) session$base_url <- "https://www.fedstat.ru"
    session$handle <- attr(data_ids, "fedstat_handle")
    if (is.null(session$handle)) {
        session$handle <- httr::handle(session$base_url)
        session$token <- NULL
    }
    session$used <- FALSE
    session
}

fedstat_refresh <- function(session, ..., timeout_seconds, attempts = 1L,
                            before = function() NULL) {
    fedstat_event(before, "TOKEN_REFRESH", "Obtaining a fresh token; resolved filters are retained.", stage = "token")
    response <- fedstat_request("GET", paste0(session$base_url, "/indicator/", session$indicator_id),
        ...,
        handle = session$handle, timeout_seconds = timeout_seconds, attempts = attempts,
        stage = "token", before = before
    )
    fresh <- fedstat_token(
        xml2::read_html(response$content, encoding = "UTF-8"),
        session$indicator_id, session$handle, session$base_url
    )
    session$token <- fresh$token
    session$token_name <- fresh$token_name
    session$used <- FALSE
    invisible(session)
}
