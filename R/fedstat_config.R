# Internal function providing default HTTP headers for requests to fedstat.ru
# Users can override these defaults via the ... argument in exported functions,
# or globally via httr::set_config(httr::add_headers(...)).
#
# Not exported.
fedstat_default_config <- function() {
    defaults <- c(
        httr::user_agent(paste0(
            "fedstatAPIr/", utils::packageVersion("fedstatAPIr"),
            " (https://github.com/DenchPokepon/fedstatAPIr)"
        )),
        httr::add_headers("Accept-Language" = "ru-RU,ru;q=0.9,en;q=0.7")
    )
    configs <- list(defaults, getOption("httr_config"), getOption("fedstatAPIr.config"))
    c(Reduce(c, configs), fedstat_user_agent(configs))
}

fedstat_user_agent <- function(configs) {
    agent <- NULL
    for (config in configs) {
        if (inherits(config, "request")) {
            if (!is.null(config$options$useragent)) agent <- config$options$useragent
            headers <- config$headers[tolower(names(config$headers)) == "user-agent"]
            if (length(headers)) agent <- unname(tail(headers, 1L))
        }
    }
    # An explicit HTTP header otherwise overrides CURLOPT_USERAGENT, even when
    # the latter came from a more specific per-call httr::user_agent().
    if (!is.null(agent)) httr::add_headers("User-Agent" = agent)
}
