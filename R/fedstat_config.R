# Internal function providing default HTTP headers for requests to fedstat.ru
# Users can override these defaults via the ... argument in exported functions,
# or globally via httr::set_config(httr::add_headers(...)).
#
# Not exported.
fedstat_default_config <- function() {
  httr::add_headers(
    "User-Agent"      = "fedstatAPIr/1.1.0 (R package; https://github.com/DenchPokepon/fedstatAPIr)",
    "Accept"          = "text/html,application/xhtml+xml,application/xml;q=0.9,*/*;q=0.8",
    "Accept-Language" = "ru-RU,ru;q=0.9,en-US;q=0.8,en;q=0.7",
    "Connection"      = "keep-alive"
  )
}
