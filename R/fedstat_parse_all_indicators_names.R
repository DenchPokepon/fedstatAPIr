#' Parses https://fedstat.ru/organizations/ indicators hierarchical list into data.frame for reference
#'
#' @param url https://fedstat.ru/organizations/
#' @param ... passed to httr::GET
#'
#' @return data.frame, database of all indicators on fedstat.ru with hierarchical structure
fedstat_parse_all_indicators_names <- function(url = "https://fedstat.ru/organizations/", ...) {
  GET_res <- httr::GET(url, ... = ...)

  if (httr::http_error(GET_res)) stop("httr::http_error")

  html_parsed <- xml2::read_html(GET_res)

  html_orgs_tree <- xml2::xml_children(rvest::html_node(html_parsed, "#orgsTree"))

  number_of_org <- xml2::xml_length(html_orgs_tree)


  recursive_agregator <- function(ved_xml_nodeset) {
    ved_xml_nodeset_len <- length(ved_xml_nodeset)

    if (!methods::is(ved_xml_nodeset, "xml_nodeset")) stop("error")
    if (ved_xml_nodeset_len == 0) {
      return(NA)
    }

    ved_xml <- ved_xml_nodeset[[1]]

    if (rvest::html_attr(ved_xml, "class") == "ved_item group i_actual"
    | rvest::html_attr(ved_xml, "class") == "ved_item group i_excluded hide") {
      if (rvest::html_attr(ved_xml, "class") == "ved_item group i_excluded hide") {
        excluded_mark <- " [\u0418\u0421\u041a\u041b\u042e\u0427\u0415\u041d]" # iskluchen
      } else {
        excluded_mark <- ""
      }

      ved_xml_child <- xml2::xml_child(ved_xml, 1)

      result <- list(
        list(href = rvest::html_attr(ved_xml_child, "href")) %>%
          `names<-`(
            paste0(
              rvest::html_text(rvest::html_node(rvest::html_node(ved_xml_child, ".ved_txt.dcell"), ".i_name")),
              excluded_mark,
              "////"
            )
          )
      )
    } else if (rvest::html_attr(ved_xml, "class") == "ved_item" |
      rvest::html_attr(ved_xml, "class") == "ved_item group_excluded") {
      if (rvest::html_attr(ved_xml, "class") == "ved_item group_excluded") {
        excluded_mark <- " [\u0418\u0421\u041a\u041b\u042e\u0427\u0415\u041d]" # iskluchen
      } else {
        excluded_mark <- ""
      }

      result <- list(
        list(ved = recursive_agregator(xml2::xml_children(rvest::html_node(ved_xml, ".ved_child")))) %>%
          `names<-`(
            paste0(
              rvest::html_text(rvest::html_node(rvest::html_node(ved_xml, ".ved_pl.dtable"), ".ved_txt.dcell")),
              excluded_mark,
              "////"
            )
          )
      )
    }

    if (ved_xml_nodeset_len == 1) {
      return(result)
    } else {
      return(c(result, recursive_agregator(ved_xml_nodeset[-1])))
    }
  }


  data_aggregated <- recursive_agregator(html_orgs_tree)

  data_aggregated_unlist <- unlist(data_aggregated)

  names_split <- strsplit(gsub("////\\.", "////", names(data_aggregated_unlist)), "////")

  # max level hierarchy 6
  # max(sapply(names_split, length, simplify = TRUE))

  data_aggregated_data_frame <- data.frame(
    name = sapply(names_split, function(x) x[length(x)], simplify = TRUE),
    url = paste0("https://fedstat.ru/", gsub("^/", "", data_aggregated_unlist)),
    excluded = grepl(
      " \\[\u0418\u0421\u041a\u041b\u042e\u0427\u0415\u041d\\]",
      sapply(names_split, function(x) x[length(x)], simplify = TRUE)
    ),
    department = sapply(names_split, function(x) x[1], simplify = TRUE),
    group_level_2 = sapply(names_split, function(x) x[2], simplify = TRUE),
    group_level_3 = sapply(names_split, function(x) x[3], simplify = TRUE),
    group_level_4 = sapply(names_split, function(x) x[4], simplify = TRUE),
    group_level_5 = sapply(names_split, function(x) x[5], simplify = TRUE),
    group_level_6 = sapply(names_split, function(x) x[6], simplify = TRUE),
    stringsAsFactors = FALSE
  )

  return(data_aggregated_data_frame)
}
