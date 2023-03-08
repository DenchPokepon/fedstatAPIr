#' Download indicator information
#'
#' @description Download indicator information from https://www.fedstat.ru/organizations/ \cr
#' Result table contains fedstat indicator id which is needed to request fedstat data \cr
#' Indicator with condition `hidden == TRUE` shows disabled records in fedstat hence ones might not be requested \cr
#'
#' @seealso \code{\link{fedstat_get_data_ids}}
#'
#' @return data.frame
#' @export
#'
#' @examples
#' \dontrun{
#' Get all indicator info
#' get_indicators()
#' }
fedstat_indicator_info <- function(){
  # workaround for `:=` and CMD check
  id = indx = NULL

  html <- xml2::read_html("https://www.fedstat.ru/organizations/")
  items <- xml2::xml_find_all(html, xpath = '//*[@id="orgsTree"]/div')

  list_dt <- lapply(items, function(x){
    # x <- items[[1]]

    children <- xml2::xml_child(x, 'div[@class="ved_child"]') |> xml2::xml_children()

    sublist_dt <- lapply(children, function(y){
      # y <- children[[1]]

      sub_children <- xml2::xml_child(y, 'div[@class="ved_child"]') |>
        xml2::xml_find_all('div//*[@class="ved_item group i_actual"]')

      data.table(
        department =  xml2::xml_child(x, 'div//*[@class="i_name org"]') |> xml2::xml_text(),
        group = xml2::xml_child(y, 'div[@class="ved_pl dtable group"]//*[@class="i_name org" or @class="i_name ci"]') |> xml2::xml_text(),
        id = xml2::xml_find_all(sub_children, 'a') |> xml2::xml_attr("href") |> sub(x = _, "/indicator/", ""),
        title = xml2::xml_find_all(sub_children, 'a//*[@class="i_name"]') |> xml2::xml_text()#,
        #active = xml_attr(sub_children, "class") == "ved_item group i_actual"
      )
    })

    rbindlist(sublist_dt, use.names = TRUE)
  }) %>% suppressWarnings()

  rbindlist(list_dt, use.names = TRUE)[!is.na(id), ][
    ,`:=`(hidden = fifelse(id == "#", TRUE, FALSE), indx =1)][
      , indx := cumsum(indx), by = "id"][
        , `:=`(id = fifelse(id == "#", paste0(id, indx), id), indx = NULL)] |>
    unique(by = c("department", "group", "id"))
}
