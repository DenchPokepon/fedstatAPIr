# Use HTML text and verification controls; ignore script vocabulary.
# Return a fixed evidence label so diagnostics cannot leak response credentials.
fedstat_access_marker <- function(content) {
    prefix <- tryCatch(rawToChar(head(content, 65536L)), error = function(e) "")
    if (!grepl("captcha|access denied|forbidden|verify|human|turnstile", prefix,
        ignore.case = TRUE, useBytes = TRUE
    )) {
        return(NULL)
    }
    html <- tryCatch(xml2::read_html(content, encoding = "UTF-8"), error = function(e) NULL)
    if (is.null(html)) {
        return(NULL)
    }
    xml2::xml_remove(xml2::xml_find_all(html, ".//script|.//style|.//noscript|.//template|.//comment()"))
    # Hidden indicator-page dialogs can contain verification UI even in normal pages.
    xml2::xml_remove(xml2::xml_find_all(
        html,
        ".//*[@hidden or @aria-hidden='true' or contains(translate(@style, ' ', ''), 'display:none')]"
    ))
    controls <- xml2::xml_find_all(html, ".//input|.//iframe|.//div[@data-sitekey]")
    labels <- paste(
        xml2::xml_attr(controls, "name"), xml2::xml_attr(controls, "id"),
        xml2::xml_attr(controls, "class"), xml2::xml_attr(controls, "src")
    )
    if (any(grepl("captcha|turnstile", labels, ignore.case = TRUE))) {
        return("verification_control")
    }
    text <- paste(xml2::xml_text(xml2::xml_find_all(html, ".//body//text()")), collapse = " ")
    if (grepl("\\bcaptcha\\b|verify.{0,40}(human|robot)", text, ignore.case = TRUE)) {
        return("verification_text")
    }
    if (grepl("access denied|\\bforbidden\\b", text, ignore.case = TRUE)) {
        return("access_refusal_text")
    }
    NULL
}
