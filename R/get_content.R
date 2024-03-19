get_content <- function(html) {
  html_body <- html |> html_element("body")

  c("style", "script", "iframe", "#CybotCookiebotDialog") |>
    purrr::walk(\(x) xml2::xml_remove(html_elements(html_body, css = x)))

  html_body |>
    html_text2() |>
    str_replace_all(fixed("&sol;"), "/") |>
    str_replace_all(fixed("&plus;"), "+")
}

get_rendered_html <- function(url) {
  session <- ChromoteSession$new()
  {
    session$Page$navigate(url, wait_ = FALSE)
    session$Page$loadEventFired()
  }
  result <- session$DOM$getDocument() |>
    purrr::pluck("root", "nodeId") |>
    session$DOM$getOuterHTML() |>
    purrr::pluck("outerHTML")
  session$close()
  result
}
