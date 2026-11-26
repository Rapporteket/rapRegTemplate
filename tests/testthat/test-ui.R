test_that("app_ui builds the navbar without a registry in the URL", {
  ui <- app_ui()
  ui_text <- paste(htmltools::renderTags(ui)$html, collapse = " ")

  expect_match(ui_text, "rapRegTemplate")
  expect_match(ui_text, "Informasjon")
  expect_match(ui_text, "Samlerapport")
})

test_that("app_ui selects the registry given in the URL", {
  ui <- app_ui(list(QUERY_STRING = "registry=hjerneslag"))
  ui_text <- paste(htmltools::renderTags(ui)$html, collapse = " ")

  expect_match(ui_text, "value=\"hjerneslag\" selected")
})
