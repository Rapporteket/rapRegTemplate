test_that("mod_andeler_ui creates the expected UI structure", {
  ui <- mod_andeler_ui("test")

  expect_s3_class(ui, "shiny.tag.list")
  ui_text <- paste(as.character(ui), collapse = " ")
  expect_match(ui_text, "Andel basert på variabel og grenser")
  expect_match(ui_text, "downloadandelPlot")
  expect_match(ui_text, "test-ind_ids")
})

test_that("mod_andeler_server is a valid Shiny module function", {
  expect_true(is.function(mod_andeler_server))
})
