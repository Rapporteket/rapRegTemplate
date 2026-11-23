test_that("info_ui creates the expected UI structure", {
  ui <- info_ui("test")
  ui_text <- paste(as.character(ui), collapse = " ")

  expect_match(ui_text, "Informasjon om Rapporteket")
  expect_match(ui_text, "test-info")
})

test_that("info_server renders the information R Markdown", {
  captured <- NULL
  local_mocked_bindings(
    renderRmd = function(file, outputType) {
      captured <<- list(file = file, outputType = outputType)
      shiny::HTML("<p>Informasjon om rapporteket</p>")
    },
    .package = "rapbase"
  )

  shiny::testServer(info_server, args = list(id = "test", user = list()), {
    out <- output$info

    expect_match(paste(as.character(out), collapse = " "), "Informasjon om rapporteket")
  })

  expect_true(grepl("inst/info\\.Rmd$|info\\.Rmd$", captured$file))
  expect_equal(captured$outputType, "html_fragment")
})
