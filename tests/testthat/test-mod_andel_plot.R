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

test_that("mod_andeler_server renders the indicator selection and plot output", {
  test_data <- data.frame(
    year = c(2022L, 2023L, 2023L),
    orgnr = c("org1", "org1", "org2"),
    var = c(0.5, 0.6, 0.7),
    denominator = c(100, 100, 200),
    ind_id = c("ind_1", "ind_1", "ind_1"),
    context = c("all", "all", "all"),
    title = "Indikator A",
    short_description = "Kort tekst",
    levelDirection = 1,
    kvalIndgrenser = I(list(c(0.2, 0.6), c(0.2, 0.6), c(0.2, 0.6)))
  )

  indicator_meta <- data.frame(
    ind_id = "ind_1",
    title = "Indikator A",
    short_description = "Kort tekst",
    levelDirection = 1,
    kvalIndgrenser = I(list(c(0.2, 0.6))),
    stringsAsFactors = FALSE
  )

  local_mocked_bindings(
    fetchSkdeIndicatorData = function(...) test_data
  )

  shiny::testServer(mod_andeler_server, args = list(id = "test", data = test_data, indicator_meta = indicator_meta), {
    session$setInputs(ind_id = "ind_1")
    session$flushReact()

    expect_equal(session$input$ind_id, "ind_1")

    ind_ui <- output$ind_ids
    expect_false(is.null(ind_ui))
    ind_ui_text <- paste(as.character(ind_ui), collapse = " ")
    expect_gt(nchar(ind_ui_text), 0)
    expect_match(ind_ui_text, "Indikator:")
    expect_match(ind_ui_text, "ind_1")

    expect_no_error(output$andelPlot)
    expect_no_error(output$downloadandelPlot)
  })
})
