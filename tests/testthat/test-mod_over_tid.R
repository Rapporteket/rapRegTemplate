test_that("mod_over_tid_ui creates the expected UI structure", {
  ui <- mod_over_tid_ui("test")

  expect_s3_class(ui, "shiny.tag.list")
  ui_text <- paste(as.character(ui), collapse = " ")
  expect_match(ui_text, "test-ind_ids")
  expect_match(ui_text, "test-unitName")
  expect_match(ui_text, "test-over_tid_plot")
  expect_match(ui_text, "test-nedlastning_over_tid_plot")
})

test_that("mod_over_tid_server renders controls and plot output", {
  test_data <- data.frame(
    year = c(2022L, 2023L, 2023L, 2023L),
    orgnr = c("100", "100", "200", "200"),
    unitName = c("org1", "org1", "org2", "org2"),
    var = c(0.5, 0.6, 0.7, 0),
    denominator = c(100, 100, 200, 200),
    ind_id = c("ind_1", "ind_1", "ind_1", "ind_1"),
    stringsAsFactors = FALSE
  )

  test_user <- list(
    role = function() "SC",
    org = function() "100"
  )

  indicator_meta <- data.frame(
    ind_id = "ind_1",
    title = "Indikator A",
    short_description = "Kort tekst",
    stringsAsFactors = FALSE
  )

  local_mocked_bindings(
    plotSPC = function(...) {
      ggplot2::ggplot(data.frame(x = 1, y = 1), ggplot2::aes(x, y)) +
        ggplot2::geom_point()
    }
  )

  shiny::testServer(mod_over_tid_server, args = list(id = "test", data = test_data, indicator_meta = indicator_meta, user = test_user), {
    session$setInputs(ind_id = "ind_1")
    session$flushReact()

    expect_equal(session$input$ind_id, "ind_1")

    org_ui <- output$unitName
    expect_false(is.null(org_ui))
    org_ui_text <- paste(as.character(org_ui), collapse = " ")
    expect_match(org_ui_text, "Sykehus:")
    expect_match(org_ui_text, "org1")

    session$setInputs(unitName = "org1")
    session$flushReact()

    expect_equal(session$input$unitName, "org1")
    expect_no_error(output$over_tid_plot)
    expect_no_error(output$nedlastning_over_tid_plot)
  })
})
