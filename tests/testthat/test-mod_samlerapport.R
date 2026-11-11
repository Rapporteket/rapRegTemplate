make_samler_data <- function() {
  grid <- expand.grid(
    year = 2022:2023,
    orgnr = c("111", "222"),
    ind_id = c("ind_1", "ind_2"),
    stringsAsFactors = FALSE
  )
  # orgnr 333 only has one indicator and must be excluded from the choices
  extra <- data.frame(
    year = 2023L,
    orgnr = "333",
    ind_id = "ind_1",
    stringsAsFactors = FALSE
  )
  grid <- rbind(grid, extra)
  grid$unitName <- c(
    "111" = "Sykehus A", "222" = "Sykehus B", "333" = "Sykehus C"
  )[grid$orgnr]
  grid$var <- 0.5
  grid$denominator <- 100
  grid
}

make_samler_meta <- function() {
  data.frame(
    ind_id = c("ind_1", "ind_2"),
    title = c("Indikator A", "Indikator B"),
    stringsAsFactors = FALSE
  )
}

make_samler_user <- function(role = "SC", org = "111") {
  list(
    role = shiny::reactive(role),
    org = shiny::reactive(org)
  )
}

test_that("samlerapport_ui creates the expected UI structure", {
  ui <- samlerapport_ui("test")
  ui_text <- paste(as.character(ui), collapse = " ")

  expect_match(ui_text, "Samlerapport")
  expect_match(ui_text, "test-unitName")
  expect_match(ui_text, "test-formatS")
  expect_match(ui_text, "test-downloadSamlerapport")
  expect_match(ui_text, "test-samlerapport")
  expect_match(ui_text, "Velg format for nedlasting")
})

test_that("unitName lists only organisations with at least two indicators (SC)", {
  shiny::testServer(
    samlerapport_server,
    args = list(
      id = "test",
      data = make_samler_data(),
      indicator_meta = make_samler_meta(),
      user = make_samler_user("SC")
    ),
    {
      ui_text <- paste(as.character(output$unitName), collapse = " ")

      expect_match(ui_text, "Sykehus:")
      expect_match(ui_text, "Sykehus A")
      expect_match(ui_text, "Sykehus B")
      expect_no_match(ui_text, "Sykehus C")
      # first organisation is selected by default
      expect_match(ui_text, "value=\"111\" selected")
    }
  )
})

test_that("data_reactive is filtered on org for non-SC users", {
  shiny::testServer(
    samlerapport_server,
    args = list(
      id = "test",
      data = make_samler_data(),
      indicator_meta = make_samler_meta(),
      user = make_samler_user("LU", "222")
    ),
    {
      expect_true(all(data_reactive()$orgnr == "222"))

      ui_text <- paste(as.character(output$unitName), collapse = " ")
      expect_match(ui_text, "Sykehus B")
      expect_no_match(ui_text, "Sykehus A")
    }
  )
})

test_that("data_reactive returns all data for SC users", {
  d <- make_samler_data()
  shiny::testServer(
    samlerapport_server,
    args = list(
      id = "test",
      data = d,
      indicator_meta = make_samler_meta(),
      user = make_samler_user("SC")
    ),
    {
      expect_equal(nrow(data_reactive()), nrow(d))
    }
  )
})

test_that("unitName keeps a valid current selection", {
  shiny::testServer(
    samlerapport_server,
    args = list(
      id = "test",
      data = make_samler_data(),
      indicator_meta = make_samler_meta(),
      user = make_samler_user("SC")
    ),
    {
      session$setInputs(unitName = "222")
      ui_text <- paste(as.character(output$unitName), collapse = " ")
      expect_match(ui_text, "value=\"222\" selected")
    }
  )
})

test_that("unitName falls back to first choice on invalid selection", {
  shiny::testServer(
    samlerapport_server,
    args = list(
      id = "test",
      data = make_samler_data(),
      indicator_meta = make_samler_meta(),
      user = make_samler_user("SC")
    ),
    {
      session$setInputs(unitName = "999")
      ui_text <- paste(as.character(output$unitName), collapse = " ")
      expect_match(ui_text, "value=\"111\" selected")
    }
  )
})

test_that("unitName stops silently when no organisation qualifies", {
  d <- make_samler_data()
  d <- d[d$orgnr == "333", ]
  shiny::testServer(
    samlerapport_server,
    args = list(
      id = "test",
      data = d,
      indicator_meta = make_samler_meta(),
      user = make_samler_user("SC")
    ),
    {
      expect_error(output$unitName, class = "shiny.silent.error")
    }
  )
})

test_that("samlerapport output calls reportProcessor with correct arguments", {
  captured <- NULL
  local_mocked_bindings(
    reportProcessor = function(...) {
      captured <<- list(...)
      shiny::HTML("<p>fragment</p>")
    }
  )

  shiny::testServer(
    samlerapport_server,
    args = list(
      id = "test",
      data = make_samler_data(),
      indicator_meta = make_samler_meta(),
      user = make_samler_user("SC")
    ),
    {
      session$setInputs(unitName = "111")
      out <- output$samlerapport

      expect_match(paste(as.character(out), collapse = " "), "fragment")
      expect_equal(captured$report, "local_monthly")
      expect_equal(captured$outputType, "html_fragment")
      expect_equal(captured$orgnr, "111")
      expect_equal(captured$indicator_meta, make_samler_meta())
      expect_equal(nrow(captured$data), nrow(make_samler_data()))
    }
  )
})

test_that("samlerapport output requires a selected unit", {
  local_mocked_bindings(
    reportProcessor = function(...) shiny::HTML("<p>fragment</p>")
  )

  shiny::testServer(
    samlerapport_server,
    args = list(
      id = "test",
      data = make_samler_data(),
      indicator_meta = make_samler_meta(),
      user = make_samler_user("SC")
    ),
    {
      expect_error(output$samlerapport, class = "shiny.silent.error")
    }
  )
})

test_that("download handler renders the report in the chosen format", {
  captured <- NULL
  local_mocked_bindings(
    reportProcessor = function(...) {
      captured <<- list(...)
      fn <- tempfile(fileext = paste0(".", captured$outputType))
      writeLines("report", fn)
      fn
    }
  )

  shiny::testServer(
    samlerapport_server,
    args = list(
      id = "test",
      data = make_samler_data(),
      indicator_meta = make_samler_meta(),
      user = make_samler_user("SC")
    ),
    {
      session$setInputs(unitName = "222", formatS = "html")
      path <- output$downloadSamlerapport

      expect_true(file.exists(path))
      expect_equal(captured$report, "local_monthly")
      expect_equal(captured$outputType, "html")
      expect_equal(captured$orgnr, "222")
      expect_equal(readLines(path), "report")
    }
  )
})

test_that("download handler filename uses the chosen format", {
  filename_fn <- NULL
  original_download_handler <- shiny::downloadHandler
  local_mocked_bindings(
    downloadHandler = function(filename, content, ...) {
      filename_fn <<- filename
      original_download_handler(filename = filename, content = content, ...)
    },
    .package = "shiny"
  )

  shiny::testServer(
    samlerapport_server,
    args = list(
      id = "test",
      data = make_samler_data(),
      indicator_meta = make_samler_meta(),
      user = make_samler_user("SC")
    ),
    {
      session$setInputs(unitName = "111", formatS = "pdf")
      expect_false(is.null(filename_fn))
      expect_match(filename_fn(), "^rapRegTemplateSamlerapport.*\\.pdf$")

      session$setInputs(formatS = "html")
      expect_match(filename_fn(), "\\.html$")
    }
  )
})
