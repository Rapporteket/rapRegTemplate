test_that("info_ui creates the expected UI structure", {
  ui <- info_ui("test")
  ui_text <- paste(as.character(ui), collapse = " ")

  expect_match(ui_text, "Informasjon om Rapporteket")
  expect_match(ui_text, "test-info")
  expect_match(ui_text, "test-registry")
  expect_match(ui_text, "test-load")
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
  local_mocked_bindings(
    fetchSkdeRegistryNames = function() c("hjertestans", "hjerneslag")
  )

  shiny::testServer(info_server, args = list(id = "test", user = list()), {
    out <- output$info

    expect_match(paste(as.character(out), collapse = " "), "Informasjon om rapporteket")
  })

  expect_true(grepl("inst/info\\.Rmd$|info\\.Rmd$", captured$file))
  expect_equal(captured$outputType, "html_fragment")
})

test_that("info_server fills the registry select from the names API", {
  captured <- NULL
  local_mocked_bindings(
    fetchSkdeRegistryNames = function() c("hjerneslag", "hjertestans")
  )
  local_mocked_bindings(
    updateSelectInput = function(session, inputId, label = NULL, choices = NULL, selected = NULL) {
      captured <<- list(inputId = inputId, choices = choices, selected = selected)
    },
    .package = "shiny"
  )

  shiny::testServer(info_server, args = list(id = "test", user = list()), {
    session$setInputs(registry = "hjerneslag")
  })

  expect_equal(captured$inputId, "registry")
  expect_equal(captured$choices, c("hjerneslag", "hjertestans"))
  expect_equal(captured$selected, "hjertestans")
})

test_that("info_server selects the first name when hjertestans is missing", {
  captured <- NULL
  local_mocked_bindings(
    fetchSkdeRegistryNames = function() c("hjerneslag", "annet")
  )
  local_mocked_bindings(
    updateSelectInput = function(session, inputId, label = NULL, choices = NULL, selected = NULL) {
      captured <<- list(choices = choices, selected = selected)
    },
    .package = "shiny"
  )

  shiny::testServer(info_server, args = list(id = "test", user = list()), {
    session$setInputs(registry = "hjerneslag")
  })

  expect_equal(captured$selected, "hjerneslag")
})

test_that("info_server leaves the select unchanged when the names API fails", {
  captured <- NULL
  local_mocked_bindings(
    fetchSkdeRegistryNames = function() stop("nettverksfeil")
  )
  local_mocked_bindings(
    updateSelectInput = function(session, inputId, label = NULL, choices = NULL, selected = NULL) {
      captured <<- TRUE
    },
    .package = "shiny"
  )

  shiny::testServer(info_server, args = list(id = "test", user = list()), {
    session$setInputs(registry = "hjerneslag")
  })

  expect_null(captured)
})

test_that("info_server pushes the chosen registry to the URL and reloads", {
  captured <- NULL
  local_mocked_bindings(
    fetchSkdeRegistryNames = function() c("hjertestans", "hjerneslag")
  )
  local_mocked_bindings(
    updateQueryString = function(queryString, mode = "replace", session = NULL) {
      captured <<- list(queryString = queryString, mode = mode)
    },
    .package = "shiny"
  )

  shiny::testServer(info_server, args = list(id = "test", user = list()), {
    session$setInputs(registry = "hjerneslag")
    session$setInputs(load = 1)
  })

  expect_equal(captured$queryString, "?registry=hjerneslag")
  expect_equal(captured$mode, "push")
})
