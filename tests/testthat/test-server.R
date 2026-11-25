make_server_data <- function() {
  data.frame(
    year = 2023L,
    orgnr = c("111", "222"),
    unitName = c("Sykehus A", "Sykehus B"),
    var = 0.5,
    denominator = 100,
    ind_id = "ind_1",
    stringsAsFactors = FALSE
  )
}

mock_app_server_dependencies <- function(role, env) {
  log <- new.env()
  log$added <- character()
  log$removed <- character()

  local_mocked_bindings(
    getSkdeIndicatorData = function(registryShortname = "hjertestans", indicator_id = NULL) {
      list(
        data = make_server_data(),
        indicator_meta = data.frame(
          ind_id = "ind_1", title = "Indikator A", stringsAsFactors = FALSE
        )
      )
    },
    info_server = function(...) NULL,
    samlerapport_server = function(...) NULL,
    mod_over_tid_server = function(...) NULL,
    mod_andeler_server = function(...) NULL,
    .env = env
  )
  local_mocked_bindings(
    navbarWidgetServer2 = function(...) {
      list(role = shiny::reactive(role), org = shiny::reactive("111"))
    },
    autoReportServer = function(...) NULL,
    autoReportOrgServer = function(...) list(value = shiny::reactive("111")),
    exportUCServer = function(...) NULL,
    exportGuideServer = function(...) NULL,
    autoReportFormatInput = function(...) NULL,
    autoReportOrgInput = function(...) NULL,
    autoReportInput = function(...) NULL,
    autoReportUI = function(...) NULL,
    exportUCInput = function(...) NULL,
    exportGuideUI = function(...) NULL,
    .package = "rapbase",
    .env = env
  )
  local_mocked_bindings(
    appendTab = function(inputId, tab, ...) {
      log$added <- c(log$added, inputId)
    },
    removeTab = function(inputId, target, ...) {
      log$removed <- c(log$removed, target)
    },
    .package = "shiny",
    .env = env
  )
  log
}

test_that("app_server adds the SC tabs for an SC user", {
  log <- mock_app_server_dependencies("SC", environment())

  expect_no_error(
    shiny::testServer(app_server, {
      # Setter en input for å kjøre observatørene
      session$setInputs(registry = "hjertestans")
    })
  )

  expect_equal(log$added, c("tabs", "tabs"))
  expect_equal(log$removed, character())
})

test_that("app_server removes the SC tabs for a non-SC user", {
  log <- mock_app_server_dependencies("LU", environment())

  expect_no_error(
    shiny::testServer(app_server, {
      session$setInputs(registry = "hjertestans")
    })
  )

  expect_equal(log$added, character())
  expect_equal(log$removed, c("Utsending", "Eksport"))
})
