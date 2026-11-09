make_report_data <- function() {
  grid <- expand.grid(
    year = 2022:2023,
    orgnr = c("111", "222"),
    ind_id = c("ind_1", "ind_2"),
    stringsAsFactors = FALSE
  )
  grid$unitName <- ifelse(grid$orgnr == "111", "Sykehus A", "Sykehus B")
  grid$var <- seq(0.4, 0.8, length.out = nrow(grid))
  grid$denominator <- 100
  grid$context <- "caregiver"
  grid$title <- ifelse(grid$ind_id == "ind_1", "Indikator A", "Indikator B")
  grid$short_description <- "Kort tekst"
  grid$levelDirection <- 1
  grid$kvalIndgrenser <- I(rep(list(c(0.5, 0.7)), nrow(grid)))
  grid
}

make_report_meta <- function() {
  data.frame(
    ind_id = c("ind_1", "ind_2"),
    title = c("Indikator A", "Indikator B"),
    short_description = "Kort tekst",
    kvalIndgrenser = I(rep(list(c(0.5, 0.7)), 2)),
    levelDirection = 1,
    stringsAsFactors = FALSE
  )
}

test_that("invalid report throws error", {
  expect_error(
    reportProcessor("invalid_report")
  )
})

test_that("invalid outputType throws error", {
  expect_error(
    reportProcessor(
      report = "local_monthly",
      outputType = "docx"
    )
  )
})

test_that("data are fetched from the API when not provided", {
  fetched <- FALSE
  rm(list = ls(rapRegTemplate:::.skde_cache), envir = rapRegTemplate:::.skde_cache)
  withr::defer(rm(list = ls(rapRegTemplate:::.skde_cache), envir = rapRegTemplate:::.skde_cache))
  local_mocked_bindings(
    fetchSkdeIndicatorData = function(...) {
      fetched <<- TRUE
      list(data = make_report_data(), indicator_meta = make_report_meta())
    }
  )

  result <- reportProcessor(
    report = "local_monthly",
    outputType = "html",
    orgnr = "111"
  )

  expect_true(fetched)
  expect_match(result, "\\.html$")
})

test_that("html report is rendered with provided data", {
  result <- reportProcessor(
    report = "local_monthly",
    outputType = "html",
    data = make_report_data(),
    indicator_meta = make_report_meta(),
    orgnr = "111"
  )

  expect_match(result, "\\.html$")
  expect_true(file.exists(result))
})

test_that("html_fragment report contains indicator titles", {
  result <- reportProcessor(
    report = "local_monthly",
    outputType = "html_fragment",
    data = make_report_data(),
    indicator_meta = make_report_meta(),
    orgnr = "111"
  )

  expect_match(result, "Indikator A")
  expect_match(result, "Indikator B")
})

test_that("report fails without a valid orgnr", {
  expect_error(
    reportProcessor(
      report = "local_monthly",
      outputType = "html",
      data = make_report_data(),
      indicator_meta = make_report_meta(),
      orgnr = NULL
    )
  )
})
