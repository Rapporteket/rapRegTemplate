test_that("plotSPC builds a p-chart for filtered indicator data", {
  testthat::skip_if_not_installed("qicharts2")

  data <- data.frame(
    year = c(2021L, 2022L, 2023L, 2021L),
    unitName = c("Hospital A", "Hospital A", "Hospital A", "Hospital B"),
    var = c(0.20, 0.25, 0.30, 0.10),
    denominator = c(100, 120, 150, 80),
    ind_id = c("ind-1", "ind-1", "ind-1", "ind-2"),
    title = c("Indicator title", "Indicator title", "Indicator title", "Other title"),
    short_description = c("Indicator description", "Indicator description", "Indicator description", "Other description"),
    stringsAsFactors = FALSE
  )

  plot <- rapRegTemplate:::plotSPC(
    data,
    title = "Indicator title",
    subtitle = "Indicator description"
  )

  expect_s3_class(plot, "ggplot")
})

test_that("plotSPC uses default title when title and subtitle are not provided", {
  testthat::skip_if_not_installed("qicharts2")

  data <- data.frame(
    year = c(2021L, 2022L, 2023L),
    unitName = c("Hospital A", "Hospital A", "Hospital A"),
    var = c(0.20, 0.25, 0.30),
    denominator = c(100, 120, 150),
    ind_id = c("ind-1", "ind-1", "ind-1"),
    stringsAsFactors = FALSE
  )

  plot <- rapRegTemplate:::plotSPC(data)

  expect_s3_class(plot, "ggplot")
  expect_equal(plot$labels$title, "SPC-diagram")
})

test_that("plotSPC fails when required columns are missing", {
  data <- data.frame(
    year = c(2021L, 2022L),
    unitName = c("Hospital A", "Hospital B"),
    var = c(0.20, 0.25),
    stringsAsFactors = FALSE
  )

  expect_error(
    rapRegTemplate:::plotSPC(data),
    "requires columns: year, unitName, var, denominator, ind_id\\. Missing: denominator, ind_id"
  )
})

test_that("plotSPC fails when filters remove all observations", {
  data <- data.frame(
    year = 2021L,
    unitName = "Hospital A",
    var = 0.20,
    denominator = 0,
    ind_id = "ind-1",
    stringsAsFactors = FALSE
  )

  expect_error(
    rapRegTemplate:::plotSPC(data),
    "no observations after filtering"
  )
})