test_that("plotSPC builds a p-chart for filtered indicator data", {
  testthat::skip_if_not_installed("qicharts2")

  data <- data.frame(
    year = c(2021L, 2022L, 2023L, 2021L),
    orgnr = c("Hospital A", "Hospital A", "Hospital A", "Hospital B"),
    var = c(0.20, 0.25, 0.30, 0.10),
    denominator = c(100, 120, 150, 80),
    ind_id = c("ind-1", "ind-1", "ind-1", "ind-2"),
    title = c("Indicator title", "Indicator title", "Indicator title", "Other title"),
    short_description = c("Indicator description", "Indicator description", "Indicator description", "Other description"),
    stringsAsFactors = FALSE
  )

  plot <- rapRegTemplate:::plotSPC(data, orgnr = "Hospital A", ind_id = "ind-1")

  expect_s3_class(plot, "ggplot")
})

test_that("plotSPC fails when filters remove all observations", {
  data <- data.frame(
    year = 2021L,
    orgnr = "Hospital A",
    var = 0.20,
    denominator = 100,
    ind_id = "ind-1",
    stringsAsFactors = FALSE
  )

  expect_error(
    rapRegTemplate:::plotSPC(data, orgnr = "Hospital B", ind_id = "ind-1"),
    "no observations after filtering"
  )
})