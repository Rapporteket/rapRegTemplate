test_that("fetchSkdeIndicatorData returns a data frame with the expected schema", {
  skip_if_offline()

  result <- fetchSkdeIndicatorData("hjertestans")

  expect_s3_class(result, "data.frame")
  expect_true(all(c(
    "year", "orgnr", "var", "denominator", "ind_id", "context",
    "title", "short_description", "levelDirection", "kvalIndgrenser"
  ) %in% names(result)))
})

test_that("fetchSkdeIndicatorData returns an empty data frame for a non-existent indicator", {
  skip_if_offline()

  result <- fetchSkdeIndicatorData("hjertestans", indicator_id = "definitely-non-existent-indicator-id")

  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 0)
  expect_true(all(c(
    "year", "orgnr", "var", "denominator", "ind_id", "context"
  ) %in% names(result)))
})
