test_that("fetchSkdeIndicatorData returns a data frame with the expected schema", {
  skip_if_offline()

  result <- fetchSkdeIndicatorData("hjertestans")

  expect_type(result, "list")
  expect_true(all(c("data", "indicator_meta") %in% names(result)))

  expect_s3_class(result$data, "data.frame")
  expect_true(all(c(
    "year", "orgnr", "var", "denominator", "ind_id", "context",
    "title", "short_description", "levelDirection", "kvalIndgrenser"
  ) %in% names(result$data)))

  expect_s3_class(result$indicator_meta, "data.frame")
  expect_true(all(c(
    "ind_id", "title", "short_description", "kvalIndgrenser", "levelDirection"
  ) %in% names(result$indicator_meta)))
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
