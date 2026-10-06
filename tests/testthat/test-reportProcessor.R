
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

test_that("renderRmd is called with correct arguments for html", {

  result <- reportProcessor(
    report = "local_monthly",
    outputType = "html",
    var = "hp",
    bins = 20
  )

  expect_match(result, ".html")

})


test_that("renderRmd is called with correct arguments for html_fragment", {

  result <- reportProcessor(
    report = "local_monthly",
    outputType = "html_fragment",
    var = "wt",
    bins = 20
  )
  expect_match(result, "Mazda RX4 Wag")

})
