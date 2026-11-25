test_that("fetchSkdeIndicatorData returns a data frame with the expected schema", {
  skip_if_offline()

  result <- fetchSkdeIndicatorData("hjertestans")

  expect_type(result, "list")
  expect_true(all(c("data", "indicator_meta") %in% names(result)))

  expect_s3_class(result$data, "data.frame")
  expect_true(all(c(
    "year", "orgnr", "unitName", "var", "denominator", "ind_id", "context",
    "title", "short_description", "levelDirection", "kvalIndgrenser"
  ) %in% names(result$data)))

  expect_s3_class(result$indicator_meta, "data.frame")
  expect_true(all(c(
    "ind_id", "title", "short_description", "kvalIndgrenser", "levelDirection"
  ) %in% names(result$indicator_meta)))
})

test_that("fetchSkdeIndicatorData returns empty list payload for a non-existent indicator", {
  skip_if_offline()

  result <- fetchSkdeIndicatorData("hjertestans", indicator_id = "definitely-non-existent-indicator-id")

  expect_type(result, "list")
  expect_true(all(c("data", "indicator_meta") %in% names(result)))

  expect_s3_class(result$data, "data.frame")
  expect_equal(nrow(result$data), 0)
  expect_true(all(c(
    "year", "orgnr", "unitName", "var", "denominator", "ind_id", "context"
  ) %in% names(result$data)))

  expect_s3_class(result$indicator_meta, "data.frame")
  expect_equal(nrow(result$indicator_meta), 0)
  expect_true(all(c(
    "ind_id", "title", "short_description"
  ) %in% names(result$indicator_meta)))
})

nested_json <- jsonlite::toJSON(list(list(indicatorData = list(list(
  indicatorID = "ind_1",
  indicatorTitle = "Indikator A",
  shortDescription = "Kort beskrivelse",
  levelDirection = 1,
  levelYellow = 0.5,
  levelGreen = 0.8,
  data = list(
    list(year = 2023, unitName = "Sykehus A", var = 0.5, denominator = 100, context = "ctx"),
    list(year = 2023, unitName = "Sykehus B", var = 0.6, denominator = 50, context = "ctx"),
    list(year = 2023, unitName = "Sykehus A", denominator = 10, context = "ctx")
  )
)))), auto_unbox = TRUE)

test_that("fetchSkdeIndicatorData flattens nested data and skips incomplete rows", {
  captured_url <- NULL
  local_mocked_bindings(
    GET = function(url, ...) {
      captured_url <<- url
      structure(list(), class = "response")
    },
    content = function(x, ...) as.character(nested_json),
    .package = "httr"
  )

  result <- fetchSkdeIndicatorData("hjertestans")

  expect_equal(captured_url, "https://prod-api.skde.org/data/hjertestans/nestedData")
  expect_equal(nrow(result$data), 2)
  expect_equal(result$data$orgnr, c("100", "101"))
  expect_equal(nrow(result$indicator_meta), 1)
  expect_equal(result$indicator_meta$title, "Indikator A")
})

test_that("fetchSkdeIndicatorData filters on indicator_id", {
  local_mocked_bindings(
    GET = function(url, ...) structure(list(), class = "response"),
    content = function(x, ...) as.character(nested_json),
    .package = "httr"
  )

  result <- fetchSkdeIndicatorData("hjertestans", indicator_id = "ind_finnes_ikke")

  expect_equal(nrow(result$data), 0)
  expect_equal(nrow(result$indicator_meta), 0)
})

test_that("fetchSkdeIndicatorData returns empty data for an empty payload", {
  local_mocked_bindings(
    GET = function(url, ...) structure(list(), class = "response"),
    content = function(x, ...) "[]",
    .package = "httr"
  )

  result <- fetchSkdeIndicatorData("hjertestans")

  expect_equal(nrow(result$data), 0)
  expect_true("kvalIndgrenser" %in% names(result$data))
})

test_that("getSkdeIndicatorData caches the result per registry", {
  calls <- 0
  local_mocked_bindings(
    fetchSkdeIndicatorData = function(registryShortname, indicator_id = NULL) {
      calls <<- calls + 1
      list(data = data.frame(), indicator_meta = data.frame())
    }
  )
  registry <- paste0("cachetest", as.integer(Sys.time()))

  getSkdeIndicatorData(registry)
  getSkdeIndicatorData(registry)

  expect_equal(calls, 1)
})

test_that("fetchSkdeRegistryNames returns sorted unique registry names", {
  captured_url <- NULL
  local_mocked_bindings(
    GET = function(url, ...) {
      captured_url <<- url
      structure(list(), class = "response")
    },
    content = function(x, ...) {
      '[{"registry_name":"hjerneslag"},{"registry_name":"hjertestans"},{"registry_name":"hjertestans"}]'
    },
    stop_for_status = function(x, ...) invisible(x),
    .package = "httr"
  )

  expect_equal(fetchSkdeRegistryNames(), c("hjerneslag", "hjertestans"))
  expect_equal(captured_url, "https://prod-api.skde.org/info/names")
})

test_that("fetchSkdeRegistryNames stops on HTTP errors", {
  local_mocked_bindings(
    GET = function(url, ...) structure(list(), class = "response"),
    stop_for_status = function(x, ...) stop("HTTP-feil"),
    .package = "httr"
  )

  expect_error(fetchSkdeRegistryNames(), "HTTP-feil")
})
