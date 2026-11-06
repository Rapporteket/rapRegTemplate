#' Common report processor for rapRegTemplate
#'
#' Makes reports for rapRegTemplate typically used for auto reports such as
#' subscriptions, dispatchments and bulletins.
#'
#' @param report The type of report to generate. Currently only "local_monthly" is supported.
#' @param outputType The format of the output file. Can be "html", "html_fragment", or "pdf".
#' @param data Data frame with indicator data. If NULL, data are fetched with
#' [fetchSkdeIndicatorData()].
#' @param indicator_meta Data frame with indicator metadata. If NULL, metadata
#' are fetched with [fetchSkdeIndicatorData()].
#' @param orgnr The organization number for which the report is generated.
#'
#' @return A character string with a path to where the produced file is located.
#' @export
reportProcessor <- function(report,
                            outputType = "pdf",
                            data = NULL,
                            indicator_meta = NULL,
                            orgnr = NULL) {
  stopifnot(report %in% c("local_monthly"))
  stopifnot(outputType %in% c("html", "html_fragment", "pdf"))

  if (is.null(data) || is.null(indicator_meta)) {
    api_data <- fetchSkdeIndicatorData()
    if (is.null(data)) {
      data <- api_data$data
    }
    if (is.null(indicator_meta)) {
      indicator_meta <- api_data$indicator_meta
    }
  }

  filename <- NULL

  if (report == "local_monthly") {
    filename <- rapbase::renderRmd(
      system.file("samlerapport.Rmd", package = "rapRegTemplate"),
      outputType = outputType,
      params = list(data = data,
                    indicator_meta = indicator_meta,
                    orgnr = orgnr)
    )
  }
  filename
}
