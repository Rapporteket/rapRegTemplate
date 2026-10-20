#' Common report processor for rapRegTemplate
#'
#' Makes reports for rapRegTemplate typically used for auto reports such as
#' subscriptions, dispatchments and bulletins.
#'
#' @param report The type of report to generate. Currently only "local_monthly" is supported.
#' @param outputType The format of the output file. Can be "html", "html_fragment", or "pdf".
#' @param orgnr The organization number for which the report is generated.
#'
#' @return A character string with a path to where the produced file is located.
#' @export
reportProcessor <- function(report,
                            outputType = "pdf",
                            data,
                            indicator_meta,
                            orgnr = NULL) {
  stopifnot(report %in% c("local_monthly"))
  stopifnot(outputType %in% c("html", "html_fragment", "pdf"))

  if (outputType == "html_fragment") {
    type <- "html"
  } else {
    type <- outputType
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
