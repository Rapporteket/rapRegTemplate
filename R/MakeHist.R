#' Make a histogram, either plot or its data
#'
#' Short demo on how to produce dynamic content in a shiny app at Rapporteket
#'
#' @param df dataframe from which output is to be made
#' @param var string defining which varable in the data frame to use
#' @param bins numeric vector defining the number of equally large groups
#' @param makeTable Logical that if TRUE function will return a data frame
#' containin the bin borders and count within each bin
#'
#' @return a graphical object or data frame
#' @export
#'
#' @examples
#' makeHist(df = mtcars, var = "mpg", bins = 5, makeTable = FALSE)

makeHist <- function(df, x, var, makeTable = FALSE) {

  ggplot2::ggplot(df, ggplot2::aes(x = as.factor(x), y = var)) +
    ggplot2::geom_bar()

}
