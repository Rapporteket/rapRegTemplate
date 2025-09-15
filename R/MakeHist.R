#' Make a histogram, either plot or its data
#'
#' Short demo on how to produce dynamic content in a shiny app at Rapporteket
#'
#' @param df dataframe from which output is to be made
#' @param var string defining which variable in the data frame to use
#' @param makeTable Logical that if TRUE function will return a data frame
#' containin the bin borders and count within each bin
#' @param x What to plot
#'
#' @return a graphical object or data frame
#' @export
#'

makeHist <- function(df, x, var, makeTable = FALSE) {

  ggplot2::ggplot(df, ggplot2::aes(x = as.factor(x), y = var)) +
    ggplot2::geom_bar()

}
