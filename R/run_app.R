#' Run the Shiny Application
#'
#' @param browser Open app in browser window
#' @param logAsJson Log in json-format
#'
#' @return An object representing the app
#' @export

run_app <- function() {
  shiny::shinyApp(ui = app_ui, server = app_server)
}
