#' Shiny module providing GUI and server logic for the intro tab
#'
#' @param id Character string module namespace
#' @return An shiny app ui object
#' @export

info_ui <- function(id) {
  ns <- shiny::NS(id)

  shiny::mainPanel(
    width = 12,
    shiny::htmlOutput(ns("info"), inline = TRUE)
  )
}

#' Server logic
#' @return A shiny app server object
#' @export

info_server <- function(id) {
  shiny::moduleServer(
    id,
    function(input, output, session) {

      # Info
      output$info <- shiny::renderUI({
        rapbase::renderRmd(
          system.file("info.Rmd", package = "rapRegTemplate"),
          outputType = "html_fragment"
        )
      })
    }
  )
}
