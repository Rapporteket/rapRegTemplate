#' Shiny module providing GUI and server logic for the intro tab
#'
#' @param id Character string module namespace
#' @return An shiny app ui object
#' @export

info_ui <- function(id) {
  ns <- shiny::NS(id)

  shiny::mainPanel(
    width = 12,
    shiny::h2("Informasjon om Rapporteket"),
    shiny::uiOutput(ns("info"), inline = TRUE)
  )
}

#' Server logic
#'
#' @param id Character string module namespace
#' @param user Shiny user object, som defineres av shiny-modulen
#' rapbase::navbarWidgetServer2. Denne inneholder informasjon om pålogget
#' bruker, og kan brukes til å tilpasse innholdet i appen basert på brukerens
#' rolle eller organisasjon.
#'
#' @return A shiny app server object
#' @export

info_server <- function(id, user) {
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
