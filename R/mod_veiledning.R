#' Shiny module providing GUI and server logic for the intro tab
#'
#' @param id Character string module namespace
#' @param selected Character string, registry selected in the select input
#' @return An shiny app ui object
#' @export

info_ui <- function(id, selected = "hjertestans") {
  ns <- shiny::NS(id)

  shiny::mainPanel(
    width = 12,
    shiny::h2("Informasjon om Rapporteket"),
    shiny::fluidRow(
      shiny::column(
        width = 4,
        shiny::div(
          style = "display:flex; align-items:flex-end; gap:8px;",
          shiny::div(
            style = "flex:1; min-width:0;",
            shiny::selectInput(
              inputId = ns("registry"),
              label = "Registernavn i SKDE-API:",
              choices = selected,
              selected = selected
            )
          ),
          shiny::actionButton(
            inputId = ns("load"),
            label = "Hent data",
            style = "margin-bottom:15px;"
          )
        )
      )
    ),
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

      # Fyller valglisten med registernavn fra SKDE-API-et, og velger registeret fra URL-en
      shiny::observe({
        regNames <- tryCatch(fetchSkdeRegistryNames(), error = function(e) character(0))
        if (length(regNames) == 0L) return()
        current <- shiny::isolate(
          shiny::parseQueryString(session$clientData$url_search)$registry
        )
        if (is.null(current) || !current %in% regNames) {
          current <- if ("hjertestans" %in% regNames) "hjertestans" else regNames[1]
        }
        shiny::updateSelectInput(session, "registry", choices = regNames, selected = current)
      })

      # Oppdaterer URL-en med valgt register, og laster appen på nytt
      # Ikke nødvendig i ekte Rapportek, kun funksjon for at registre
      # kan få demonstrert hvordan Rapporteket fungerer
      shiny::observeEvent(input$load, {
        shiny::req(input$registry)
        shiny::updateQueryString(
          paste0("?registry=", input$registry),
          mode = "push",
          session = session
        )
        session$reload()
      }, ignoreInit = TRUE)
    }
  )
}
