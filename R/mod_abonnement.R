#' Shiny module providing GUI and server logic for the subscription tab
#'
#' @param id Character string module namespace
NULL

abonnement_ui <- function(id) {
  ns <- shiny::NS(id)

  shiny::sidebarLayout(
    shiny::sidebarPanel(
      width = 3,
      shiny::selectInput(
        ns("subscriptionRep"), "Rapport:",
        c("Samlerapport1", "Samlerapport2")
      ),
      shiny::selectInput(
        ns("subscriptionFreq"), "Frekvens:",
        list("\u212brlig" = "\u212brlig-year",
            Kvartalsvis = "Kvartalsvis-quarter",
            "M\u00e5nedlig" = "M\u00e5nedlig-month",
            Ukentlig = "Ukentlig-week",
            Daglig = "Daglig-DSTday"),
        selected = "M\u00e5nedlig-month"
      ),
      shiny::actionButton(
        ns("subscribe"), "Bestill",
        icon = shiny::icon("paper-plane")
      )
    ),
    shiny::mainPanel(
      shiny::uiOutput(ns("subscriptionContent"))
    )
  )
}

abonnement_server <- function(id) {
  shiny::moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns

      
  # Abonnement
  ## rekative verdier for aa holde rede paa endringer som skjer mens
  ## applikasjonen kjorer
  subscription <- shiny::reactiveValues(
    tab = rapbase::makeAutoReportTab(session, type = "subscription")
  )

  ## lag tabell over gjeldende status for abonnement
  output$activeSubscriptions <- DT::renderDataTable(
    subscription$tab, server = FALSE, escape = FALSE, selection = "none",
    options = list(dom = "tp", ordning = FALSE,
                   columnDefs = list(list(visible = FALSE, targets = 6))),
    rownames = FALSE
  )

  ## lag side som viser status for abonnement, ogsaa naar det ikke finnes noen
  output$subscriptionContent <- shiny::renderUI({
    userFullName <- rapbase::getUserFullName(session)
    if (length(subscription$tab) == 0) {
      shiny::p(paste("Ingen aktive abonnement for", userFullName))
    } else {
      shiny::tagList(
        shiny::p(paste0("Aktive abonnement som sendes per epost til ",
                        userFullName, ":")),
        DT::dataTableOutput("activeSubscriptions")
      )
    }
  })
  
    }
  )
}