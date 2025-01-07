#' Shiny module providing GUI and server logic for the subscription v2 tab
#'
#' @param id Character string module namespace
NULL

abonnement_v2_ui <- function(id) {
  ns <- shiny::NS(id)

  shiny::sidebarLayout(
    shiny::sidebarPanel(
      rapbase::autoReportInput(ns("testSubscription"))
    ),
    shiny::mainPanel(
      rapbase::autoReportUI(ns("testSubscription"))
    )
  )
}

abonnement_v2_server <- function(id) {
  shiny::moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns

      ## nye abonnement
      ## Objects currently shared among subscription and dispathcment
      orgs <- list(Sykehus1 = 1234,
                   Sykehus2 = 4321)
      reports <- list(
        Samlerapport1 = list(
          synopsis = "Automatisk samlerapport1",
          fun = "samlerapport1Fun",
          paramNames = c("p1", "p2"),
          paramValues = c("Alder", 1)
        ),
        Samlerapport2 = list(
          synopsis = "Automatisk samlerapport2",
          fun = "samlerapport2Fun",
          paramNames = c("p1", "p2"),
          paramValues = c("BMI", 1)
        )
      )

      ## Subscription
      rapbase::autoReportServer(
        id = ns("testSubscription"), registryName = "rapRegTemplate",
        type = "subscription", reports = reports, orgs = orgs, freq = "quarter"
      )
    }
  )
}