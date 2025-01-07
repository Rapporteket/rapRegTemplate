#' Shiny module providing GUI and server logic for the subscription v2 tab
#'
#' @param id Character string module namespace
NULL

abonnementV2_ui <- function(id) {
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

abonnementV2_server <- function(id) {
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
      id = "testSubscription", registryName = "kvarus",
      type = "subscription", reports = reports, orgs = orgs, freq = "quarter"
    )
    }
  )
}