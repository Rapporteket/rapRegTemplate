#' Shiny module providing GUI and server logic for the subscription v2 tab
#'
#' @param id Character string module namespace
#' @return An shiny app ui object

abonnement_ui <- function(id) {

  shiny::sidebarLayout(
    shiny::sidebarPanel(
      rapbase::autoReportInput(id)
    ),
    shiny::mainPanel(
      rapbase::autoReportUI(id)
    )
  )
}

abonnement_server <- function(id, user) {

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
    id = id, registryName = "rapRegTemplate",
    type = "subscription", reports = reports, orgs = orgs, freq = "quarter", user = user
  )
}

#' Example report function 1
#' @return Path to the generated report file
#' @export

samlerapport1Fun <- function(file = NULL, p1 = "Alder", p2 = 1, reshID = 999999) {

  if (is.null(file)) {
    file <- base::tempfile(fileext = ".pdf")
  }

  grDevices::pdf(file)
  graphics::plot.new()
  graphics::text(0.5, 0.9, "Samlerapport 1", cex = 2, font = 2)
  grDevices::dev.off()

  return(file)
}

#' Example report function 2
#' @return Path to the generated report file
#' @export

samlerapport2Fun <- function(file = NULL, p1 = "Alder", p2 = 1, reshID = 999999) {

  if (is.null(file)) {
    file <- base::tempfile(fileext = ".pdf")
  }

  grDevices::pdf(file)
  graphics::plot.new()
  graphics::text(0.5, 0.9, "Samlerapport 2", cex = 2, font = 2)
  grDevices::dev.off()

  return(file)
}
