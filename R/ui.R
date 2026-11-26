#' Client (ui) for the rapRegTemplate app
#'
#' @param request Shiny request object, used to read the registry from the URL
#' @return An shiny app ui object
#' @export

app_ui <- function(request = NULL) {

  regTitle <- "rapRegTemplate"

  # Henter registernavn fra SKDE-API-et
  registry <- shiny::parseQueryString(request$QUERY_STRING)$registry

  shiny::tagList(
    shiny::navbarPage(
      title = rapbase::regTitle(regTitle),
      windowTitle = regTitle,
      theme = rapbase::rapTheme(version = 5),
      id = "tabs",
      shiny::tabPanel(
        "Informasjon",
        info_ui("info", selected = registry),
        rapbase::navbarWidgetInput("navbar-widget", selectOrganization = TRUE)
      ),
      shiny::tabPanel(
        "Over tid",
        mod_over_tid_ui("over_tid")
      ),
      shiny::tabPanel(
        "Andeler",
        mod_andeler_ui("andeler")
      ),
      shiny::tabPanel(
        "Samlerapport",
        samlerapport_ui("samlerapport")
      ),
      shiny::tabPanel(
        shiny::span(
          "Abonnement",
          title = "Bestill tilsending av rapporter p\u00e5 e-post"
        ),
        shiny::sidebarLayout(
          shiny::sidebarPanel(
            rapbase::autoReportInput("subscription")
          ),
          shiny::mainPanel(
            rapbase::autoReportUI("subscription")
          )
        )
      )
    ) # navbarPage
  ) # tagList
}
