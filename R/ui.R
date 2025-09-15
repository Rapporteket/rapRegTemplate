#' Client (ui) for the rapRegTemplate app
#'
#' @return An shiny app ui object
#' @export

app_ui <- function() {

  shiny::addResourcePath("rap", system.file("www", package = "rapbase"))
  regTitle <- "rapRegTemplate"

  shiny::tagList(
    shiny::navbarPage(
      title = shiny::div(
        shiny::a(shiny::includeHTML(
          system.file("www/logo.svg", package = "rapbase")
        )
        ),
        regTitle
      ),
      windowTitle = regTitle,
      theme = "rap/bootstrap.css",
      id = "tabs",
      shiny::tabPanel(
        "Informasjon",
        info_ui("info"),
        rapbase::navbarWidgetInput("navbar-widget", selectOrganization = TRUE)
      ),
      shiny::tabPanel(
        "Fordeling",
        mod_fordeling_plot_ui("fordeling")
      ),
      shiny::tabPanel(
        "Over tid",
        mod_over_tid_ui("over_tid")
      ),
      shiny::tabPanel(
        "Samlerapport",
        samlerapport_ui("samlerapport")
      ),
      shiny::tabPanel(
        "Pivot-tabell",
        pivot_ui("pivot")
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
