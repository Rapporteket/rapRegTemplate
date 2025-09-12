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
        "Veiledning",
        veiledning_ui("veiledning"),
        rapbase::navbarWidgetInput("navbar-widget", selectOrganization = TRUE)
      ),
      shiny::tabPanel(
        "Figur og tabell",
        mod_fordeling_plot_UI("fordeling")
      ),
      shiny::tabPanel(
        "Samlerapport",
        samlerapport_ui("samlerapport")
      ),
      # shiny::tabPanel(
      #   shiny::span("Abonnement",
      #               title = "Bestill tilsending av rapporter p\u00e5 e-post"),
      #   abonnement_ui("abonnement")
      # ),
      shiny::tabPanel(
        "Utsending",
        utsending_ui("utsending")
      )
    ) # navbarPage
  ) # tagList
}
