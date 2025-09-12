#' Server logic for the rapRegTemplate app
#'
#' @param input shiny input object
#' @param output shiny output object
#' @param session shiny session object
#'
#' @return A shiny app server object
#' @export

app_server <- function(input, output, session) {
  user <- rapbase::navbarWidgetServer2(
    "navbar-widget",
    orgName = "pilot"
  )

  test_data <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2025/2025-06-24/cases_year.csv')

  data_licorice_gargle <- readr::read_csv2("../dev/data_licorice_gargle.csv")

  # Brukerinformasjon i menylinja (navbar)
  output$appUserName <- shiny::renderText(
    paste(
      rapbase::getUserFullName(session),
      rapbase::getUserRole(session), sep = ", "
    )
  )
  output$appOrgName <- shiny::renderText(rapbase::getUserReshId(session))
  userInfo <-
    rapbase::howWeDealWithPersonalData(session, callerPkg = "rapRegTemplate")

  shiny::observeEvent(input$userInfo, {
    shinyalert::shinyalert(
      "Dette vet Rapporteket om deg:", userInfo,
      type = "", imageUrl = "rap/logo.svg",
      closeOnEsc = TRUE, closeOnClickOutside = TRUE,
      html = TRUE, confirmButtonText = rapbase::noOptOutOk()
    )
  })

  veiledning_server("veiledning")
  #plots_server("plots")
  samlerapport_server("samlerapport")
  mod_fordeling_plot_server("fordeling", data = data_licorice_gargle)
  #abonnement_server("abonnement", user)
  utsending_server("utsending")
}
