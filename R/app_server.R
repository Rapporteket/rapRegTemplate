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
  plots_server("plots")
  samlerapport_server("samlerapport")

  subParamNames <- shiny::reactive(c("reshID"))
  subParamValues <- shiny::reactive(user$org())

  rapbase::autoReportServer(
    id = "subscription",
    registryName = "rapRegTemplate",
    type = "subscription",
    paramNames = subParamNames,
    paramValues = subParamValues,
    reports = list(
      `Automatisk samlerapport1` = list(
        synopsis = "
        Automatisk samlerapport1 som inneholder spennende statistikk
        om pasienter og operasjoner",
        fun = "samlerapport1Fun",
        paramNames = c("p1", "p2", "reshID"),
        paramValues = c("Alder", 1, 999999)
      ),
      `Automatisk samlerapport2` = list(
        synopsis = "Automatisk samlerapport2 som inneholder enda mer spennende statistikk
        om pasienter og operasjoner, i tillegg til BMI",
        fun = "samlerapport2Fun",
        paramNames = c("p1", "p2", "reshID"),
        paramValues = c("BMI", 1, 999999)
      )
    ),
    freq = "quarter",
    user = user
  )
}
