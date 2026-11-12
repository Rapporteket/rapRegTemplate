#' Server logic for the rapRegTemplate app
#'
#' @param input shiny input object
#' @param output shiny output object
#' @param session shiny session object
#'
#' @return A shiny app server object
#' @export

app_server <- function(input, output, session) {


  APIdata <- getSkdeIndicatorData()

  orgData <- dplyr::distinct(APIdata$data, .data$unitName, .data$orgnr)

  # data.frame som mapper ReshID og sykehusnavn
  map_orgname <- data.frame(
    UnitId = orgData$orgnr,
    orgname = orgData$unitName
  )
  user <- rapbase::navbarWidgetServer2(
    "navbar-widget",
    orgName = "pilot",
    map_orgname = map_orgname,
    caller = "rapRegTemplate"
  )

  data_licorice_gargle <- licorice_gargle


  info_server("info", user = user)
  samlerapport_server("samlerapport", data = APIdata$data, indicator_meta = APIdata$indicator_meta, user = user)
  mod_fordeling_plot_server("fordeling", data = data_licorice_gargle)
  mod_over_tid_server("over_tid", data = APIdata$data, indicator_meta = APIdata$indicator_meta, user = user)
  mod_andeler_server("andeler", data = APIdata$data, indicator_meta = APIdata$indicator_meta)

  #################
  # Subscriptions #
  #################

  autoReports <- list(
    `Automatisk samlerapport` = list(
      synopsis = "Automatisk samlerapport som inneholder spennende statistikk
        om indikatorer for valgt sykehus",
      fun = "reportProcessor",
      paramNames = c("report", "orgnr", "outputType"),
      paramValues = c("local_monthly", "", "html_fragment")
    )
  )

  subParamNames <- shiny::reactive(c("orgnr"))
  subParamValues <- shiny::reactive(user$org())

  rapbase::autoReportServer(
    id = "subscription",
    registryName = "rapRegTemplate",
    type = "subscription",
    paramNames = subParamNames,
    paramValues = subParamValues,
    reports = autoReports,
    freq = "quarter",
    user = user
  )

  ################
  # SC user tabs #
  ################

  shiny::observeEvent(
    shiny::req(user$role()), {
      if (user$role() != "SC") {
        message("Removing dispatchment tab for user with role ", user$role())
        shiny::removeTab("tabs", target = "Utsending")
        message("Removing export tab for user with role ", user$role())
        shiny::removeTab("tabs", target = "Eksport")
      } else {
        message("Adding dispatchment tab for user with role ", user$role())
        shiny::appendTab(
          "tabs",
          shiny::tabPanel(
            "Utsending",
            shiny::sidebarLayout(
              shiny::sidebarPanel(
                rapbase::autoReportFormatInput("dispatchment"),
                rapbase::autoReportOrgInput("dispatchment"),
                rapbase::autoReportInput("dispatchment")
              ),
              shiny::mainPanel(
                rapbase::autoReportUI("dispatchment")
              )
            )
          )
        )
        message("Adding export tab for user with role ", user$role())
        shiny::appendTab(
          "tabs",
          shiny::tabPanel(
            "Eksport",
            shiny::sidebarLayout(
              shiny::sidebarPanel(
                rapbase::exportUCInput("export")
              ),
              shiny::mainPanel(
                rapbase::exportGuideUI("exportGuide")
              )
            )
          )
        )
      }
    }
  )


  #################
  # Dispatchments #
  #################

  orgs <- stats::setNames(orgData$orgnr, orgData$unitName)

  org <- rapbase::autoReportOrgServer("dispatchment", orgs)

  disParamNames <- shiny::reactive(c("orgnr"))
  disParamValues <- shiny::reactive(c(org$value()))

  rapbase::autoReportServer(
    id = "dispatchment",
    registryName = "rapRegTemplate",
    type = "dispatchment",
    org = org$value,
    paramNames = disParamNames,
    paramValues = disParamValues,
    reports = autoReports,
    orgs = orgs,
    user = user,
    runAutoReportButton = TRUE
  )

  ###############
  # Export data #
  ###############

  ## brukerkontroller
  rapbase::exportUCServer(
    "export",
    dbName = "data",
    teamName = Sys.getenv("SHINYPROXY_APPID", unset = "unknown")
  )
  ## veiledning
  rapbase::exportGuideServer("exportGuide", dbName = "data")
}
