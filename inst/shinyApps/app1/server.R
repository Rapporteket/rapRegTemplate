library(shiny)
library(magrittr)
library(rapRegTemplate)

server <- function(input, output, session) {

  # Last inn data
  # regData <- getFakeRegData()

  # Gjenbrukbar funksjon for å bearbeide Rmd til html
  htmlRenderRmd <- function(srcFile, params = list()) {
    # set param needed for report meta processing
    # params <- list(tableFormat="html")
    system.file(srcFile, package="rapRegTemplate") %>%
      knitr::knit() %>%
      markdown::markdownToHTML(.,
                               options = c('fragment_only',
                                           'base64_images',
                                           'highlight_code')) %>%
      shiny::HTML()
  }

  # Brukerinformasjon i menylinja (navbar)
  output$appUserName <- renderText(paste(getUserFullName(session),
                                         getUserRole(session), sep = ", "))
  output$appOrgName <- renderText(getUserReshId(session))
  userInfo <- rapbase::howWeDealWithPersonalData(session,
                                                 callerPkg = "rapRegTemplate")
  observeEvent(input$userInfo, {
    shinyalert("Dette vet Rapporteket om deg:", userInfo,
               type = "", imageUrl = "rap/logo.svg",
               closeOnEsc = TRUE, closeOnClickOutside = TRUE,
               html = TRUE, confirmButtonText = rapbase::noOptOutOk())
  })

  # Veiledning
  output$veiledning <- renderUI({
    htmlRenderRmd("veiledning.Rmd")
  })


  # Figur og tabell
  ## Figur
  #output$distPlot <- renderPlot({
  #  makeHist(df = regData, var = input$var, bins = input$bins)
  #})

  ## Tabell
  #output$distTable <- renderTable({
  #  makeHist(df = regData, var = input$var, bins = input$bins, makeTable = TRUE)
  #})


  # Samlerapport
  ## vis
  output$samlerapport <- renderUI({
    htmlRenderRmd(srcFile = "samlerapport.Rmd",
                  params = list(var = input$varS, bins = input$binsS))
  })

  ## last ned
  output$downloadSamlerapport <- downloadHandler(
    filename = function() {
      "rapRegTemplateSamlerapport.html"
    },
    content = function(file) {
      srcFile <- normalizePath(system.file("samlerapport.Rmd",
                                           package = "rapRegTemplate"))
      tmpFile <- "tmpSamlerapport.Rmd"
      owd <- setwd(tempdir())
      on.exit(setwd(owd))
      file.copy(srcFile, tmpFile, overwrite = TRUE)
      out <- rmarkdown::render(tmpFile,
                               output_format =  rmarkdown::html_document(),
                               params = list(var = input$varS,
                                             bins = input$binsS),
                               output_dir = tempdir())
      file.rename(out, file)
    }
  )


  # Abonnement
  ## rekative verdier for å holde rede på endringer som skjer mens
  ## applikasjonen kjører
  rv <- reactiveValues(
    subscriptionTab = rapbase::makeUserSubscriptionTab(session))

  ## lag tabell over gjeldende status for abonnement
  output$activeSubscriptions <- DT::renderDataTable(
    rv$subscriptionTab, server = FALSE, escape = FALSE, selection = 'none',
    options = list(dom = 'tp', ordning = FALSE), rownames = FALSE
  )

  ## lag side som viser status for abonnement, også når det ikke finnes noen
  output$subscriptionContent <- renderUI({
    userFullName <- rapbase::getUserFullName(session)
    userEmail <- rapbase::getUserEmail(session)
    if (length(rv$subscriptionTab) == 0) {
      p(paste("Ingen aktive abonnement for", userFullName))
    } else {
      tagList(
        p(paste0("Aktive abonnement som sendes per epost til ", userFullName,
                 "(",userEmail, "):")),
        DT::dataTableOutput("activeSubscriptions")
      )
    }
  })

  ## nye abonnement
  observeEvent (input$subscribe, {
    package <- "rapRegTemplate"
    type <- "subscription"
    owner <- getUserName(session)
    interval <- strsplit(input$subscriptionFreq, "-")[[1]][2]
    intervalName <- strsplit(input$subscriptionFreq, "-")[[1]][1]
    runDayOfYear <- rapbase::makeRunDayOfYearSequence(
      interval = interval)

    rapbase::getUserEmail(session)
    organization <- rapbase::getUserReshId(session)

    if (input$subscriptionRep == "Samlerapport1") {
      synopsis <- "Automatisk samlerapport1"
      fun <- "samlerapport1Fun"
      paramNames <- c("p1", "p2")
      paramValues <- c("Alder", 1)

    }
    if (input$subscriptionRep == "Samlerapport2") {
      synopsis <- "Automatisk samlerapport2"
      fun <- "samlerapport2Fun"
      paramNames <- c("p1", "p2")
      paramValues <- c("BMI", 2)
    }
    rapbase::createAutoReport(synopsis = synopsis, package = package,
                              type = type, fun = fun, paramNames = paramNames,
                              paramValues = paramValues, owner = owner,
                              email = email, organization = organization,
                              runDayOfYear = runDayOfYear,
                              interval = interval, intervalName = intervalName)
    rv$subscriptionTab <- rapbase::makeUserSubscriptionTab(session)
  })


  # Utsending
  ## reaktive verdier for å holde rede på endringer som skjer mens
  ## applikasjonen kjører
  dispatchment <- reactiveValues(
    tab = rapbase::makeRegDispatchmentTab(session = session)
  )
  sendTo <- reactiveValues(email = vector())

  ## observér og foreta endringer mens applikasjonen kjører
  observeEvent(input$doAddEmail, {
    sendTo$email <- c(sendTo$email, input$email)
  })
  observeEvent(input$doDelEmail, {
    sendTo$email <- sendTo$email[!sendTo$email == input$email]
  })
  observeEvent (input$dispatch, {
    package <- "rapRegTemplate"
    type <- "dispatchment"
    owner <- getUserName(session)
    interval <- strsplit(input$dispatchmentFreq, "-")[[1]][2]
    intervalName <- strsplit(input$dispatchmentFreq, "-")[[1]][1]
    runDayOfYear <- rapbase::makeRunDayOfYearSequence(
      interval = interval)

    email <- sendTo$email
    organization <- rapbase::getUserReshId(session)

    if (input$dispatchmentRep == "Samlerapport1") {
      synopsis <- "Automatisk samlerapport1"
      fun <- "samlerapport1Fun"
      paramNames <- c("p1", "p2")
      paramValues <- c("Alder", 1)

    }
    if (input$dispatchmentRep == "Samlerapport2") {
      synopsis <- "Automatisk samlerapport2"
      fun <- "samlerapport2Fun"
      paramNames <- c("p1", "p2")
      paramValues <- c("BMI", 2)
    }
    rapbase::createAutoReport(synopsis = synopsis, package = package,
                              type = type, fun = fun, paramNames = paramNames,
                              paramValues = paramValues, owner = owner,
                              email = email, organization = organization,
                              runDayOfYear = runDayOfYear,
                              interval = interval, intervalName = intervalName)
    dispatchment$tab <- rapbase::makeRegDispatchmentTab(session)
  })

  ## ui: legg til gyldig- og slett epost
  output$handleEmailControls <- renderUI({
    if (!grepl("^[A-Za-z0-9._%+-]+@[A-Za-z0-9.-]+\\.[A-Za-z]{2,}$",
               input$email)) {
      tags$p("Angi ny mottaker over")
    } else {
      if (input$email %in% sendTo$email) {
        actionButton("doDelEmail", "Slett epostmottaker",
                     icon = shiny::icon("trash"))
      } else {
        actionButton("doAddEmail", "Legg til epostmottaker",
                     icon = shiny::icon("pencil"))
      }
    }
  })

  ## ui: vis valgte mottakere
  output$recipients <- renderText(paste(sendTo$email, sep = "<br>"))


  ## lag tabell over gjeldende status for utsending
  output$activeDispatchments <- DT::renderDataTable(
    dispatchment$tab, server = FALSE, escape = FALSE, selection = 'none',
    options = list(dom = 'tp', ordning = FALSE), rownames = FALSE
  )

  ## ui: lag side som viser status for utsending, også når det ikke finnes noen
  output$dispatchmentContent <- renderUI({
    if (length(dispatchment$tab) == 0) {
      p("Det finnes ingen utendinger")
    } else {
      tagList(
        p("Aktive utsendinger:"),
        DT::dataTableOutput("activeDispatchments")
      )
    }
  })

  # Slett eksisterende auto rapport (gjelder for alle typer, e.g. abb og utsen)
  observeEvent(input$del_button, {
    selectedRepId <- strsplit(input$del_button, "_")[[1]][2]
    rapbase::deleteAutoReport(selectedRepId)
    rv$subscriptionTab <- rapbase::makeUserSubscriptionTab(session)
    dispatchment$tab <- rapbase::makeRegDispatchmentTab(session)
  })

}
