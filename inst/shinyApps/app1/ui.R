library(shiny)
library(shinyalert)
library(rapbase)

addResourcePath('rap', system.file('www', package='rapbase'))
regTitle = "rapRegTemplate"

ui <- tagList(
  navbarPage(
    title = div(a(includeHTML(system.file("www/logo.svg",
                                          package = "rapbase"))),
                regTitle),
    windowTitle = regTitle,
    theme = "rap/bootstrap.css",
    id = "tabs",

    tabPanel("Veiledning",
      mainPanel(width = 12,
        htmlOutput("veiledning", inline = TRUE),
        useShinyalert(),
        appNavbarUserWidget(user = uiOutput("appUserName"),
                            organization = uiOutput("appOrgName"),
                            addUserInfo = TRUE)
      )
    ),
    tabPanel("Figur og tabell"
      # ,
      # sidebarLayout(
      #   sidebarPanel(width = 3,
      #     selectInput(inputId = "var",
      #                 label = "Variabel:",
      #                 c("mpg", "disp", "hp", "drat", "wt", "qsec")),
      #     sliderInput(inputId = "bins",
      #                 label = "Antall grupper:",
      #                 min = 1,
      #                 max = 10,
      #                 value = 5)
      #   ),
      #   mainPanel(
      #     tabsetPanel(
      #       tabPanel("Figur", plotOutput("distPlot")),
      #       tabPanel("Tabell", tableOutput("distTable"))
      #     )
      #   )
      # )
    ),
    tabPanel("Samlerapport"
        # ,
        # tabPanel("Fordeling av mpg",
        #   sidebarLayout(
        #     sidebarPanel(width = 3,
        #       selectInput(inputId = "varS",
        #                   label = "Variabel:",
        #                   c("mpg", "disp", "hp", "drat", "wt", "qsec")),
        #       sliderInput(inputId = "binsS",
        #                   label = "Antall grupper:",
        #                   min = 1,
        #                   max = 10,
        #                   value = 5),
        #       downloadButton("downloadSamlerapport", "Last ned!")
        #     ),
        #     mainPanel(
        #       uiOutput("samlerapport")
        #     )
        #   )
        # )
      ),
    tabPanel("Abonnement"
      # ,
      # sidebarLayout(
      #   sidebarPanel(width = 3,
      #     selectInput("subscriptionRep", "Rapport:", c("Samlerapport1", "Samlerapport2")),
      #     selectInput("subscriptionFreq", "Frekvens:",
      #                 list(Årlig = "Årlig-year",
      #                      Kvartalsvis = "Kvartalsvis-quarter",
      #                      Månedlig = "Månedlig-month",
      #                      Ukentlig = "Ukentlig-week",
      #                      Daglig = "Daglig-DSTday"),
      #                 selected = "Månedlig-month"),
      #     actionButton("subscribe", "Bestill", icon = shiny::icon("paper-plane"))
      #   ),
      #   mainPanel(
      #     uiOutput("subscriptionContent")
      #   )
      # )
    ),
    tabPanel("Utsending"
      # ,
      # sidebarLayout(
      #   sidebarPanel(width = 3,
      #     uiOutput("report"),
      #     uiOutput("freq"),
      #     textInput("email", "Epostmottakere:"),
      #     uiOutput("editEmail"),
      #     htmlOutput("recipients"),
      #     tags$hr(),
      #     uiOutput("makeDispatchment")
      #   ),
      #   mainPanel(
      #     uiOutput("dispatchmentContent")
      #   )
      # )
    )

  ) # navbarPage
) # tagList
