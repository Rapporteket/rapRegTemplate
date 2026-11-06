#' Shiny module providing GUI and server logic for the report tab
#'
#' @param id Character string module namespace
#' @return An shiny app ui object
#' @export

samlerapport_ui <- function(id) {
  ns <- shiny::NS(id)

  shiny::tabPanel(
    "Samlerapport",
    shiny::sidebarLayout(
      shiny::sidebarPanel(
        width = 3,
        shiny::uiOutput(outputId = ns("unitName")),
        shiny::selectInput(
          inputId = ns("formatS"),
          label = "Velg format for nedlasting:",
          choices = list(PDF = "pdf", HTML = "html")
        ),
        shiny::downloadButton(
          outputId = ns("downloadSamlerapport"),
          label = "Last ned!"
        )
      ),
      shiny::mainPanel(
        shiny::uiOutput(ns("samlerapport"))
      )
    )
  )
}

#' Server logic for samlerapport
#'
#' @param id Character string module namespace
#'
#' @return A Shiny app server object
#' @export

samlerapport_server <- function(id, data, indicator_meta, user) {
  shiny::moduleServer(
    id,
    function(input, output, session) {

      data_reactive <- shiny::reactive({
        shiny::req(data, user$role())
        if (user$role() == "SC") {
          data
        } else {
          dplyr::filter(data, as.character(.data$orgnr) == as.character(user$org()))
        }
      })

      output$unitName <- shiny::renderUI({
        shiny::req(user$role())
        org_choices <- data_reactive() |>
          dplyr::filter(
            !is.na(.data$orgnr),
            !is.na(.data$unitName),
            !is.na(as.integer(.data$year)),
            !is.na(.data$ind_id),
            .data$ind_id %in% as.character(indicator_meta$ind_id),
            !is.na(.data$var),
            !is.na(.data$denominator),
            .data$denominator > 0
          ) |>
          dplyr::group_by(.data$orgnr) |>
          dplyr::filter(dplyr::n_distinct(.data$ind_id) >= 2L) |>
          dplyr::ungroup() |>
          dplyr::distinct(.data$orgnr, .data$unitName) |>
          dplyr::arrange(.data$unitName)

        shiny::req(nrow(org_choices) > 0)

        choices <- stats::setNames(
          as.character(org_choices$orgnr),
          org_choices$unitName
        )

        selected_org <- if (!is.null(input$unitName) && input$unitName %in% choices) {
          input$unitName
        } else {
          as.character(org_choices$orgnr[[1]])
        }

        shiny::selectInput(
          inputId = session$ns("unitName"),
          label = "Sykehus:",
          choices = choices,
          selected = selected_org
        )
      })
      # Samlerapport
      ## vis
      output$samlerapport <- shiny::renderUI({
        shiny::req(input$unitName, data_reactive())
        reportProcessor(
          report = "local_monthly",
          outputType = "html_fragment",
          data = data_reactive(),
          indicator_meta = indicator_meta,
          orgnr = input$unitName
        )
      })

      ## last ned
      output$downloadSamlerapport <- shiny::downloadHandler(
        filename = function() {
          shiny::req(input$formatS)
          basename(tempfile(pattern = "rapRegTemplateSamlerapport",
                            fileext = paste0(".", input$formatS)))
        },
        content = function(file) {
          shiny::req(input$unitName, input$formatS)
          fn <- reportProcessor(
            report = "local_monthly",
            outputType = input$formatS,
            data = data_reactive(),
            indicator_meta = indicator_meta,
            orgnr = input$unitName
          )
          file.rename(fn, file)
        }
      )
    }
  )
}
