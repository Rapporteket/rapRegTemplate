#' Shiny module providing GUI and server logic for the plot tab
#'
#' @param id Character string module namespace
#' @return An shiny app ui object
#' @export

mod_over_tid_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::sidebarLayout(

      shiny::sidebarPanel(
        width = 4,
        shiny::uiOutput(outputId = ns("ind_ids")),
        shiny::uiOutput(outputId = ns("orgnr"))
      ),

      shiny::mainPanel(
        shiny::tabsetPanel(
          id = ns("tab"),
          shiny::tabPanel(
            "Figur",
            value = "Fig",
            shiny::plotOutput(outputId = ns("over_tid_plot")),
            shiny::downloadButton(
              ns("nedlastning_over_tid_plot"),
              "Last ned figur"
            )
          )
        )
      )
    )
  )
}



#' @param id Character string module namespace
#' @param data Data frame containing the data to be plotted.
#'
#'@title Server fordeling
#'
#'@export

mod_over_tid_server <- function(id, data, indicator_meta) {
  shiny::moduleServer(
    id,
    function(input, output, session) {
      data_reactive <- shiny::reactive({
        data
      })

      valid_spc_data_reactive <- shiny::reactive({
        shiny::req(data_reactive())
        data_reactive() |>
          dplyr::filter(
            !is.na(.data$var),
            !is.na(.data$denominator),
            .data$var != 0,
            .data$denominator != 0
          ) |>
          dplyr::mutate(orgnr = as.character(.data$orgnr))
      })

      selected_spc_data_reactive <- shiny::reactive({
        shiny::req(input$ind_id, input$orgnr)
        valid_spc_data_reactive() |>
          dplyr::filter(.data$orgnr == input$orgnr, .data$ind_id == input$ind_id)
      })

      output$ind_ids <- shiny::renderUI({
        choices <- stats::setNames(
          indicator_meta$ind_id,
          indicator_meta$title
        )
        shiny::selectInput(
          inputId = session$ns("ind_id"),
          label = "Indikator:",
          choices = choices
        )
      })

      output$orgnr <- shiny::renderUI({
        shiny::req(input$ind_id)
        org_choices <- valid_spc_data_reactive() |>
          dplyr::filter(.data$ind_id == input$ind_id) |>
          dplyr::pull(.data$orgnr) |>
          as.character() |>
          unique() |>
          sort()

        choices <- stats::setNames(
          org_choices,
          org_choices
        )

        selected_orgnr <- if (!is.null(input$orgnr) && input$orgnr %in% org_choices) {
          input$orgnr
        } else {
          org_choices[[1]]
        }

        shiny::selectInput(
          inputId = session$ns("orgnr"),
          label = "Sykehus:",
          choices = choices,
          selected = selected_orgnr,
        )
      })

      plot_over_tid_reactive <- shiny::reactive({
        shiny::req(input$orgnr, input$ind_id, cancelOutput = TRUE)
        shiny::req(nrow(selected_spc_data_reactive()) > 0, cancelOutput = TRUE)
        selected_indicator <- indicator_meta[indicator_meta$ind_id == input$ind_id, , drop = FALSE]
        plotSPC(
          selected_spc_data_reactive(),
          title = selected_indicator$title,
          subtitle = selected_indicator$short_description
        )
      })

      output$over_tid_plot <- shiny::renderPlot({
        shiny::req(input$orgnr, input$ind_id, cancelOutput = TRUE)
        shiny::req(nrow(selected_spc_data_reactive()) > 0, cancelOutput = TRUE)
        plot_over_tid_reactive()
      })

      # Lag nedlastning
      output$nedlastning_over_tid_plot <-  shiny::downloadHandler(
        filename = function() {
          paste("plot_over_tid", Sys.Date(), ".pdf", sep = "")
        },
        content = function(file) {
          pdf(file, onefile = TRUE, width = 15, height = 9)
          plot(plot_over_tid_reactive())
          dev.off()
        }
      )
    }
  )
}
