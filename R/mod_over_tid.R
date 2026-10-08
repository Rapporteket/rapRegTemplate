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
        shiny::uiOutput(outputId = ns("unitName"))
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
#' @param indicator_meta Data frame containing metadata for the indicators.
#'
#'@title Server fordeling
#'
#'@export

mod_over_tid_server <- function(id, data, indicator_meta, user) {
  shiny::moduleServer(
    id,
    function(input, output, session) {
      data_reactive <- shiny::reactive({
        data
      })

      valid_spc_data_reactive <- shiny::reactive({
        shiny::req(data_reactive(), user$role())
        filtered_data <- data_reactive()
        if (user$role() != "SC") {
          filtered_data <- dplyr::filter(filtered_data, as.character(.data$orgnr) == as.character(user$org()))
        }

        filtered_data |>
          dplyr::filter(
            !is.na(.data$var),
            !is.na(.data$denominator),
            .data$denominator > 0
          ) |>
          dplyr::mutate(unitName = as.character(.data$unitName))
      })

      selected_spc_data_reactive <- shiny::reactive({
        shiny::req(input$ind_id, input$unitName)
        valid_spc_data_reactive() |>
          dplyr::filter(.data$unitName == input$unitName, .data$ind_id == input$ind_id)
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

      output$unitName <- shiny::renderUI({
        shiny::req(input$ind_id, user$role())
        if (user$role() == "SC") {
          org_choices <- valid_spc_data_reactive() |>
            dplyr::filter(.data$ind_id == input$ind_id) |>
            dplyr::pull(.data$unitName) |>
            as.character() |>
            unique() |>
            sort()
        } else {
          org_choices <- valid_spc_data_reactive() |>
            dplyr::filter(.data$ind_id == input$ind_id, .data$orgnr == user$org()) |>
            dplyr::pull(.data$unitName) |>
            as.character() |>
            unique() |>
            sort()
        }

        choices <- stats::setNames(
          org_choices,
          org_choices
        )

        selectedUnitName <- if (!is.null(input$unitName) && input$unitName %in% org_choices) {
          input$unitName
        } else {
          org_choices[[1]]
        }

        shiny::selectInput(
          inputId = session$ns("unitName"),
          label = "Sykehus:",
          choices = choices,
          selected = selectedUnitName,
        )
      })

      plot_over_tid_reactive <- shiny::reactive({
        shiny::req(input$unitName, input$ind_id, cancelOutput = TRUE)
        shiny::req(nrow(selected_spc_data_reactive()) > 0, cancelOutput = TRUE)
        selected_indicator <- indicator_meta[indicator_meta$ind_id == input$ind_id, , drop = FALSE]
        plotSPC(
          selected_spc_data_reactive(),
          title = selected_indicator$title,
          subtitle = selected_indicator$short_description
        )
      })

      output$over_tid_plot <- shiny::renderPlot({
        shiny::req(input$unitName, input$ind_id, cancelOutput = TRUE)
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
