#' Shiny module providing GUI and server logic for the Andeler tab
#'
#' @param id Character string module namespace
#' @return An shiny app ui object
#' @export

mod_andeler_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    "Andel basert på variabel og grenser",
    shiny::sidebarLayout(
      shiny::sidebarPanel(
        width = 3,
        shiny::uiOutput(outputId = ns("ind_ids")),
        shiny::downloadButton(
          outputId = ns("downloadandelPlot"),
          label = "Last ned!"
        )
      ),
      shiny::mainPanel(
        shiny::plotOutput(outputId = shiny::NS(id, "andelPlot"))
      )
    )
  )
}

#' Server logic for andel plot
#'
#' @param id Character string module namespace
#' @param data Data frame containing the data to be plotted.
#' @param indicator_meta Data frame containing metadata for the indicators.
#'
#' @return A Shiny app server object
#' @export

mod_andeler_server <- function(id, data, indicator_meta) {
  shiny::moduleServer(
    id,
    function(input, output, session) {

      data_reactive <- shiny::reactive({
        data
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

      plotReactive <- shiny::reactive({
        shiny::req(input$ind_id)
        selected_indicator <- indicator_meta[indicator_meta$ind_id == input$ind_id, , drop = FALSE]

        data <- data_reactive() |>
          dplyr::filter(.data$ind_id == input$ind_id)

        rapFigurer::plotIndikator(
          data,
          title = selected_indicator$title[[1]],
          shortDescription = selected_indicator$short_description[[1]],
          showYear = max(data$year, na.rm = TRUE),
          kvalIndgrenser = selected_indicator$kvalIndgrenser[[1]],
          levelDirection = selected_indicator$levelDirection[[1]]
        )
      })

      output$andelPlot <- shiny::renderPlot({
        plotReactive()
      },
      height = function() {
        shiny::req(input$ind_id)
        selected_data <- data_reactive()[data_reactive()$ind_id == input$ind_id, , drop = FALSE]
        n_bins <- length(unique(selected_data$unitName))
        min(700, n_bins * 30)
      }
      )

      output$downloadandelPlot <-  shiny::downloadHandler(
        filename = function() {
          paste("plot_andeler", Sys.Date(), ".pdf", sep = "")
        },
        content = function(file) {
          pdf(file, onefile = TRUE, width = 15, height = 9)
          plot(plotReactive())
          dev.off()
        }
      )


    }
  )
}
