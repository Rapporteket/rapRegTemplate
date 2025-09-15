pivot_ui <- function(id) {
  ns <- shiny::NS(id)

  shiny::tagList(
    shiny::fluidRow(
      shiny::column(12,
        rpivotTable::rpivotTableOutput(ns("pivotSurvey"))
      )
    )
  )
}


pivot_server <- function(id) {
  shiny::moduleServer(
    id,
    function(input, output, session) {

      # Last inn data
      regData <- getFakeRegData()

      output$pivotSurvey <- rpivotTable::renderRpivotTable({
        rpivotTable::rpivotTable(regData)
      })
    }
  )
}
