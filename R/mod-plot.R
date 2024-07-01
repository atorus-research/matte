plotUI <- function(id) {
  ns <- NS(id)
  shiny::plotOutput(ns("plot"))
}

plotServer <- function(id, data, metadata) {
  moduleServer(
    id,
    function(input, output, session) {
       output$plot <- renderPlot(
          data
       )
    }
  )
}
