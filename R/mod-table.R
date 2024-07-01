tableUI <- function(id) {
  ns <- NS(id)
  tagList(
     reactable::reactableOutput(ns("table"))
  )
}

tableServer <- function(id, data, metadata) {
  moduleServer(
    id,
    function(input, output, session) {
       output$table <- renderReactable(
          reactable(data |> select(metadata$modules$table))
       )
    }
  )
}
