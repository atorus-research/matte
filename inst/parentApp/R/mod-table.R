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
      selection <- strsplit(metadata$modules$table, ", ") %>% unlist()
       output$table <- renderReactable(
          reactable(data |> dplyr::select(!!selection))
       )
    }
  )
}
