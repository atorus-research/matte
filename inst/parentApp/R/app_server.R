#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {

  metadata <- yaml::read_yaml("jobs/meta.yaml")
  metadata$data <- map(metadata$data, as.symbol)
  #metadata$modules$plot <- map(metadata$modules$plot, as.symbol)

  data <- readRDS("jobs/data.rds")

  plotServer("plot", data, metadata)
  tableServer("table", data, metadata)
}
