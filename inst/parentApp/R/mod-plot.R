plotUI <- function(id) {
  ns <- NS(id)
  shiny::plotOutput(ns("plot"))
}

plotServer <- function(id, data, metadata) {
  moduleServer(
    id,
    function(input, output, session) {
       output$plot <- renderPlot(
          ggplot() +
            geom_point(data,
                       mapping = aes(x = mpg,
                                     y = hp,
                                     color = am_fac)) +
            theme_bw() +
            theme(legend.title = element_blank())
       )
    }
  )
}
