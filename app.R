library(shiny)
library(bslib)

ui <- page_navbar(
   id = "page",
   title = "Parent Package",
   plotUI("plot"),
   tableUI("table")
)

server <- function(input, output, session) {

   # THIS IS A YAML FILE
   metadata <- list(
      metadata = list(
         filepath = "",
      ),
      data = list(
         var_1 = "col1",
         var_2 = "col2",
         var_3 = "col3",
         var_4 = "col4"
      ),
      modules = list(
         plot = list(
            x_var = "var_1",
            y_var = "var_2"
         ),
         table = list(
            ("var1")
         )
      )
   )

   # mtcars and then we make a new column or two
   # in data_prep.R and use those here
   data <- mtcars

   plotServer("plot", data, metadata)
   tableServer("table", data, metadata)
}

shinyApp(ui, server)
