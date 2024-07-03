#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @export
#' @noRd
app_ui <- function(input, output, session) {
  page_navbar(
    id = "page",
    title = "Parent Package",
    theme = bs_theme(version = 4, bootswatch = "minty"), # Using a Bootswatch theme
    nav_panel(
      title = "Plot",
      plotUI("plot"),
    ),
    nav_panel(
      title = "Table",
      tableUI("table")
    )
  )
}


#' Add external Resources to the Application
#'
#' This function is internally used to add external
#' resources inside the Shiny application.
#'
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @export
#' @noRd
golem_add_external_resources <- function() {
  add_resource_path(
    "www",
    app_sys("app/www")
  )

  tags$head(
    favicon(),
    bundle_resources(
      path = app_sys("app/www"),
      app_title = "parentApp"
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert()
  )
}
