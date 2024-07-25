## install parent package
if (!require(PARENT_PACKAGE, quietly = TRUE)) {
  remotes::install_github("PARENT_PACKAGE_REPO")
}

## install matte
if (!require(matte, quietly = TRUE)) {
  remotes::install_github("atorus-research/matte@data-dev-prod")
}

## Launch the ShinyApp
source("R/app_ui.R")
source("R/app_server.R")

shiny::shinyApp(ui = app_ui, server = app_server)
