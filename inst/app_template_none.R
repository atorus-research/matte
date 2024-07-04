## install parent package
if (!require(PARENT_PACKAGE, quietly = TRUE)) {
  remotes::install_github("PARENT_PACKAGE_REPO")
}

## Launch the ShinyApp

shinyApp(ui = app_ui,
         server = app_server)
