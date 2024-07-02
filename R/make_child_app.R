#' Make Child app from Parent app with modules
#'
#' @param parent_app_dir
#' @param child_app_dir
#' @param include_renv
#' @param copy_jobs_dir
#' @param job_file_type
#' @param framework
#' @param overwrite
#'
#' @return
#' @export
#'
#' @examples
#' make_child_app(parent_app_dir = "inst/parentApp",
#'                child_app_dir = "~/childTest")
#'
#'
#' make_child_app(parent_app_dir = "inst/parentApp",
#'                child_app_dir = "~/childTest2",
#'                copy_jobs_dir = FALSE,
#'                include_renv = TRUE)
#'
#' make_child_app(parent_app_dir = "inst/parentApp",
#'                child_app_dir = "~/childTest3",
#'                include_renv = TRUE,
#'                framework = "golem")
#'
#'

# TODO: add logging

make_child_app <- function(parent_app_dir,
                           child_app_dir,
                           include_renv = FALSE,
                           copy_jobs_dir = FALSE,
                           include_meta_yaml = TRUE,
                           job_file_type = "Rmd",
                           framework = "none",
                           overwrite = FALSE) {

  #TODO: what if the path to parent is github? should that be an explicit argument or just detected? clone it into a tmp folder?

  if (!job_file_type %in% c("Rmd", "qmd")) {
    stop("`job_file_type` must be one of: 'Rmd' or 'qmd'")
  }
  if (!framework %in% c("none", "rhino", "golem")) {
    stop("`framework` must be one of: 'none', 'golem' or 'rhino'")
  }

  ##create the child app path dir (if it doesn't exist)
  parent_app_dir_ <- normalizePath(parent_app_dir, winslash = "/")
  child_app_dir_ <-
    normalizePath(child_app_dir, winslash = "/") %>% suppressWarnings()

  ## make sure all needed files/directories exist in parent_app
  if (!file.exists(file.path(parent_app_dir_, "app.R")) ||
      !file.exists(file.path(parent_app_dir_, "R", "app_ui.R")) ||
      !file.exists(file.path(parent_app_dir_, "R", "app_server.R"))) {
    stop(
      sprintf(
        "%s must contain app.R, R/app_ui.R and R/app_server.R in order to be duplicated",
        basename(parent_app_dir_)
      )
    )
  }

  if (copy_jobs_dir && !file.exists(file.path(parent_app_dir_, "jobs"))) {
    stop(
      sprintf(
        "%s must contain jobs folder when copy_jobs_dir = TRUE",
        basename(parent_app_dir_)
      )
    )
  }

  ## create child app directory
  if (file.exists(child_app_dir_) && !overwrite) {
    stop(
      sprintf(
        "The folder %s already exists. Please specify new folder, or specify `overwrite = TRUE`",
        child_app_dir_
      )
    )
  }
  else if (framework == "golem") {
    golem::create_golem(child_app_dir_, open = FALSE)
  }
  else if (framework == "rhino") {
    rhino::init(child_app_dir_)
  }
  else{
    dir.create(child_app_dir_)
  }


  ## create R folder
  if (!file.exists(paste0(child_app_dir_, "/R"))) {
    dir.create(paste0(child_app_dir_, "/R"))
  }


  ## start copying over all of the needed files from parent_app or templates
  file.copy(
    from = file.path(parent_app_dir_, "app.R"),
    to   = file.path(child_app_dir_, "app.R"),
    overwrite = TRUE
  )
  file.copy(
    from = file.path(parent_app_dir_, "R", "app_ui.R"),
    to   = file.path(child_app_dir_, "R", "app_ui.R"),
    overwrite = TRUE
  )
  file.copy(
    from = file.path(parent_app_dir_, "R", "app_server.R"),
    to   = file.path(child_app_dir_, "R", "app_server.R"),
    overwrite = TRUE
  )
  if (file.exists(file.path(parent_app_dir_, "manifest.json"))) {
    file.copy(
      from = file.path(parent_app_dir_, "manifest.json"),
      to   = file.path(child_app_dir_, "manifest.json"),
      overwrite = TRUE
    )
  }

  ## create jobs directory
  if (copy_jobs_dir) {
    dir.create(paste0(child_app_dir_, "/jobs"))
    file.copy(
      from = paste0(parent_app_dir_, "/jobs"),
      to = child_app_dir_,
      recursive = TRUE,
      overwrite = overwrite
    )
  }
  else{
    dir.create(paste0(child_app_dir_, "/jobs"))
    #copy over the templates
    file.copy(
      from = system.file("inst", paste0("batch_template.", job_file_type)),
      to = file.path(
        paste0(child_app_dir_, "/jobs"),
        paste0("batch_template.", job_file_type)
      ),
      overwrite = overwrite
    )
  }

  ## if they want a meta yaml file, but do not have one in the parent app jobs folder, copy it from template
  if (include_meta_yaml && all(!grepl(".yaml", list.files(paste0(parent_app_dir_, "/jobs"))))) {
    file.copy(
      from = system.file("inst", "meta_template.yaml"),
      to = file.path(
        paste0(child_app_dir_, "/jobs"), "meta.yaml"
      ),
      overwrite = overwrite
    )
  }

  ## not sure if this is the right approach for renv...
  ## might need to do this once we are in the child app Rproj, but works for now
  if (include_renv && framework != "rhino") {
    renv::init(project = child_app_dir_, restart = FALSE)
  }

  #create dependencies.R
  fileCon <- file(file.path(child_app_dir_, "R", "dependencies.R"))
  writeLines(c(paste("#' @import", basename(parent_app_dir_)),
               "#'",
               "NULL"),
             con = fileCon)
  close(fileCon)

  ## update .Rbuildignore to ignore jobs folder
  write_union(path = file.path(child_app_dir_, ".Rbuildignore"),
              lines = "^jobs")

}
