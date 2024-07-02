#' Make Child app from Parent app with modules
#'
#' @param parent_app_name
#' @param parent_app_dir
#' @param child_app_name
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
#' make_child_app(parent_app_name = "parentApp",
#'                parent_app_dir = "inst/parentApp",
#'                child_app_name = "childTest",
#'                child_app_dir = "~/childTest")
#'
#'
#' make_child_app(parent_app_name = "parentApp",
#'                parent_app_dir = "inst/parentApp",
#'                child_app_name = "childTest2",
#'                child_app_dir = "~/childTest2",
#'                copy_jobs_dir = FALSE)
#'
#'

# TODO: add logging

make_child_app <- function(parent_app_name,
                           # do we need this?
                           parent_app_dir,
                           child_app_name,
                           # do we need this?
                           child_app_dir,
                           include_renv = FALSE,
                           copy_jobs_dir = TRUE,
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
        parent_app_name
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
  else if (job_file_type == "golem") {
    golem::create_golem(child_app_dir_)
  }
  else if (job_file_type == "rhino") {
    rhino::init(child_app_dir_)
  }
  else{
    dir.create(child_app_dir_)
  }


  ## create R folder
  dir.create(paste0(child_app_dir_, "/R"))


  ## start copying over all of the needed files from parent_app or templates
  file.copy(
    from = file.path(parent_app_dir_, "app.R"),
    to   = file.path(child_app_dir_, "app.R"),
    overwrite = overwrite
  )
  file.copy(
    from = file.path(parent_app_dir_, "R", "app_ui.R"),
    to   = file.path(child_app_dir_, "R", "app_ui.R"),
    overwrite = overwrite
  )
  file.copy(
    from = file.path(parent_app_dir_, "R", "app_server.R"),
    to   = file.path(child_app_dir_, "R", "app_server.R"),
    overwrite = overwrite
  )
  if (file.exists(file.path(parent_app_dir_, "manifest.json"))) {
    file.copy(
      from = file.path(parent_app_dir_, "manifest.json"),
      to   = file.path(child_app_dir_, "manifest.json"),
      overwrite = overwrite
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
    #copy over the template
    file.copy(
      from = system.file("inst", paste0("batch_template.", job_file_type)),
      to = file.path(
        paste0(child_app_dir_, "/jobs"),
        paste0("batch_template.", job_file_type)
      ),
      overwrite = overwrite
    )

  }

  ##not sure if this is the right approach for renv... might need to do this once we are in the child app Rproj
  if (include_renv) {
    renv::init(project = child_app_dir_, restart = FALSE)
  }

  #create dependencies.R
  fileCon <- file(file.path(child_app_dir_, "R", "dependencies.R"))
  writeLines(c(paste("#' @import", parent_app_name),
               "#'",
               "NULL"),
             con = fileCon)
  close(fileCon)

  file.path(child_app_dir_, "R", "dependencies.R")

}


# ##github parent app
#
# make_child_app_gh <- function(parent_app_repo,
#                               parent_app_branch,
#                            child_app_name, # do we need this?
#                            child_app_dir,
#                            github_username,
#                            github_path,
#                            include_renv = FALSE,
#                            include_jobs_dir = TRUE,
#                            format = "none",
#                            overwrite = FALSE) {
#
#   ## make sure all needed files/directories exist in parent_app
#
#   #what if path is github? should that be an explicit argument or just detected?
#
#   ##create the child app path dir (if it doesn't exist)
#   parent_app_dir_ <- normalizePath(parent_app_dir)
#   child_app_dir_ <- normalizePath(child_app_dir)
#
#   if(exists(child_app_dir_) && !overwrite) {
#     stop()
#   }
#   else{
#     create.dir(child_app_dir_)
#   }
#
#   ##change working directory to child app
#   setwd(child_app_dir_)
#
#
#   ## start copying over all of the needed files from parent_app
#   file.copy()
#
#   ##create all other files/directories from template
#
#   if(include_jobs_dir) {
#     dir.create(paste0())
#     #copy over the templates
#     file.copy()
#
#   }
#
#   if(include_renv) {
#     renv::init()## maybe use renv::scaffold?
#     devtools::install_local(parent_app_dir_)
#     renv::snapshot()
#   }
#
#   #update description
#   usethis::use_package(parent_app_name)
#
#   ##done!
#
#
#
# }
