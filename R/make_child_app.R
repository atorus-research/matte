#' Make Child app from Parent app with modules
#'
#' @param parent_github_repo `character` github repo of the parent in the form of "parent/repo"
#' @param child_app_dir `character` Path to new child app, including the child app directory
#' @param include_renv `logical` Include `renv` structure in child app or now. Default is `FALSE`
#' @param copy_jobs_dir `logical` Copy the `jobs` directory from the parent app (`TRUE`)
#'  or use jobs template in `matte/inst`. Default is `FALSE`
#' @param include_meta_yaml `logical` Include the template metadata yaml file in the child app. Default is `TRUE`
#' @param job_file_type `character` Type of job template to include ("Rmd" or "qmd"). Default is `Rmd`
#' @param framework `character` App framework used for child. Options are "golem", "rhino" or "none". Default is "none"
#' @param overwrite `logical` Allow function to overwrite existing child directory. Default is `FALSE`
#'
#' @export
#'
#' @examples
#' \dontrun{
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
#' }
#'

# TODO: add logging

make_child_app <- function(parent_github_repo,
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

  top_level_files <- gh::gh("/repos/:owner/:repo/contents", owner = "atorus-research", repo = "matteParentPkg")
  r_folder_files <- gh::gh("/repos/:owner/:repo/contents/R", owner = "atorus-research", repo = "matteParentPkg")
  jobs_folder_files <- gh::gh("/repos/:owner/:repo/contents/jobs", owner = "atorus-research", repo = "matteParentPkg")

  ##create the child app path dir (if it doesn't exist)
  parent_pkg_name <- strsplit(parent_github_repo, "/")[[1]][[2]]
  child_app_dir_ <- normalizePath(child_app_dir, winslash = "/") |> suppressWarnings()

  ## make sure all needed files/directories exist in parent_app
  if (length(Filter(function(x){x$name == "app.R"}, top_level_files)) == 0 ||
      length(Filter(function(x){x$name == "app_ui.R"}, r_folder_files)) == 0 ||
      length(Filter(function(x){x$name == "app_server.R"}, r_folder_files)) == 0) {
    stop(
      sprintf(
        "%s must contain app.R, R/app_ui.R and R/app_server.R in order to be duplicated",
        parent_pkg_name
      )
    )
  }

  if (copy_jobs_dir && length(Filter(function(x){x$name == "jobs"}, top_level_files)) == 0) {
    stop(
      sprintf(
        "%s must contain jobs folder when copy_jobs_dir = TRUE",
        parent_pkg_name
      )
    )
  }

  ## create child app directory

  if (file.exists(child_app_dir_) && overwrite) {
    warning(sprintf(
      "The child app folder, %s already exists. Overwriting the directory now.",
      child_app_dir_
    ))
  }
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
    # rhino::init(child_app_dir_)
  }
  else {
    dir.create(child_app_dir_)
  }

  ## create Rproj
  rproj_file <- paste0(child_app_dir_, "/", basename(child_app_dir), ".Rproj")
  if (!file.exists(rproj_file)) {
    file.create(rproj_file)
    fileCon <- file(rproj_file)
    writeLines(c("Version: 1.0",
                 "RestoreWorkspace: No",
                 "SaveWorkspace: No",
                 "AlwaysSaveHistory: Default",
                 "EnableCodeIndexing: Yes",
                 "UseSpacesForTab: Yes",
                 "NumSpacesForTab: 2",
                 "Encoding: UTF-8",
                 "RnwWeave: Sweave",
                 "LaTeX: pdfLaTeX",
                 "AutoAppendNewline: Yes",
                 "StripTrailingWhitespace: Yes",
                 "LineEndingConversion: Posix",
                 "BuildType: Package",
                 "PackageUseDevtools: Yes",
                 "PackageInstallArgs: --no-multiarch --with-keep.source",
                 "PackageRoxygenize: rd,collate,namespace"),
               con = fileCon)
    close(fileCon)
  }

  ## create Renviron (dev/prod detection)
  renviron_file <- paste0(child_app_dir_, "/", ".Renviron")
  if (!file.exists(renviron_file)) {
    file.create(renviron_file)
    fileCon <- file(renviron_file)
    writeLines(c("DEVELOPMENT_ENVIRONMENT=DEV",
                 "DEVELOPMENT_DATA_LOCATION_TYPE=LOCAL",
                 "DEVELOPMENT_DATA_LOCATION=data"),
               con = fileCon)
    close(fileCon)
  }

  ## create R folder
  if (!file.exists(paste0(child_app_dir_, "/R"))) {
    dir.create(paste0(child_app_dir_, "/R"))
  }


  ## start copying over all of the needed files from parent_app or templates
  ## We're going to download from github into a temp location and copy it over


  ## Pull parent repo info from github
  tmp_folder <- tempdir()

  download.file("https://raw.githubusercontent.com/atorus-research/matteParentPkg/main/R/app_ui.R", file.path(tmp_folder, "app_ui.R"))
  file.copy(
    from = file.path(tmp_folder, "app_ui.R"),
    to   = file.path(child_app_dir_, "R", "app_ui.R"),
    overwrite = TRUE
  )

  download.file("https://raw.githubusercontent.com/atorus-research/matteParentPkg/main/R/app_server.R", file.path(tmp_folder, "app_server.R"))
  file.copy(
    from = file.path(tmp_folder, "app_server.R"),
    to   = file.path(child_app_dir_, "R", "app_server.R"),
    overwrite = TRUE
  )


  if (length(Filter(function(x){x$name == "manifest.json"}, top_level_files)) == 1) {

    download.file("https://raw.githubusercontent.com/atorus-research/matteParentPkg/main/manifest.json", file.path(tmp_folder, "manifest.json"))
    file.copy(
      from = file.path(tmp_folder, "manifest.json"),
      to   = file.path(child_app_dir_, "manifest.json"),
      overwrite = TRUE
    )
  }
  ## add the app.R templates
  if (framework != "rhino") {

    file.copy(
      from = system.file(paste0("app_template_",framework,".R"),
                         package = "matte"),
      to   = file.path(child_app_dir_, "app.R"),
      overwrite = TRUE
    )

    # app.R installs parent package
    file_content <- readLines(file.path(child_app_dir_, "app.R"))
    file_content <- gsub("PARENT_PACKAGE_REPO", parent_github_repo, file_content)
    file_content <- gsub("PARENT_PACKAGE", parent_pkg_name, file_content)
    writeLines(file_content, file.path(child_app_dir_, "app.R"))
  }
  ## add the main.R template for rhino
  else {
    file.copy(
      from = system.file("main_template_rhino.R",
                         package = "matte"),
      to   = file.path(child_app_dir_, "R", "main.R"),
      overwrite = TRUE
    )
  }

  ## create jobs directory
  if (copy_jobs_dir) {
    dir.create(paste0(child_app_dir_, "/jobs"))

    dir.create(file.path(tmp_folder, "jobs"))

    for (jobs_file in jobs_folder_files) {
      download.file(jobs_file$download_url, file.path(tmp_folder, "jobs", jobs_file$name))

      file.copy(
        from = file.path(tmp_folder, "jobs", jobs_file$name),
        to = file.path(child_app_dir_, "jobs"),
        recursive = TRUE,
        overwrite = overwrite
      )
    }
  }
  else{
    dir.create(paste0(child_app_dir_, "/jobs"))
    #copy over the templates
    file.copy(
      from = system.file(paste0("batch_template.", job_file_type),
                         package = "matte"),
      to = file.path(
        paste0(child_app_dir_, "/jobs"),
        paste0("batch_template.", job_file_type)
      ),
      overwrite = overwrite
    )
  }

  ## if they want a meta yaml file, but do not have one in the parent app jobs folder, copy it from template
  if (include_meta_yaml && length(Filter(function(x){grepl(".yaml", x$name)}, jobs_folder_files)) == 0) {
    file.copy(
      from = system.file("meta_template.yaml",
                         package = "matte"),
      to = file.path(
        paste0(child_app_dir_, "/jobs"), "meta.yaml"
      ),
      overwrite = overwrite
    )
  }

  ## creates renv scaffolding, but user will need to snapshot for their app
  if (include_renv && framework != "rhino") {
    renv::scaffold(project = child_app_dir_)
  }

  #create dependencies.R
  fileCon <- file(file.path(child_app_dir_, "R", "dependencies.R"))
  writeLines(c(paste("#' @import", parent_pkg_name),
               "#'",
               "NULL"),
             con = fileCon)
  close(fileCon)

  ## update .Rbuildignore to ignore jobs folder
  write_union(path = file.path(child_app_dir_, ".Rbuildignore"),
              lines = "^jobs")

}
