#' Create a jobs directory within the specified path
#'
#' This function creates a directory named 'jobs' and copies template files into
#' the newly created directory. Either R Markdown, Quarto, or both types of
#' files can be used as templates.
#'
#' @param path Directory for which jobs directory should be created
#' @param template Whether to use R Markdown, quarto, or both types of templates
#'
#' @return Path of the job directory created
#' @export
#'
use_batch_jobs <- function(path, template=c('rmarkdown', 'quarto')) {

  path_ <- suppressWarnings(normalizePath(path))

  template <- match.arg(template, several.ok=TRUE)

  if (!dir.exists(path_)) {
    stop(sprintf("The folder %s does not exist", path_), call.=FALSE)
  }

  job_dir <- file.path(path_, "jobs")

  if (dir.exists(job_dir)) {
    message(sprintf("The directory %s already exists.", job_dir))
    return(job_dir)
  } else {
    message(sprintf("Creating directory %s", job_dir))
    dir.create(job_dir)
  }

  for (t in template) {
    x <- ifelse(t == "rmarkdown", "Rmd", "qmd")
    fname <- sprintf("batch_template.%s", x)
    tmpf <- system.file(fname, package="matte")
    file.copy(tmpf, file.path(job_dir, fname))
  }

  job_dir
}
