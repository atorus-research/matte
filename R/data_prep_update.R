## Handle local data in `data folder`


get_updated_time <- function(data_loc) {
  current_files <- list.files(file.path(data_loc))

  current_data_info <- file.info(file.path(data_loc, current_files))
  current_data_info$file_name <- current_files

  rownames(current_data_info) <- NULL
  current_data_info <- current_data_info[, c("file_name", "mtime")]

  return(current_data_info)
}

#' @export
run_data_prep_local_data_folder <- function(dev_data_loc) {

  if (length(list.files(dev_data_loc)) == 0) {
    message("There is no data. Skipping data processing")
    return(invisible(NULL))
  }

  # Look for the file that has file name & last updated
  data_update_log <- file.path("jobs", "data_update_log.csv")

  # If not exist, make one
  if (!file.exists(data_update_log)) {
    curr_data_info <- get_updated_time(dev_data_loc)
    write.csv(curr_data_info, data_update_log, row.names = FALSE)
  }

  data_update_log <- read.csv(data_update_log)

  # Pull the current information in the data folder to compare against log
  current_data_info <- get_updated_time(dev_data_loc)

  # Conditionally run data prep
  if (nrow(current_data_info) > nrow(data_update_log)) {
    message("New data has been added! Running data prep")
    rmarkdown::render("jobs/data_prep.Rmd")
  } else if (nrow(current_data_info) < nrow(data_update_log)) {
    message("Data has been removed! Running data prep")
    rmarkdown::render("jobs/data_prep.Rmd")
  } else {
    joined <- data_update_log |>
      dplyr::inner_join(current_data_info, by = "file_name")

    n_updates <- joined |>
      dplyr::mutate(
        mtime.x = lubridate::ymd_hms(mtime.x),
        mtime.y = lubridate::ymd_hms(mtime.y)
      ) |>
      dplyr::filter(mtime.x < mtime.y) |>
      nrow()

    if (n_updates > 0) {
      message("Data has been updated! Running data prep")
      rmarkdown::render("jobs/data_prep.Rmd")
    } else {
      message("No change in data, skipping data prep")
    }
  }
  return(invisible(NULL))
}


