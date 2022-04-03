#' Load single measurement
#'
#' Loads a single measurement.
#' @details
#' #' The input files need to be in the following format (even though column names do not matter):
#'
#' | **`t`** |   | **`y`** |
#' | :----: | :----: |:----: |
#' | `t.1` |   | `y.1` |
#' | `...` |   | `...` |
#' | `t.n` |   | `y.n` |
#'
#' All columns except the first two are removed.
#'
#' @param file Character string containing the path to measurement file.
#' @param columns A vector of column numbers. The first entry will be used as the x-axis values, the second entry as y-axis values.
#' All other columns will be ignored. Default: `c(1,2)`.
#' @return A tibble with two columns named "t" and "y".
#' @examples
#'\dontrun{
#'# Using package example files from GitHub stored within
#'# https://github.com/Peter-T-Ruehr/forceR-data/blob/main/example_data.zip
#'
#'# store name of folder that contains file
#'input.folder <- "./example_data/corrected/"
#'
#'# store file name of file to load (first file of file.list)
#'file.name <- list.files(input.folder, pattern = "csv", full.names = TRUE)[1]
#'
#'# load a single file
#'df.1 <- load_single(file.name)
#'}
#' @export
load_single <- function (file,
                         columns = c(1:2)){
  # file <- "C:/Users/pruehr/Documents/forceR/vignettes/example_data/corrected/0980_cropped_ampdriftcorr.csv"
  # do a test read
  data_read <- read_csv(file, progress = FALSE, col_types = cols())

  # check if more than 2 columns were defined for plotting
  if(length(columns)>2) stop('More than two columns were defined by user.')
  # check if data as at least as many columns as max. number in columns variable
  if(ncol(data_read) < max(columns)) stop('Data has fewer columns than defined by user.')

  print(paste0("Removing all columns but ", colnames(data_read)[columns[1]], " and ", colnames(data_read)[columns[2]],
               ", renaming them to t and y and adding filename column based on the file name."))
  data_read <- data_read[,c(columns[1],columns[2])]
  # only keep first two columns (Time and final corrected Voltage) and measurement
  colnames(data_read) <- c("t", "y")
  data_read$filename <- sub("\\.[[:alnum:]]+$", "", basename(file))
  return(data_read)
}




#' Load Multiple Measurements
#'
#' Rapidly loads multiple measurements and prints progress and estimated time to completion
#'   to console.
#' @details
#' The input files need to be in the following format (even though column names do not matter):
#'
#' | **`t`** | **`y`** |
#' | :----: |:----: |
#' | `t.1` | `y.1` |
#' | `...` | `...` |
#' | `t.n` | `y.n` |
#'
#' All columns except the first two are removed.
#'
#' @param folder Character string containing the path to the measurements.
#' @param columns A vector of column numbers. The first entry will be used as the x-axis values, the second entry as y-axis values.
#' All other columns will be ignored. Default: `c(1,2)`.
#' @return Returns a tibble in the format
#'
#' | **`t`** | **`y`** | **`filename`**|
#' | :----: | :----: | :----: |
#' | `t.1` |  `y.1` |  `...`  |
#' | `...` | `...` | `...`  |
#' | `t.n` |  `y.n` |  `...`  |
#' @examples
#'\dontrun{
#'# Using package example files from GitHub stored within
#'# https://github.com/Peter-T-Ruehr/forceR-data/blob/main/example_data.zip
#'
#'# store name of folder that contains files
#'input.folder <- "./example_data/corrected/"
#'
#'# load a mutiple files
#' df.all <- load_mult(folder = input.folder,
#'                     columns = c(1:2))
#'}
#' @export
load_mult <- function (folder,
                       columns = c(1:2)){ # was: load.multiple.measurements.prepared
  pb <- NULL

  if(!endsWith("/", folder)){
    folder <- paste0(folder, "/")
  }
  file.list <- dir(folder, pattern = "\\.csv", recursive = FALSE, full.names = TRUE)
  print(paste0("Found ", length(file.list), " files."))

  # do a test read
  print("Checking if first file passes requirements...")
  data_read <- read_csv(file.list[1], progress = FALSE, col_types = cols())
  # check if more than 2 columns were defined for plotting
  if(length(columns)>2) stop('More than two columns were defined by user.')
  # check if data as at least as many columns as max. number in columns variable
  if(ncol(data_read) < max(columns)) stop(paste0('Data in ', file.list[1], ' has fewer columns than defined by user.'))
  print("Requirements met.")

  # reloading file list withouth full names
  file.list <- dir(folder, pattern = "\\.csv", recursive = FALSE)
  print(paste0("Removing all columns but ", colnames(data_read)[columns[1]], " and ", colnames(data_read)[columns[2]],
                                         ", renaming them to t and y and adding filename column based on the file name."))

  suppressWarnings(pb <- progress_estimated(length(file.list)))

  load_single_w_progress <- function (file){
    # file <- "C:/Users/pruehr/Documents/forceR/vignettes/example_data/corrected/0980_cropped_ampdriftcorr.csv"
    suppressWarnings(pb$tick()$print())
    data_read <- read_csv(file, progress = FALSE, col_types = cols())
    # only keep first two columns
    data_read <- data_read[,c(columns[1],columns[2])]
    colnames(data_read) <- c("t", "y")
    data_read$filename <- sub("\\.[[:alnum:]]+$", "", basename(file))
    return(data_read)
  }

  suppressWarnings(measurements <- paste0(folder,file.list) %>%
                     map_df(~load_single_w_progress(.)))
  # measurements$Time.sec <- measurements$Time.msec/1000
  return(measurements)
}
