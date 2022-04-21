#' Convert LJStream *.dat file to standard time series
#'
#' Converts LJStream *.dat file to standard time series.
#' @param file File path to raw measurement (*.dat file).
#' @param write.files A logical value indicating if results should be
#' written as file. Default: `TRUE`.
#'@return Returns and, if `write.files == TRUE`, saves converted data in
#' CSV-format in new subfolder "./converted". If this folder does not exist,
#' it will be created. \cr
#' \cr
#' The output tibble has the following format:
#'
#' | **`t`** |   | **`y`** |
#' | :----: | :----: |:----: |
#' | `t.1` |   | `y.1` |
#' | `...` |   | `...` |
#' | `t.n` |   | `y.n` |
#' @examples
#' # get file path of forceR example file
#' filename <- forceR_example(type = "LJStream")
#' file.converted <- convert_measurement (file = filename,
#'                        write.files = FALSE)
#' file.converted
#'
#' @export
convert_measurement <- function (file,
                                 write.files = TRUE){

  if(!is.character(file)) stop ("'file' must be a character string.")

  Time <- y0 <- NULL

  if(!file.exists(file)) stop(paste0("File ", file, " does not exist!"))

  folder <- dirname(file)
  if(!str_sub(folder, -1) == '/'){
    folder <- paste0(folder, "/")
  }
  path.target = paste0(folder, "converted")
  if(!dir.exists(path.target)){
    dir.create(path.target, showWarnings = FALSE)
  } else {
    # print(paste0(path.target, " already exists."))
  }
  if(!str_sub(path.target, -1) == '/'){
    path.target <- paste0(path.target, "/")
  }

  res.reduction <- 10
  file.name <- basename(file)

  data <- read_delim(file, delim = "\t", skip = 6,
                     show_col_types = FALSE)

  # Check what the decimal is. Returns "" when it is a full stop, and the
  #   decimal, e.g. "," if it is not.
  decimal <- gsub('[[:digit:]]+', '', data[1,1])

  if(decimal != ""){
    # print(paste0("Converting \'", decimal, "\' to \'.\'..."))
    data$Time <- as.numeric(gsub(decimal, ".", gsub("\\.", "", data$Time))) * 1000
    data$y0 <- as.numeric(gsub(decimal, ".", gsub("\\.", "", data$y0)))
  }

  data <- data %>% as_tibble() %>%
    select(Time, y0) %>%
    rename(t = Time, y = y0)

  if(write.files == TRUE){
    write_csv(data, paste0(path.target, gsub("\\.dat$", "\\.csv", basename(file))), quote = "none")
  }
  return(data)
  # print("Done!")
}


#' Crop Time Series
#'
#' Interactive function to crop a time series.
#' @param file File path to measurement.
#' @param write.files A logical value indicating if results should be
#' written as file. Default: `TRUE`.
#' @details Select points at start and end of desired part of measurements.
#' Only the last two points will be
#'   taken into account to allow the user to correct erroneous clicks.
#' @return Returns and, if `write.files == TRUE`, saves cropped data in
#' CSV-format in new subfolder "./cropped". If this folder does not exist,
#' it will be created. \cr
#' \cr
#' The tibble has the following format:
#'
#' | **`t`** |   | **`y`** |
#' | :----: | :----: |:----: |
#' | `t.1` |   | `y.1` |
#' | `...` |   | `...` |
#' | `t.n` |   | `y.n` |
#'
#' @examples
#' # get file path of forceR example file
#' filename <- forceR_example(type = "raw")
#'
#'# plot measurement
#'plot_measurement(filename)
#'
#'# crop file - without storing result as file
#'file.cropped <- crop_measurement(file,
#'                   write.files = FALSE)
#'
#'file.cropped
#'
#'\donttest{
#'# crop file - with result stored in "./cropped"
#'crop_measurement(filename,
#'                   write.files = TRUE)
#'
#'# plot results
#'# define folder where cropped file wass stored:
#'cropped.folder <- "./cropped"
#'
#'# plot the cropped file - this will only work if you used your own
#'filename.cropped <- file.path(dirname(filename), "cropped",
#'                                 gsub("\\.csv", '_cropped.csv', basename(filename)))
#'
#'# plot the cropped file
#'plot_measurement(filename.cropped)
#'}
#' @export
crop_measurement <- function (file,
                              write.files = TRUE){

  if(!is.character(file)) stop ("'file' must be a character string.")


  if(!file.exists(file)) stop(paste0("File ", file, " does not exist!"))

  y <- NULL

  folder <- dirname(file)
  if(!str_sub(folder, -1) == '/'){
    folder <- paste0(folder, "/")
  }
  path.target = paste0(folder, "cropped")
  if(!dir.exists(path.target)){
    dir.create(path.target, showWarnings = FALSE)
  } else {
    # print(paste0(path.target, " already exists."))
  }
  if(!str_sub(path.target, -1) == '/'){
    path.target <- paste0(path.target, "/")
  }

  res.reduction <- 10
  file.name <- basename(file)

  data = read_csv(file, show_col_types = FALSE)

  colnames(data) <- c("t", "y")

  # Check what the decimal is. Returns "" when it is a full stop, and the
  #   decimal, e.g. "," if it is not.
  decimal <- gsub('[[:digit:]]+', '', data[1,1])

  if(decimal != ""){
    warning(paste0("Converting decimal separator \'", decimal, "\' to \'.\'..."))
    data$t <- as.numeric(gsub(decimal, ".", gsub("\\.", "", data$t))) * 1000
    data$y <- as.numeric(gsub(decimal, ".", gsub("\\.", "", data$y)))
  }

  sampling_rate_Hz <- 1/round(data$t[2] - data$t[1], digits = 4) * 1000

  plot(data[,1:2], type = 'l',
       main = paste0(file.name, " (", sampling_rate_Hz, " Hz)"), xlab = "time [m.sec]", ylab = "y")

  message("Select beginning and end of measurement and click \"Finish\". Only the last two points will be used.")
  cutoffs <- locator(type = "n")

  # only take last two cutoffs
  cutoffs$x <- cutoffs$x[(length(cutoffs$x)-1):(length(cutoffs$x))]
  cutoffs$y <- cutoffs$y[(length(cutoffs$y)-1):(length(cutoffs$y))]

  # convert cutoffs outside the graph area to time value(s) at beginning and/or end of graph area
  if(cutoffs$x[length(cutoffs$x)-1] < min(data$t)){cutoffs$x[1] <- min(data$t)}
  if(cutoffs$x[length(cutoffs$x)] > max(data$t)){cutoffs$x[2] <- max(data$t)}

  ceiling_dec <- function(x, level=1) round(x + 5*10^(-level-1), level)
  floor_dec <- function(x, level=1) round(x - 5*10^(-level-1), level)
  start_time_msec <- which(data$t == ceiling_dec(cutoffs$x[length(cutoffs$x)-1], level = -1))
  end_time_msec <-  which(data$t == floor_dec(cutoffs$x[length(cutoffs$x)], level = -1))

  data_cut <- data[start_time_msec:end_time_msec,]
  data_cut$t <- data_cut$t - data_cut$t[1]

  # define x ticks
  x_ticks <- seq(0, data_cut$t[nrow(data_cut)], 1000)

  # plot with amplifier_scaling - correction
  plot(data_cut$t[seq(1,nrow(data_cut),res.reduction)], data_cut$y[seq(1,nrow(data_cut),res.reduction)], type = 'l',
       main = paste0(file.name, " (", sampling_rate_Hz, " Hz)"), xlab = "t", ylab = "y", xaxt = "n")

  axis(side = 1,
       at = x_ticks,
       labels = x_ticks)

  data_cut <- data_cut %>%
    select(t, y)
  if(write.files == TRUE){
    write_csv(data_cut, paste0(path.target, sub("\\.[[:alnum:]]+$", "", file.name), "_cropped.csv"), quote = "none")
  }
  return(data_cut)
  # print("Done!")
}

