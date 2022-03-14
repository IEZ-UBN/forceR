#' Charge Amplifier Drift Correction
#'
#' Removes the systemic, asymptotical drift of charge amplifiers with resistor-capacitor (RC) circuits.
#'
#' @param folder Path to folder containing measurements.
#' @param tau Numeric time constant of charge amplifier in the same time unit as the measurement data.
#' Default: `9400`
#' @param print.to.screen A logical value indicating if results should be plotted in the current R plot.
#' device. Slows down process. Default: `FALSE`.
#' @param print.to.pdf  A logical value indicating  if results should be saved as PDFs. Does not slow
#' down the process as much as printing to the R plot device and is considered necessary to quality check
#' the results. Default: `TRUE`.
#' @param res.reduction A numeric value to reduce the number of time steps by during plotting. Speeds up
#' the plotting process and reduces PDF size. Has no effect on the results, only on the plots. Default: `10`.
#' @param start.file.number A numeric value indicating at which file the loop should start. Helpful in
#' case the loop stopped during a previous run. Default: `1`.
#'
#' @details
#' The input files need to be in the following format:
#'
#' | `t` |   | `y` |
#' | :----: | :----: |:----: |
#' | `t.1` |   | `y.2` |
#' | `...` |   | `...` |
#' | `t.n` |   | `y.n` |
#'
#' @return
#'
#' The following folders will be created (if they did not exist before):
#' * `"./ampdriftcorr/"`
#' * `"./ampdriftcorr/logs/"`
#' * `"./ampdriftcorr/pdfs/"`
#'
#' Saves amplifier-drift-corrected data files in CSV-format in new subfolder `"./ampdriftcorr"`,
#' log files containing information on the method used to correct the amplifier drift in
#' `"./ampdriftcorr/logs/"` and PDFs with the graphs before and after correction in `"./ampdriftcorr/pdfs/"`.
#' @examples
#'\dontrun{
#'# Using package example files from GitHub stored within
#'# https://github.com/Peter-T-Ruehr/forceR-data/blob/main/example_data.zip
#'
#'# define folder where files are are stored that need amplifier drift correction
#'data.folder <- "./example_data"
#'cropped.folder <- file.path(data.folder, "cropped")
#'
#'# run apmplifier drift correction on all of files in cropped.folder
#'# the results will be saved in several new folders within the cropped folder.
#'# If print.to.pdf == TRUE, than graphs showing the corrections will be saved as PDFs as well.
#'
#'amp_drift_corr(folder = cropped.folder,
#'                tau = 9400,
#'                print.to.screen = FALSE,
#'                print.to.pdf = TRUE,
#'                res.reduction = 10,
#'                start.file.number = 1)
#'}
#' @export
amp_drift_corr <- function(folder,
                           tau = 9400,
                           print.to.screen = FALSE,
                           print.to.pdf = TRUE,
                           res.reduction = 10,
                           start.file.number = 1){

  if(!is.character(folder)) stop ("folder must contain character string only!")
  if(length(folder) > 1) stop ("folder must only contain one character string!")
  if(!dir.exists(folder)) stop (paste0("folder ", folder, " cannot be found!"))

  if(!is.numeric(tau)) stop ("tau must be numeric!")
  if(length(tau) > 1) stop ("tau must be a single number!")

  if(!is.logical(print.to.screen)) stop ("print.to.screen must be logical!")

  if(!is.logical(print.to.pdf)) stop ("print.to.pdf must be logical!")

  if(!is.numeric(res.reduction)) stop ("res.reduction must be numeric!")
  if(length(res.reduction) > 1) stop ("res.reduction must be a single number!")

  if(!is.numeric(start.file.number)) stop ("start.file.number must be numeric!")
  if(length(start.file.number) > 1) stop ("start.file.number must be a single number!")

  x <- NULL

  if(!str_sub(folder, -1) == '/'){
    folder <- paste0(folder, "/")
  }
  path.target <- paste0(folder, "ampdriftcorr")
  if(!dir.exists(path.target)){
    dir.create(path.target, showWarnings = FALSE)
  } else {
    print(paste0(path.target, " already exists."))
  }
  if(!str_sub(path.target, -1) == '/'){
    path.target <- paste0(path.target, "/")
  }

  path.target.logs <- paste0(path.target, "logs")
  if(!dir.exists(path.target.logs)){
    dir.create(path.target.logs, showWarnings = FALSE)
  } else {
    print(paste0(path.target.logs, " already exists."))
  }
  if(!str_sub(path.target.logs, -1) == '/'){
    path.target.logs <- paste0(path.target.logs, "/")
  }

  path.target.pdfs <- paste0(path.target, "pdfs")
  if(!dir.exists(path.target.pdfs)){
    dir.create(path.target.pdfs, showWarnings = FALSE)
  } else {
    print(paste0(path.target.pdfs, " already exists."))
  }
  if(!str_sub(path.target.pdfs, -1) == '/'){
    path.target.pdfs <- paste0(path.target.pdfs, "/")
  }

  file.list <- list.files(folder, pattern = "csv")
  print(paste0("found ", length(file.list), " files..."))

  for(f in start.file.number:length(file.list)){
    file <- file.list[f]
    curr.measurement <- sub("\\.[[:alnum:]]+$", "", basename(file))

    print(paste0("file ", f, ": ", curr.measurement))

    data <- read_delim(file.path(folder, file.list[f]),
                       delim = ",",
                       show_col_types = FALSE)

    colnames(data) <- c("t", "y")
    time.start <- data$t[1]
    data$t <- data$t-time.start
    data$y.raw <- data$y
    # plot(data$t, data$y, type = "l",
    #      main = paste0(curr.measurement))


    # get time step size
    t.step <- data$t[2] - data$t[1]
    # if(t.step == 0.5){
    #   data$t <- seq(0, nrow(data)*t.step-t.step, t.step)
    # } else {
    #   data$t <- seq(0, nrow(data)*t.step-1, t.step)
    # }

    if(print.to.pdf == TRUE){
      pdf(paste0(path.target.pdfs, curr.measurement, "_ampdriftcorr", ".pdf"),
          width = 20, height = 10)
    }


    # # plot data with rownumber in x, not data$t
    # plot(data$y, type = "l", ylim=c(min(data$y), max(data$y)),
    #      main = curr.measurement)

    # logarithmic drift correction
    # correction constant
    # print(t.step)

    # reference y
    V0 <- 1

    # calculate change for t.step
    corr.const <- V0*exp(-(t.step/tau))

    # get first y value to make it V.0
    V.0 <- data$y[1]

    # check if there are na-values in y (so far only the case in bf_0051)
    na.row <- which(is.na(data$y))
    if(length(na.row) > 0){
      message(paste0("found ", length(na.row), " NA values in time series!"))
      # replace na value with next y value
      data$y[na.row] <- data$y[na.row+1]
    }

    # subtract V.0 from y
    data$y.0cor <- data$y-V.0
    # plot(data$y.0cor, type="l")

    # create empty column for corrected y values
    data$y <- NA
    data$y[1] <- 0

    # predict y under no load
    # indexing vectors instead of data.frames is much faster
    y <- data$y
    y.0cor <- data$y.0cor

    for(i in 2:nrow(data)){ # nrow(data)
      y[i] <- y[i-1] + (y.0cor[i] - (y.0cor[i-1] * corr.const))
      print_progress(i, nrow(data))
    }

    data$y <- y

    # delete columns with correction data
    data$y.0cor <- NULL

    # plot data
    plot(data$t[seq(1,nrow(data),res.reduction/t.step)], data$y.raw[seq(1,nrow(data),res.reduction/t.step)], type = "l",
         ylim=c(min(data$y.raw, data$y, na.rm = TRUE), max(data$y.raw, data$y, na.rm = TRUE)),
         main = paste0(curr.measurement, "; t.step = ", t.step), lwd = 3, col = "grey70",
         xlab = "t", ylab = "y")
    lines(data$t[seq(1,nrow(data),res.reduction/t.step)], data$y[seq(1,nrow(data),res.reduction/t.step)], type="l", lwd = 2, col = "green")
    lines(c(min(data$t), max(data$t)), c(0,0), type="l", col = "red")

    # get linear correction line
    lin.cor.line.x <- c(0,max(data$t))
    lin.cor.line.y <- c(0,data$y[nrow(data)]) # nrow(data)
    # lines(lin.cor.line.x, lin.cor.line.y, type="l", col = "orange")

    baseline.sp <- spline(x=lin.cor.line.x, y=lin.cor.line.y, n = nrow(data)*2.1)
    baseline.sp <- bind_cols(x = baseline.sp$x, y = baseline.sp$y)
    lines(baseline.sp$x, baseline.sp$y, type="l", col = "orange", lwd=1)

    # set round factor: -1 = 10; 0 = same
    if(t.step == 1 | t.step == 2){
      round.factor <- 0
    } else if(t.step == 10){
      round.factor <- -1
    } else if(t.step == 0.5){
      round.factor <- "defined later"
    }

    # filter points that lie within measured area
    if(t.step == 1 | t.step == 2 | t.step == 10){
      baseline.sp.filtered <- baseline.sp %>%
        mutate(x = round(x,round.factor)) %>%
        group_by(x) %>%
        summarize(mean.V = mean(y)) %>%
        filter(x>=0 & x<= max(data$t))
      if(t.step == 2){
        baseline.sp.filtered <- baseline.sp.filtered %>%
          filter(x %% t.step == 0)
      }
    } else if(t.step == 0.5){
      baseline.sp.filtered <- baseline.sp %>%
        mutate(x = ceiling(x*2) / 2) %>%
        group_by(x) %>%
        summarize(mean.V = mean(y)) %>%
        filter(x>=0 & x<= max(data$t))
    }

    # subtract new baseline from rawdata
    data$y <- round(data$y-baseline.sp.filtered$mean.V,6)
    lines(data$t[seq(1,nrow(data),res.reduction/t.step)], data$y[seq(1,nrow(data),res.reduction/t.step)], type="l", col = "darkgreen")

    if(print.to.pdf == TRUE){
      dev.off()
    }

    # plot data
    if(print.to.screen == TRUE){
      plot(data$t[seq(1,nrow(data),res.reduction/t.step)], data$y.raw[seq(1,nrow(data),res.reduction/t.step)], type = "l",
           ylim=c(min(data$y.raw, data$y, na.rm = TRUE), max(data$y.raw, data$y, na.rm = TRUE)),
           main = paste0(curr.measurement, "; t.step = ", t.step), lwd = 3, col = "grey70",
           xlab = "t", ylab = "y")
      lines(data$t[seq(1,nrow(data),res.reduction/t.step)], data$y[seq(1,nrow(data),res.reduction/t.step)], type="l", lwd = 2, col = "green")
      lines(c(min(data$t), max(data$t)), c(0,0), type="l", col = "red")
      lines(baseline.sp$x, baseline.sp$y, type="l", col = "orange", lwd=1)
      lines(data$t[seq(1,nrow(data),res.reduction/t.step)], data$y[seq(1,nrow(data),res.reduction/t.step)], type="l", col = "darkgreen")

    }

    data <- data %>%
      select(t, y)

    write_csv(data, paste0(path.target, curr.measurement, "_ampdriftcorr.csv"))

    # save little log file with cut window infos
    write_csv(data.frame(correction.factor = "exp(-(t/tau))",
                         tau = tau,
                         t = t.step,
                         script.version = "0.0.4"),
              paste0(path.target.logs, curr.measurement, "_ampdriftcorr_log.csv"))
  }
}




#' Automatic or Manual Baseline Correction of Time Series
#'
#' If baseline (zero-line) of measurement is unstable (e.g. due to temperature fluctuations, wind, ...),
#' the baseline needs to be continually adjusted throughout the measurement. This script allows an
#' automatic adjustment of the baseline.
#' The automatic approach invokes a sliding window, during which the 'minimum' within each sliding window
#' is stored. A 'minimum' is defined by the `quantile.size`: if set to `0.05`, the value below which only 5% of
#' the measurement data within the sliding window lies, is treated as the current window's minimum. This
#' prevents the treatment of potential artefacts as minima. In a second iteration, another sliding window
#' calculates the average of these 'minima'. The resulting values are subtracted from
#' the original time series. This approach works well for time series with relatively short peaks.
#' If the automatic approach does not yield acceptable results, an interactive manual approach to correct
#' the baseline can be performed instead.
#'
#' @param file A character string containing the full path to the measurement file that needs correction.
#' See Details for info on what the file should look like.
#' @param corr.type Character string defining the desired mode of baseline correction. One of `"auto"`
#'   or `"manual"`. Default: `"auto"`
#' @param print.to.screen A logical value indicating if results should be plotted in the current R plot
#'   device. Default: `TRUE`.
#' @param print.to.pdf  A logical value indicating  if results should be saved as PDFs. This is considered
#'   necessary to quality check and comunicate the results. Default: `TRUE`.
#' @param window.size.mins A numeric value for the size of the search window to find minima in. Should be
#'   in the same time unit as the measurement. Longer peaks require higher values, shorter peaks require
#'   smaller values. Default: `1000`.
#' @param window.size.means  A numeric value for the size of the window to average the minima in. Should be
#'   in the same time unit as the measurement. By default (`NULL`), the same value as specified for
#'   `window.size.mins` is used.
#' @param quantile.size A numerical value between `0` and `1` to define the quantile which is treated as
#'   the 'minimum' of a sliding window. Default: `0.05`.
#' @param y.scale A numeric value to reduce the y-axis range during plotting. This simplifies the manual
#'   placement of the points during the manual correction procedure.
#' @param res.reduction A numeric value to reduce the number of time steps by during plotting. Speeds up
#'   the plotting process and reduces PDF size. Has no effect on the results, only on the plots. Default: 10.
#' @param Hz A numeric value to reduce sampling frequency for temporary analyses. This works as a smoothing
#'   filter during temporary analyses and does not reduce the actual sampling frequency of the data.
#'   Default: `100`.
#'
#' @details
#' The input files should to be in the following format:
#'
#' | `t` |   | `y` |
#' | :----: | :----: |:----: |
#' | `t.1` |   | `y.2` |
#' | `...` |   | `...` |
#' | `t.n` |   | `y.n` |
#'
#' In case there are more than two columns, only the first two columns will be used. If the first two columns
#' are not named 't' and 'y', they will be renamed.
#'
#' @return
#'
#' The following folders will be created (if they did not exist before):
#' * `"./ampdriftcorr/"`
#' * `"./ampdriftcorr/logs/"`
#' * `"./ampdriftcorr/pdfs/"`
#'
#' Saves amplifier-drift-corrected data files in CSV-format in new subfolder `"./ampdriftcorr"`,
#' log files containing information on the method used to correct the amplifier drift in
#' `"./ampdriftcorr/logs/"` and PDFs with the graphs before and after correction in `"./ampdriftcorr/pdfs/"`.
#' ampdriftcorr.folder <- file.path(data.folder, "cropped/ampdriftcorr")
#' @examples
#'\dontrun{
#'# Using a package example file from GitHub stored within
#'# https://github.com/Peter-T-Ruehr/forceR-data/blob/main/example_data.zip
#'
#'#'########### AUTOMATIC MODE
#'
#'# define folder and file to apply the baseline drift correction to
#'data.folder <- "./example_data"
#'ampdriftcorr.folder <- file.path(data.folder, "cropped/ampdriftcorr")
#'file = file.path(ampdriftcorr.folder, "1068_cropped_ampdriftcorr.csv")
#'
#'# plot file
#'plot_measurement(file)
#'
#'# run the automatic baseline drift correction
#'# the results will be saved in new subfolders of the ampdriftcorr.folder
#'baseline_corr(file,
#'              corr.type = "auto",
#'              print.to.screen = TRUE,
#'              print.to.pdf = TRUE,
#'              window.size.mins = 2000,
#'              window.size.means = NULL,
#'              quantile.size = 0.05,
#'              y.scale = 0.5,
#'              res.reduction = 10,
#'              Hz = 100)
#'
#'[1] "1068_cropped_ampdriftcorr"
#'[1] "You selected automatic drift correction:"
#'[1] "sliding minima window size: 2000"
#'[1] "sliding means of minima window size: 2000"
#'[1] "calculating with 100 Hz."
#'[1] "Finding 5th percentile sliding minima..."
#'100%...[1] "Finding sliding means..."
#'
#'
#'
#'#'########### MANUAL MODE
#'
#'#'# define folder and file to apply the baseline drift correction to
#'data.folder <- "./example_data"
#'ampdriftcorr.folder <- file.path(data.folder, "cropped/ampdriftcorr")
#'file = file.path(ampdriftcorr.folder, "1174_cropped_ampdriftcorr")
#'
#'# plot file
#'plot_measurement(file)
#'
#'# run the manual baseline drift correction.The user is expected to
#'# interactively mark the baseline with dots in the R plot device.
#'# the results will be saved in new subfolders of the ampdriftcorr.folder
#'
#'baseline_corr(file,
#'              corr.type = "manual",
#'              print.to.screen = TRUE,
#'              print.to.pdf = TRUE)
#'              plot_measurement(file)
#'
#'[1] "1174_cropped_ampdriftcorr"
#'[1] "You selected manual drift correction."
#'[1] "Please select baseline points and click \"Finish\"."
#'[1] "31 line points taken."
#'[1] "Done!"
#'}
#' @export
baseline_corr <- function(file,
                          corr.type = "auto",
                          print.to.screen = TRUE,
                          print.to.pdf = TRUE,
                          window.size.mins = 1000,
                          window.size.means = NULL,
                          quantile.size = 0.05,
                          y.scale = 0.5,
                          res.reduction = 10,
                          Hz = 100){

  if(!is.character(file)) stop ("file must contain character string only!")
  if(length(file) > 1) stop ("file must only contain one character string!")
  if(!dir.exists(file)) stop (paste0("file ", file, " cannot be found!"))

  if(!is.character(corr.type)) stop ("corr.type must be a character string!")
  if(length(corr.type) != 1) stop ("corr.type must only contain one character string!")
  if(!(corr.type == "auto") & !(corr.type == "man")) stop ("corr.type can only be 'auto' or 'man'!")

  if(!is.logical(print.to.screen)) stop ("print.to.screen must be logical!")

  if(!is.logical(print.to.pdf)) stop ("print.to.pdf must be logical!")

  if(!is.numeric(window.size.mins)) stop ("window.size.mins must be numeric!")
  if(length(window.size.mins) > 1) stop ("window.size.mins must be a single number!")

  if(!is.numeric(window.size.means) & !is.null(window.size.means)) stop ("window.size.means must be numeric or NULL!")
  if(length(window.size.means) != 1 & !is.null(window.size.means)) stop ("window.size.means must be a single number!")

  if(!is.numeric(quantile.size)) stop ("quantile.size must be numeric!")
  if(length(quantile.size) > 1) stop ("quantile.size must be a single number!")

  if(!is.numeric(y.scale)) stop ("y.scale must be numeric!")
  if(length(y.scale) > 1) stop ("y.scale must be a single number!")

  if(!is.numeric(res.reduction)) stop ("res.reduction must be numeric!")
  if(length(res.reduction) > 1) stop ("res.reduction must be a single number!")

  if(!is.numeric(Hz)) stop ("Hz must be numeric!")
  if(length(Hz) > 1) stop ("Hz must be a single number!")

  x <- y <- y.zerocor <- t.10 <- V.10 <- NULL

  if(is.null(window.size.means)) window.size.means <- window.size.mins

  folder <- dirname(file)
  if(!str_sub(folder, -1) == '/'){
    folder <- paste0(folder, "/")
  }
  path.target = paste0(folder, "baselinecorr")
  if(!dir.exists(path.target)){
    dir.create(path.target, showWarnings = FALSE)
  } else {
    print(paste0(path.target, " already exists."))
  }
  if(!str_sub(path.target, -1) == '/'){
    path.target <- paste0(path.target, "/")
  }

  path.target.logs = paste0(path.target, "logs")
  if(!dir.exists(path.target.logs)){
    dir.create(path.target.logs, showWarnings = FALSE)
  } else {
    print(paste0(path.target.logs, " already exists."))
  }
  if(!str_sub(path.target.logs, -1) == '/'){
    path.target.logs <- paste0(path.target.logs, "/")
  }

  path.target.pdfs = paste0(path.target, "pdfs")
  if(!dir.exists(path.target.pdfs)){
    dir.create(path.target.pdfs, showWarnings = FALSE)
  } else {
    print(paste0(path.target.pdfs, " already exists."))
  }
  if(!str_sub(path.target.pdfs, -1) == '/'){
    path.target.pdfs <- paste0(path.target.pdfs, "/")
  }

  data <- read_delim(file,
                     delim = ",",
                     show_col_types = FALSE)

  curr.measurement <- sub("\\.[[:alnum:]]+$", "", basename(file))
  print(curr.measurement)

  if(ncol(data) <= 1) stop("Data contains less than two columns. Process terminated")

  if(ncol(data) > 2){
    warning("Data contains more than two columns. Only first two columns will be used.")
    data <- data[,1:2]
  }

  if(sum(colnames(data) == c("t", "y")) != 2){
    warning("Column names of data were not 't' and 'y'. Renaming colums.")
    colnames(data) <- c("t", "y")
  }


  t.step <- data$t[2] - data$t[1]
  plot(data$t[seq(1,nrow(data),res.reduction/t.step)],
       data$y[seq(1,nrow(data),res.reduction/t.step)],
       type = "l", lwd = 2, col = "darkgreen",
       main = paste0(curr.measurement))
  lines(c(data$t[1], data$t[nrow(data)]), rep(0, 2), type="l", col = "red", lwd = 1)


  if(corr.type == "manual"){
    print("You selected manual drift correction.")

    t.step <- data$t[2] - data$t[1]

    plot(data$t[seq(1,nrow(data),res.reduction/t.step)], data$y[seq(1,nrow(data),res.reduction/t.step)], type = "l", lwd = 2, col = "grey90",
         main = paste0(curr.measurement), ylim = c(1.1*min(data$y), y.scale*max(data$y)))
    lines(data$t[seq(1,nrow(data),res.reduction/t.step)], data$y[seq(1,nrow(data),res.reduction/t.step)], type = "l", col = "darkgreen",
          main = paste0(curr.measurement))
    lines(c(data$t[1], data$t[nrow(data)]), rep(0, 2), type="l", col = "red", lwd = 1)

    print("Please select baseline points and click \"Finish\".")
    baseline <- locator(type = "n")
    print(paste0(length(baseline$x), " line points taken."))

    lines(baseline, type="l", col = "orange")
    means.spline <- spline(x=baseline$x, y=baseline$y, n = nrow(data)*2.1)
    means.spline <- bind_cols(x = means.spline$x, y = means.spline$y)
    lines(means.spline$x, means.spline$y, type="l", col = "green", lwd=2)

    # set round factor: -1 = 10; 0 = same
    if(t.step == 1){
      round.factor <- 0
    } else if(t.step == 10){
      round.factor <- -1
    } else if(t.step == 0.5){
      round.factor <- "defined later"
    } else if(t.step == 2){
      round.factor <- "defined later"
    }

    # filter points that lie within measured area
    if(t.step == 1 | t.step == 10){
      means.spline.filtered <- means.spline %>%
        mutate(x = round(x,round.factor)) %>%
        group_by(x) %>%
        summarize(mean.V = mean(y)) %>%
        filter(x>=0 & x<= max(data$t))
    } else if(t.step == 0.5){
      means.spline.filtered <- means.spline %>%
        mutate(x = ceiling(x*2) / 2) %>%
        group_by(x) %>%
        summarize(mean.V = mean(y)) %>%
        filter(x>=0 & x<= max(data$t))
    } else if(t.step == 2){
      means.spline.filtered <- means.spline %>%
        mutate(x = 2 * ceiling(x / 2)) %>%
        group_by(x) %>%
        summarize(mean.V = mean(y)) %>%
        filter(x>=0 & x<= max(data$t))
    }

    # add values at start and end if line is shorter than data time frame
    line.t.start <- means.spline.filtered$x[1]
    line.t.end <- means.spline.filtered$x[nrow(means.spline.filtered)]
    points(c(line.t.start, line.t.end), c(0,0), col = "red", pch = 16)
    if(line.t.start > 0){
      line.start.tibble <- as_tibble(setNames(data.frame(matrix(nrow = line.t.start/t.step, ncol = length(c("x", "mean.V")))), c("x", "mean.V")))
      line.start.tibble$x <- seq(0, line.t.start-t.step, t.step)
      line.start.tibble$mean.V <- means.spline.filtered$mean.V[1]
      means.spline.filtered <- rbind(line.start.tibble,means.spline.filtered)
    }
    if(line.t.end < data$t[nrow(data)]){
      line.end.tibble <- as_tibble(setNames(data.frame(matrix(nrow = (data$t[nrow(data)]-line.t.end)/t.step, ncol = length(c("x", "mean.V")))), c("x", "mean.V")))
      line.end.tibble$x <- seq(line.t.end+t.step, data$t[nrow(data)], t.step)
      line.end.tibble$mean.V <- means.spline.filtered$mean.V[nrow(data)]
      means.spline.filtered <- rbind(means.spline.filtered, line.end.tibble)
    }
    lines(means.spline.filtered$x, means.spline.filtered$mean.V, type="l", col = "darkgreen", lwd=2)

    # subtract new baseline from cor.data
    data$y.zerocor <- data$y-means.spline.filtered$mean.V

    # round y values
    data$y <- round(data$y, 6)
    data$y.zerocor <- round(data$y.zerocor, 6)

    # start PDF device
    if(print.to.pdf == TRUE){
      pdf(paste0(path.target.pdfs, curr.measurement, "_baselinecorr", ".pdf"),
          width = 20, height = 10)
    }

    # plot data
    plot(data$t[seq(1,nrow(data),res.reduction/t.step)], data$y[seq(1,nrow(data),res.reduction/t.step)], type = "l",
         main = paste0(curr.measurement), lwd = 4, col = "grey80",
         xlim = c(0, max(data$t)),
         ylim = c(min(data$y, data$y, data$y.zerocor, na.rm = TRUE),
                  max(data$y, data$y, data$y.zerocor, na.rm = TRUE)))
    lines(data$t[seq(1,nrow(data),res.reduction/t.step)], data$y[seq(1,nrow(data),res.reduction/t.step)], type="l", col = "grey60", lwd = 4)
    lines(means.spline.filtered$x, means.spline.filtered$mean.V, type="l", col = "green", lwd = 1)
    lines(data$t[seq(1,nrow(data),res.reduction/t.step)], data$y.zerocor[seq(1,nrow(data),res.reduction/t.step)], type="l", col = "darkgreen", lwd = 1)
    lines(c(data$t[1], data$t[nrow(data)]), rep(0, 2), type="l", col = "red", lwd = 1)

    if(print.to.pdf == TRUE){
      dev.off()
    }

    if(print.to.screen == TRUE){
      # plot data with correction lines
      plot(data$t[seq(1,nrow(data),res.reduction/t.step)], data$y[seq(1,nrow(data),res.reduction/t.step)], type = "l",
           main = paste0(curr.measurement), lwd = 4, col = "grey80",
           xlim = c(0, max(data$t)),
           ylim = c(min(data$y, data$y, data$y.zerocor, na.rm = TRUE),
                    max(data$y, data$y, data$y.zerocor, na.rm = TRUE)))
      lines(data$t[seq(1,nrow(data),res.reduction/t.step)], data$y[seq(1,nrow(data),res.reduction/t.step)], type="l", col = "grey60", lwd = 4)
      lines(means.spline.filtered$x, means.spline.filtered$mean.V, type="l", col = "green", lwd = 1)
      lines(data$t[seq(1,nrow(data),res.reduction/t.step)], data$y.zerocor[seq(1,nrow(data),res.reduction/t.step)], type="l", col = "darkgreen", lwd = 1)
      lines(c(data$t[1], data$t[nrow(data)]), rep(0, 2), type="l", col = "red", lwd = 1)
    }

    data <- data %>%
      select(t, y.zerocor) %>%
      rename(y = y.zerocor)

    write_csv(data, paste0(path.target, curr.measurement, "_baselinecorr.csv"))

    # save little log file with manual base spline line
    # write_csv(data.frame(baseline), paste0(path.target, "/logs/", "prepared_bf_", curr.measurement, "_man_log.csv"))
    write_csv(data.frame(baseline), paste0(path.target.logs, curr.measurement, "_baselinecorr_log.csv"))
    print("Done!")

  }
  if(corr.type == "auto") {
    # graphics.off()
    print("You selected automatic drift correction:")
    print(paste0("sliding minima window size: ", window.size.mins))
    print(paste0("sliding means of minima window size: ", window.size.means))

    t.step <- data$t[2] - data$t[1]

    max.t.msec <- max(data$t, na.rm=T)

    # add lines in front and at end of data time window for better moving averages
    lines.to.add <- window.size.mins/t.step*15/t.step

    line.start.tibble <- as_tibble(setNames(data.frame(matrix(nrow = lines.to.add, ncol = length(colnames(data)))), colnames(data)))

    line.start.tibble$y <- data$y[1]
    line.start.tibble$y <- data$y[1]

    line.end.tibble <- as_tibble(setNames(data.frame(matrix(nrow = lines.to.add, ncol = length(colnames(data)))), colnames(data)))
    line.end.tibble$y <- data$y[nrow(data)]
    line.end.tibble$y <- data$y[nrow(data)]

    data.concat <- rbind(line.start.tibble, data, line.end.tibble)

    data.concat$t <- seq(0, nrow(data.concat)*t.step-t.step, t.step)

    # create data frame with data reduced to x Hz to even out spikes and unsteady sensor signal
    t.step.10 = 1000/Hz
    print(paste0("calculating with ", Hz, " Hz."))

    data.concat.10 <- data.concat %>%
      group_by(t.10 = t.step.10 * round(t/t.step.10)) %>%
      summarize(V.10 = mean(y)) %>%
      rename(t = t.10, y = V.10)

    print(paste0("Finding ", quantile.size*100, "th percentile sliding minima..."))

    times.msec <- c()
    minima <- c()
    for(k in (window.size.mins/2/t.step.10):(nrow(data.concat.10)-window.size.mins/t.step.10/2)){
      times.msec[length(times.msec)+1] <- k*t.step.10-window.size.mins*t.step.10/2/t.step.10
      # get window
      curr.window.data <- data.concat.10$y[(k-window.size.mins/2/t.step.10):
                                             (k+window.size.mins/2/t.step.10)]
      # calculate slope within window
      curr.slope <- curr.window.data[length(curr.window.data)] - curr.window.data[1]
      # if positive or constant slope:
      if(curr.slope >= 0){
        # get y value below only (1-quantile.size)*100 % of the data lie.
        # This prevents getting singular value or very short minima
        minima[length(minima)+1] <- quantile(curr.window.data, quantile.size)
        # if negative slope:
      } else {
        # check if k becomes smaller than 1 if halve window.size is subtracted and set k to 1 if that's the case
        if(k-window.size.mins/2 < 1){k=1}
        # get window new window with different position relative to k
        curr.window.data <- data.concat.10$y[(k-window.size.mins/t.step.10):
                                               (k-window.size.mins/t.step.10/2)]
        # get y value below only (1-quantile.size)*100 % of the data lie.
        # This prevents getting singular value or very short minima
        minima[length(minima)+1] <- quantile(curr.window.data, quantile.size)
      }
      print_progress(k, nrow(data.concat.10)-window.size.mins/t.step.10/2)
    }


    # replace NA values with previous minumum
    for(m in 1:length(minima)){
      if(is.na(minima[m])){
        minima[m] <- minima[m-1]
      }
    }

    # replace minima at beginning outside the data time window with first minimum found
    minima[1:(window.size.mins/2/t.step.10+1)] <- 0

    # plot(data.concat.10$t, data.concat.10$y, type = "l", col = "darkgreen",
    #       main = paste0(curr.measurement))
    # lines(c(data.concat.10$t[1], data.concat.10$t[nrow(data.concat.10)]), rep(0, 2), type="l", col = "red", lwd = 1)
    # lines(times.msec, minima, col = "cyan")

    print("Finding sliding means...")

    means <- roll_mean(
      x = minima,
      width = window.size.means/t.step.10,    complete_obs = FALSE,
      na_restore = FALSE,
      online = FALSE
    )
    means <- means[(window.size.mins/2/t.step):length(means)]

    times.msec.means <- roll_mean(
      x = times.msec,
      width = window.size.means/t.step.10,
      complete_obs = FALSE,
      na_restore = FALSE,
      online = FALSE
    )
    times.msec.means <- times.msec.means[(window.size.mins/2/t.step):length(times.msec.means)]

    length.diff <- t.step.10 * (length(minima) - length(means))
    means <- c(rep(means[1], ceiling(length.diff/2/t.step.10)),
               means,
               rep(means[length(means)], floor(length.diff/2/t.step.10)))

    times.msec.means <- c(seq(0, floor(length.diff/2-t.step.10), t.step.10),
                          times.msec.means,
                          seq(max(times.msec.means, na.rm = TRUE)+t.step, floor(max(times.msec.means+t.step, na.rm = TRUE)+floor(length.diff/2)), t.step.10))


    # plot(data.concat.10$t, data.concat.10$y, type = "l", col = "darkgreen",
    #       main = paste0(curr.measurement))
    # lines(times.msec, minima, type = "l", col = "blue")
    # lines(times.msec.means, means, type = "l", col = "green")

    t.start <- lines.to.add*t.step

    # get means into correct time frame
    means.tibble <- as_tibble(cbind(times.msec.means, means)) %>%
      rename(t = times.msec.means) %>%
      filter(t >= t.start & t <= max.t.msec+t.start) %>%
      mutate(t = t-t.start)

    means.spline <- spline(x=means.tibble$t, y=means.tibble$means, n = nrow(data))
    means.spline <- bind_cols(t = means.spline$x, means = means.spline$y)

    data$y.zerocor <- data$y-means.spline$means

    # plot(data$t[seq(1,nrow(data),res.reduction/t.step)], data$y[seq(1,nrow(data),res.reduction/t.step)],
    #      type = "l", col = "grey80", lwd = 3,
    #      main = paste0(curr.measurement))
    # lines(means.tibble$t, means.tibble$means, col = "blue")
    # lines(means.spline$t, means.spline$means, type="l", col = "red", lwd=2)
    # lines(data$t, data$y.zerocor, col = "green")

    # round y values
    data$y <- round(data$y, 6)
    data$y <- round(data$y, 6)
    data$y.zerocor <- round(data$y.zerocor, 6)

    # start PDF device
    print("Writing to PDF...")
    if(print.to.pdf == TRUE){
      pdf(paste0(path.target.pdfs, curr.measurement, "_baselinecorr", ".pdf"),
          width = 20, height = 10)
    }
    plot(data$t[seq(1,nrow(data),res.reduction/t.step)], data$y[seq(1,nrow(data),res.reduction/t.step)],
         main = paste0(curr.measurement), lwd = 4, , type="l", col = "grey60",
         xlim = c(0, max(data$t)),
         ylim = c(min(data$y, data$y, data$y.zerocor),
                  max(data$y, data$y, data$y.zerocor))) # y_raw
    lines(x=c(data$t[1], data$t[nrow(data)]), y=c(0,0), lty = 3, col = "red") # zero liner
    lines(times.msec-nrow(line.start.tibble)*t.step, minima, type="l", col = "blue", lwd = 1) # minima
    lines(means.spline$t , means.spline$means, type="l", col = "green", lwd = 2) # means
    lines(data$t[seq(1,nrow(data),res.reduction/t.step)], data$y.zerocor[seq(1,nrow(data),res.reduction/t.step)], type = "l", lwd = 1,
          main = paste0(curr.measurement), col = "darkgreen") # y_cor

    if(print.to.pdf == TRUE){
      dev.off()
    }

    if(print.to.screen == TRUE){
      print("Printing to screen...")
      # plot data without correction lines
      plot(data$t[seq(1,nrow(data),res.reduction/t.step)], data$y.zerocor[seq(1,nrow(data),res.reduction/t.step)], type = "l",
           main = paste0(curr.measurement), lwd = 1, col = "black",
           xlim = c(0, max(data$t)),
           ylim = c(min(data$y, data$y, data$y.zerocor),
                    max(data$y, data$y, data$y.zerocor))) # y_raw
      lines(x=c(data$t[1], data$t[nrow(data)]), y=c(0,0), lty = 3, col = "red") # zero line


      # plot data with correction lines
      plot(data$t[seq(1,nrow(data),res.reduction/t.step)], data$y[seq(1,nrow(data),res.reduction/t.step)],
           main = paste0(curr.measurement), lwd = 4, , type="l", col = "grey60",
           xlim = c(0, max(data$t)),
           ylim = c(min(data$y, data$y, data$y.zerocor),
                    max(data$y, data$y, data$y.zerocor))) # y_raw
      lines(x=c(data$t[1], data$t[nrow(data)]), y=c(0,0), lty = 3, col = "red") # zero liner
      lines(times.msec-nrow(line.start.tibble)*t.step, minima, type="l", col = "blue", lwd = 1) # minima
      lines(means.spline$t , means.spline$means, type="l", col = "green", lwd = 2) # means
      lines(data$t[seq(1,nrow(data),res.reduction/t.step)], data$y.zerocor[seq(1,nrow(data),res.reduction/t.step)], type = "l", lwd = 1,
            main = paste0(curr.measurement), col = "darkgreen") # y_cor
    }


    data <- data %>%
      select(t, y.zerocor) %>%
      rename(y = y.zerocor)

    write_csv(data, paste0(path.target, curr.measurement, "_baselinecorr.csv"))

    # save little log file with window size infos
    write_csv(data.frame(window.size.minima.msec = window.size.mins,
                         window.size.means.msec = window.size.means,
                         script.version = "0.0.4"),
              paste0(path.target.logs, curr.measurement, "_baselinecorr_log.csv")) # _auto_log.csv

    print("Done!")
  }
}
