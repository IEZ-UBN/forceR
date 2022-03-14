#' Plot raw measurement
#'
#' Plots a time series.
#' @details
#' #' The input files need to be in the following format (even though column names do not matter):
#'
#' | **`t`** |   | **`y`** |
#' | :----: | :----: |:----: |
#' | `t.1` |   | `y.1` |
#' | `...` |   | `...` |
#' | `t.n` |   | `y.n` |
#'
#' @param file File path to measurement.
#' @param columns A vector of column numbers. The first entry will be used as the x-axis values, the second entry as y-axis values.
#' All other columns will be ignored. Default: `c(1,2)`.
#' @return Creates a plot in the current plot device.
#' @examples
#'\dontrun{
#'# Using a package example file from GitHub stored within
#'# https://github.com/Peter-T-Ruehr/forceR-data/blob/main/example_data.zip
#'
#'data.folder <- "./example_data"
#'file = file.path(data.folder, "0982.csv")
#'plot_measurement(file)
#'}
#' @export
plot_measurement <- function (file,
                              columns = c(1:2)){
  # tested for v1.19
  if(file.exists(file)){
    file.name <- basename(file)
    data = read_csv(file, show_col_types = FALSE)

    # check if more than 2 columns were defined for plotting
    if(length(columns)>2) stop('More than two columns were defined by user.')
    # check if data as at least as many columns as max. number in columns variable
    if(ncol(data) < max(columns)) stop('Data has fewer columns than defined by user.')


    sampling_rate_Hz <- 1/round(data[2,1] - data[1,1], digits = 4)*1000

    plot(data[,columns], type = 'l',
         main = paste0(file.name, " (", sampling_rate_Hz, " Hz)"), xlab = "time [m.sec]", ylab = "y")

  } else {
    stop("File does not exist!")
  }
}



#' Plot Peaks
#'
#' Plots the peaks identified by the function `find_peaks()`.
#'
#' @param df.peaks df.peaks The resulting tibble of the function `find_peaks()`. See `?find_peaks` for more details.
#' @param df.data A data frame or tibble in the below format. The columns `t` (time), `force` and `measurement`
#'   (measurement ID) must be present. This will usually be the same table that was used before in `find_peaks()`.
#' @param additional.msecs A numeric value indicating how many m.secs before and after the actual peak curve should be plotted. Default: `2000`
#' @param path.plots A string character defining where to save the plots.
#' @param print.to.pdf A logical value indicating if the peak curves should be saved as PDFs. Default: `TRUE`
#'   Default: `TRUE`
#'
#' @details
#' #' The input data frame or tibble `df.data` needs to contain the following columns:
#'
#' | **`t`** | **`force`** |  **`measurement`** |
#' | :----: | :----: |:----: |
#' | `t.1` |  `force.1` | `measurement.1` |
#' | `...` |  `...` |  `...` |
#' | `t.n` |  `force.n` | `measurement.m` |
#'
#' @return
#'
#' In addition, if `print.to.pdf == TURE`,
#' Plots one graph per peak curve and, if `print.to.pdf == TURE`, saves all peak curves as one PDF at `path.plots`.
#'
#' @examples
#' require(dplyr)
#'
#' # PREPARATION ####
#' # create a classifier to store specimen info (see package vignette for details)
#' classifier <- tibble(species = c("A","A","A","A","B","B","B","B"),
#'                      specimen = c("a","a","b","b","c","c","d","d"),
#'                      measurement = paste0("m_0", 1:8),
#'                      amp = c(rep(2,4), rep(0.5, 4)),
#'                      lever.ratio = rep(0.5, 8))
#'
#' # create temporary tibble to store data for bite series simulation
#' classifier.temp <- classifier %>%
#'   mutate(type = c(rep("sin", 4), rep("plat", 4)),
#'          max.y = c(1.9, 2.4, 2.2, 2.0, 6.8, 7.2, 7.5, 7.2),
#'          length.of.bite = c(20, 20, 18, 22, 50, 40, 45, 40),
#'          length.of.series = c(rep(200, 4), rep(600, 4)),
#'          jit = c(rep(0.5, 4), rep(1, 4)))
#'
#' # create tibble with simulated time series with different
#' # bite charactersitics for each measurement, specimen and species
#' df.all <- NULL
#' for(i in 1:nrow(classifier.temp)){
#'   df.curr <- simulate_bites(no.of.bites = 7,
#'                             length.of.bite = classifier.temp$length.of.bite[i],
#'                             length.of.series = classifier.temp$length.of.series[i],
#'                             max.y = classifier.temp$max.y[i],
#'                             max.y.jit = 15,
#'                             jit = classifier.temp$jit[i],
#'                             bite.type = classifier.temp$type[i],
#'                             plot = FALSE)
#'
#'   # add measurement number to df.curr
#'   df.curr <- df.curr %>%
#'     mutate(measurement = classifier.temp$measurement[i])
#'
#'   # add current sumulated bite series to df.all
#'   df.all <- rbind(df.all, df.curr)
#' }
#' # remove temporary tibble to avoid confusion
#' rm(classifier.temp)
#'
#' # rename columns
#' df.all <- df.all %>%
#'   rename(force = y)
#'
#' # add classifier info to bite table (df.all)
#' df.all <- left_join(df.all,
#'                     classifier,
#'                     by = "measurement")
#'
#' peaks.df <- find_strongest_peaks(df = df.all,
#'                                  no.of.peaks = 5,
#'                                  print.to.pdf = FALSE)
#'
#' # RUN THE FUNCTION ####
#' plot_peaks(df.peaks = peaks.df,
#'            df.data = df.all,
#'            additional.msecs = 20,
#'            print.to.pdf = FALSE)
#' @export
plot_peaks <- function(df.peaks,
                       df.data,
                       additional.msecs = 2000,
                       path.plots,
                       print.to.pdf = TRUE){

  measurement <- NULL

  if(print.to.pdf == TRUE){
    print(paste0("plotting to ", path.plots, today(),"_all_peak_curves.pdf..."))
    pdf(paste0(path.plots, today(),"_all_peak_curves.pdf"), onefile = TRUE, paper = "a4", height = 14)
  }
  par(mfrow=c(3,2))
  for(b in 1:nrow(df.peaks)){ # nrow(df.peaks)

    curr.peak.starts <- str_split(df.peaks$starts[b], pattern = "; ")[[1]]
    curr.peak.ends <- str_split(df.peaks$ends[b], pattern = "; ")[[1]]
    measurements <- str_split(df.peaks$measurements[b], pattern = "; ")[[1]]

    # plot each peak
    for(c in 1:length(curr.peak.starts)){
      curr.measurement <- measurements[c]
      curr.peak.start <- as.numeric(curr.peak.starts[c])
      curr.peak.end <- as.numeric(curr.peak.ends[c])

      # add msces before and after
      curr.peak.window.with.frame <- df.data %>%
        filter(measurement == curr.measurement) %>%
        filter(t >= (curr.peak.start-additional.msecs) & t <= (curr.peak.end+additional.msecs)) %>%
        select(t, force)

      # plot(curr.peak.window.with.frame, type="l")

      curr.peak.window <- curr.peak.window.with.frame %>%
        filter(t >= (curr.peak.start) & t <= (curr.peak.end))

      # plot(curr.peak.window, type="l")

      if(nrow(curr.peak.window) >= 1){
        plot(curr.peak.window.with.frame, type="l", col="grey80", lwd=.5, ylim = c(0,max(curr.peak.window$force)))
        lines(curr.peak.window, type="l", main = paste0(curr.measurement), col="black", lwd=1)
        title(main = paste0(curr.measurement, ", peak: ", c), cex.main = 0.95)
      }
    }
    print_progress(b, nrow(df.peaks))
  }
  if(print.to.pdf == TRUE){
    dev.off()
  }
  par(mfrow=c(1,1))
  print("Done!")
}
