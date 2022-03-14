#' Reduce Sampling Frequency
#'
#' Reduces the sampling frequency to a certain Hz value. If the desired frequency is smaller than the
#'   original frequency, the data remains unchanged.
#' @details
#' The input data frame or tibble should have the following format:
#'
#' | **`t`** | **`y`** |
#' | :----: | :----: |
#' | `t.1` |  `y.1` |
#' | `...` | `...` |
#' | `t.n` |  `y.n` |
#'
#' or, if `measurement.col` is not `NULL`, then
#'
#' | **`t`** | **`y`** | **`measurement.col`**|
#' | :----: | :----: | :----: |
#' | `t.1` |  `y.1` |  `...`  |
#' | `...` | `...` | `...`  |
#' | `t.n` |  `y.n` |  `...`  |
#'
#' Since, when not `NULL`, the `measurement.col` is called by its character string, the position of the column does not matter, except it
#'   must not be among the first two columns which are reserved for `t` and `y`.
#'
#' All columns except the first two are removed. Values in `t` are expected to be in m.secs.
#'
#' @param df Data frame or tibble in the below mentioned format.
#' @param Hz Numeric value of desired frequency. Fedault `200`
#' @param measurement.col Character string. If `measurement.col` is not defined, the whole input data frames will be
#'   treated as if it was just one single time series. This is okay for data frames like that indeed only conatin one
#'   time series, but for data frames
#'   with multiple time series, a grouping column needs to be defined. Default: `NULL`
#' @return Returns a tibble reduced to the desired frequency in the following format:
#'
#' | **`t`** | **`y`** |
#' | :----: |:----: |
#' | `t.1` | `y.1` |
#' | `...` | `...` |
#' | `t.n` | `y.n` |
#'
#' or, if `measurement.col` is not `NULL`, then
#'
#' | **`t`** | **`y`** | **`measurement.col`**|
#' | :----: | :----: | :----: |
#' | `t.1` |  `y.1` |  `...`  |
#' | `...` | `...` | `...`  |
#' | `t.n` |  `y.n` |  `...`  |
#'
#' @examples
#' require(dplyr)
#'
#' # simulate a short time series with sinusoidal bites
#' df.1 <- simulate_bites(no.of.bites = 7,
#'                        length.of.bite = 100,
#'                        length.of.series = 1000,
#'                        max.y = 3,
#'                        max.y.jit = 15,
#'                        jit = 0.5,
#'                        bite.type = "sin",
#'                        plot = TRUE)
#' df.1$measurement <- "m_01"
#'
#' # simulate a short time series with plateu-like bites
#' df.2 <- simulate_bites(no.of.bites = 7,
#'                        length.of.bite = 150,
#'                        length.of.series = 1500,
#'                        max.y = 5,
#'                        max.y.jit = 15,
#'                        jit = 2,
#'                        bite.type = "plat",
#'                        plot = TRUE)
#' df.2$measurement <- "m_02"
#'
#' # combine tibbles
#' df.all <- rbind(df.1, df.2)
#'
#' # reduce sampling frequency to 200 Hz
#' df.all.200 <- reduce_frq(df = df.all,
#'                          Hz = 200,
#'                          measurement.col = "measurement")
#'
#' plot(df.all.200 %>%
#'        filter(measurement == "m_02") %>%
#'        select(t, y),
#'      type = "l", col = "black")
#' lines(df.all.200 %>%
#'        filter(measurement == "m_01") %>%
#'        select(t, y),
#'      type = "l", col = "blue")
#' @export
reduce_frq <- function (df,
                        Hz = 200,
                        measurement.col = NULL){

  y <- t.frq <- y.frq <- measurement <- NULL

  # sample.rate <- diff(df$t[1:2])
  t.red.factor = 1000/Hz # [Hz]
  if(is.null(measurement.col)){
    print("You chose no \'measurement.col\', so the measurement should only conatin a single time series.")
    df <- df[,c(1:2)]
    colnames(df) <- c("t", "y")

    df.frq <- df %>%
      dplyr::group_by(t.frq = t.red.factor * round(t/t.red.factor)) %>%
      dplyr::summarize(y.frq = mean(y)) %>%
      dplyr::arrange(t.frq) %>%
      dplyr::rename(t = t.frq, y = y.frq)

  } else {
    print(paste0("The column \'", measurement.col, "\' will be used to group the separate time series."))
    measurement.col.no <- which(colnames(df) == measurement.col)

    df <- df[,c(1:2, measurement.col.no)]
    colnames(df) <- c("t", "y", "measurement")

    df.frq <- df %>%
      dplyr::group_by(t.frq = t.red.factor * round(t/t.red.factor), measurement) %>%
      dplyr::summarize(y.frq = mean(y)) %>%
      dplyr::arrange(measurement, t.frq) %>%
      dplyr::rename(t = t.frq, y = y.frq) %>%
      dplyr::select(t, y, measurement)
    colnames(df.frq)[3] <- measurement.col
  }

  return(df.frq)
}


#' Convert Time Series to Force
#'
#' Converts a time series, e.g. a continuous voltage measurement from a sensor to force data
#'   according to an amplification value and, depending on the measurement setup, the lever ratio of the
#'   rocker forwarding the force from the point the force acts on to the sensor.
#'
#'  These values should be stored in a `classifier` (s. below). At the same, it addas `specimen` and `species`
#'  info from the respective columns of the `classifier`.
#'
#' @details
#' The `classifier` should have the following format:
#'
#' | **`specimen`** | **`measurement`** | **`measurement`** | **`amp`** | **`lever.ratio`** |
#' | :----: | :----: | :----: |:----: | :----: |
#' | `specimen.1` | `species.1` | `measurement.1` | `amp.1` | `lever.ratio.1` |
#' | `...` | `...` | `...` | `...` | `...`  |
#' | `specimen.n` | `species.n` | `measurement.n` | `amp.n` | `lever.ratio.n` |
#'
#' If one one or both of the columns `amp` or `lever.ratio` are missing, these variables will be treated as `1`,
#'   i.e., the force data was not amplified and/or there was no lever in the measurement setup.
#'
#'   The force (`F`) in Newton is calculated *via* the following formula:
#'
#'   `F = y * lever.ratio * (1 / amp)`
#'
#'   where `y` is the measurement series, e.g. in `V`, \cr
#'   `amp` is the amplification value, e.g. in `V/N`, \cr
#'   and `lever.ratio` is the mechanical lever ratio of the measurement setup.
#'
#' @param df Data frame or tibble in the below mentioned format.
#' @param classifier Classifier in the below mentioned format.
#' @param measurement.col Character string. If `measurement.col` is not defined, the whole input data frames will be
#'   treated as if it was just one single time series. This is okay for data frames like that indeed only conatin one
#'   time series, but for data frames
#'   with multiple time series, a grouping column needs to be defined. Default: `NULL`
#' @return Returns a tibble reduced in the same format as the input tibble with an additional column called '"'`force`'.
#' @examples
#' # This example contains a self-sufficient data PREPARATION section
#' # before the function is actually run.
#'
#' require(dplyr)
#'
#' # simulate a short time series with sinusoidal bites
#' df.1 <- simulate_bites(no.of.bites = 7,
#'                        length.of.bite = 20,
#'                        length.of.series = 200,
#'                        max.y = 3,
#'                        max.y.jit = 15,
#'                        jit = 0.5,
#'                        bite.type = "sin",
#'                        plot = TRUE)
#' df.1$measurement <- "m_01"
#'
#' # simulate a short time series with plateu-like bites
#' df.2 <- simulate_bites(no.of.bites = 7,
#'                        length.of.bite = 30,
#'                        length.of.series = 300,
#'                        max.y = 30,
#'                        max.y.jit = 15,
#'                        jit = 2,
#'                       bite.type = "plat",
#'                       plot = TRUE)
#'df.2$measurement <- "m_02"
#'
#'# combine tibbles
#'df.all <- rbind(df.1, df.2)
#'
#'# create a classifier (see package vignette for details)
#'classifier <- tibble(measurement = c("m_01", "m_02"),
#'                      species = c("A", "B"),
#'                      specimen = c("a", "b"),
#'                      amp = c(2, 20),
#'                      lever.ratio = c(0.5, 0.5))
#'
#'# convert y column of df.all to force column using info from classifier
#'df.all <- y_to_force(df = df.all,
#'                       classifier = classifier,
#'                       measurement.col = "measurement")
#'df.all
#'
#'# plot the result. We see that the 10 x lower y values of m_01
#'# actually represent similar force values due the 10 x higher amplification used in m_02.
#'plot(df.all %>%
#'       filter(measurement == "m_01") %>%
#'       select(t, force),
#'     type = "l", col = "black")
#'lines(df.all %>%
#'        filter(measurement == "m_02") %>%
#'        select(t, force),
#'      type = "l", col = "blue")
#' @export
y_to_force <- function (df,
                        classifier,
                        measurement.col){

  amp <- lever.ratio <- y <- species <- specimen <- NULL

  # df <- df.all.200
  if(!("lever.ratio" %in% colnames(classifier))){
    classifier <- classifier %>% mutate(lever.ratio = 1)
  }
  if(!("amp" %in% colnames(classifier))){
    classifier <- classifier %>% mutate(amp = 1)
  }
  df <- df %>%
   left_join(classifier %>%
                select(all_of(measurement.col), species, specimen, amp, lever.ratio),
             by=measurement.col) %>%
    mutate(force = y * lever.ratio / amp) %>%
    select(species, specimen, measurement.col, t, force)
  return(df)
}
