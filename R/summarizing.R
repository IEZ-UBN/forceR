#' Summarize Table
#'
#' Finds mininum, maximum and standard deviation of force per measurement and taxon and
#'  creates summary tibble.
#'
#' @param df Data frame or tibble containing at least three colums. The column names must contain the
#' grouping variables defined in `var1` and `var2` and the column `force` (time series of force measurements).
#' @param var1 A character string defining the column to calculate minimal and maximal force values per measurement.
#'   This must be the column that contains the unique measurement ID, e.g. measurement number.
#' @param var2 A character string defining the column for which the summary should be calculated.
#' @return A tibble summarizing the input data frame `df`. The resulting tibble will contain
#' the columns `t`, `force`, `measurement`, `species`, `specimen`, `amp`, `lever.ratio`,
#' `max.F.measurement`, `mean.F.specimen`, `max.F.specimen`, `sdv.max.F.specimen`, `n.measurements.in.specimen`.
#' @examples
#' require(dplyr)
#'
#' # PREPARATION ####
#' # create a classifier to store specimen info
#' classifier <- tibble(species = c("A","A","A","A","B","B","B","B"),
#'                      specimen = c("a","a","b","b","c","c","d","d"),
#'                      measurement = paste0("m_0", 1:8),
#'                      amp = c(rep(2,4), rep(0.5, 4)),
#'                      lever.ratio = rep(0.5, 8))
#'
#' # create temporary tibble to store data for bite series simulation
#' classifier.temp <- classifier %>%
#'   mutate(type = c(rep("sin", 4), rep("plat", 4)),
#'          max.y = c(3.0, 2.6, 2.2, 2.0, 5.0, 5.2, 8.0, 7.2),
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
#' rename(force = y)
#'
#' # add classifier info to bite table (df.all)
#' df.all <- left_join(df.all,
#'                     classifier,
#'                     by = "measurement")
#'
#' # RUN THE FUNCTION ####
#' # sumarize by measurement and specimen
#' df.all.summary <- summarize_measurements(df = df.all,
#'                        var1 = "measurement",
#'                        var2 = "specimen")
#'
#'
#'#  PLOT RESULT ####
#' \dontrun{
#' ggplot2::ggplot(data = df.all.summary, mapping = aes(x=specimen,y=max.F.measurement)) +
#'   geom_jitter(aes(color='blue'),alpha=0.7) +
#'   geom_boxplot(fill="bisque",color="black",alpha=0.3) +
#'   # scale_y_log10() +
#'   labs(y="max(F)/specimen") +
#'   guides(color=FALSE) +
#'   theme_minimal()
#' }
#' @export
summarize_measurements <- function(df, var1, var2){

  max.F.var1 <- t <- force <- NULL

  # df <- df.all.200.tax
  # var1 <- "measurement"
  # var2 <- "species"
  print(paste0("Summary will be created for '", var1, "' and '", var2, "'."))

  var1.col.no <- which(colnames(df) == var1)
  colnames(df)[var1.col.no] <- "var1"
  var2.col.no <- which(colnames(df) == var2)
  colnames(df)[var2.col.no] <- "var2"

  data.sumarized <- df %>%
    # find max Fs of measurements
    group_by(var1) %>%
    # calculate max force values for each var1
    mutate(max.F.var1 = max(force)) %>%
    # keep only one row per specimen
    slice(1) %>%
    # find mean F values for taxa (= species)
    group_by(var2) %>%
    # calculate species mean of max. Fs of all specimens of one ID (=species)
    mutate(mean.F.var2 = round(mean(max.F.var1),6),
           max.F.var2 = max(max.F.var1),
           sdv.max.F.var2 = sd(max.F.var1),
           # count observations per var2
           n.var1s.in.var2 = n()) %>%
    # keep only one row per var2 (= species)
    # slice(1) %>%
    ungroup()

  colnames(data.sumarized) <- gsub("var1", var1, colnames(data.sumarized))
  colnames(data.sumarized) <- gsub("var2", var2, colnames(data.sumarized))

  if("t" %in% colnames(data.sumarized)){
    data.sumarized <- data.sumarized %>%
    select(-t)
  }
  if("force" %in% colnames(data.sumarized)){
    data.sumarized <- data.sumarized %>%
      select(-force)
  }

  return(data.sumarized)
}
