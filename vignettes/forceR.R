## ----setup,  include = FALSE--------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----warning=FALSE, message=FALSE, eval=F-------------------------------------
#  install.packages('forceR')

## ----warning=FALSE, message=FALSE, eval=F-------------------------------------
#  require(devtools)
#  devtools::install_github("https://github.com/Peter-T-Ruehr/forceR")

## ----warning=FALSE, message=FALSE---------------------------------------------
library(magrittr)
library(dplyr)
library(stringr)
library(purrr)
library(ggplot2)
library(readr)

library(forceR)

## ----eval=FALSE, warning=FALSE, message=FALSE---------------------------------
#  data.folder <- "./example_data"

## ----eval=TRUE, warning=FALSE, message=FALSE, include=F-----------------------
data.folder <- "C:/Users/pruehr.EVOLUTION/Documents/forceR_bkp_2022-03-01/vignettes/example_data"

## ----eval=FALSE, warning=FALSE, message=FALSE, fig.width = 7, fig.height=6----
#  file <- file.path(data.folder, "0982.csv")
#  plot_measurement(file,
#                   columns = c(1:2))

## ----eval=TRUE, warning=FALSE, message=FALSE, include=F-----------------------
cropped.folder <- "C:/Users/pruehr.EVOLUTION/Documents/forceR_bkp_2022-03-01/vignettes/example_data/cropped"

## ----eval=FALSE, warning=FALSE, message=FALSE---------------------------------
#  cropped.folder <- "./example_data/cropped"

## ----eval=FALSE, warning=FALSE, message=FALSE---------------------------------
#  file.cropped <- crop_measurement(file,
#                                   path.data = cropped.folder)

## ----eval=FALSE, warning=FALSE, message=FALSE, fig.width = 7, fig.height=6----
#  cropped.folder <- "./example_data/cropped"
#  
#  file <- file.path(cropped.folder, "0982_cropped.csv")
#  plot_measurement(file)

## ----eval=TRUE, warning=FALSE, message=FALSE, include=F-----------------------
cropped.folder <- "C:/Users/pruehr.EVOLUTION/Documents/forceR_bkp_2022-03-01/vignettes/example_data/cropped"
ampdriftcorr.folder <- "C:/Users/pruehr.EVOLUTION/Documents/forceR_bkp_2022-03-01/vignettes/example_data/cropped/ampdriftcorr"

## ----eval=FALSE, warning=FALSE, message=FALSE---------------------------------
#  # create file list with all cropped files that need amplifier drift corrections.
#  #   Her we use the cropped.folder with the cropped measurements.
#  file.list <- list.files(cropped.folder,
#                          pattern = "csv$",
#                          full.names = TRUE)
#  
#  # define folder where to save the amplifier drift corrected file.
#  #   If this folder exists, it will be created.
#  ampdriftcorr.folder <- "./cropped/ampdriftcorr"
#  
#  for(filename in file.list){
#    print(filename)
#    amp_drift_corr(filename = filename,
#                   tau = 9400,
#                   res.reduction = 10,
#                   plot.to.screen = FALSE,
#                   write.data = TRUE,
#                   write.PDFs = TRUE,
#                   write.logs = TRUE,
#                   output.folder = ampdriftcorr.folder,
#                   show.progress = FALSE)
#    print("***********")
#  }

## ----eval=TRUE, warning=FALSE, message=FALSE, include=F-----------------------
cropped.folder <- "C:/Users/pruehr.EVOLUTION/Documents/forceR_bkp_2022-03-01/vignettes/example_data/cropped"
ampdriftcorr.folder <- "C:/Users/pruehr.EVOLUTION/Documents/forceR_bkp_2022-03-01/vignettes/example_data/cropped/ampdriftcorr"
baselinecorr.folder <- "C:/Users/pruehr.EVOLUTION/Documents/forceR_bkp_2022-03-01/vignettes/example_data/cropped/ampdriftcorr/baselinecorr"

## ----eval=FALSE, warning=FALSE, message=FALSE, fig.width = 7, fig.height=6----
#  
#  filename = file.path(ampdriftcorr.folder, "1068_ampdriftcorr.csv")
#  
#  plot_measurement(filename)
#  
#  baselinecorr.folder <- "./cropped/ampdriftcorr/baselinecorr"
#  
#  file.baselinecorr <- baseline_corr(filename = filename,
#                                     corr.type = "auto",
#                                     window.size.mins = 2000,
#                                     window.size.means = NULL,
#                                     quantile.size = 0.05,
#                                     y.scale = 0.5,
#                                     res.reduction = 10,
#                                     Hz = 100,
#                                     plot.to.screen = TRUE,
#                                     write.data = TRUE,
#                                     write.PDFs = TRUE,
#                                     write.logs = TRUE,
#                                     output.folder = baselinecorr.folder,
#                                     show.progress = FALSE)

## ----eval=FALSE, warning=FALSE, message=FALSE, fig.width = 7, fig.height=6----
#  filename = file.path(ampdriftcorr.folder, "1174_ampdriftcorr.csv")
#  
#  plot_measurement(file)
#  
#  file.baselinecorr <- baseline_corr(filename = filename,
#                                     corr.type = "manual",
#                                     plot.to.screen = TRUE,
#                                     write.data = TRUE,
#                                     write.PDFs = TRUE,
#                                     write.logs = TRUE,
#                                     output.folder = baselinecorr.folder,
#                                     show.progress = FALSE)

## ----eval=TRUE, warning=FALSE, message=FALSE, include=F-----------------------
data.folder <- "C:/Users/pruehr.EVOLUTION/Documents/forceR_bkp_2022-03-01/vignettes/example_data"
cropped.folder <- "C:/Users/pruehr.EVOLUTION/Documents/forceR_bkp_2022-03-01/vignettes/example_data/cropped"
ampdriftcorr.folder <- "C:/Users/pruehr.EVOLUTION/Documents/forceR_bkp_2022-03-01/vignettes/example_data/cropped/ampdriftcorr"
baselinecorr.folder <- "C:/Users/pruehr.EVOLUTION/Documents/forceR_bkp_2022-03-01/vignettes/example_data/cropped/ampdriftcorr/baselinecorr"
data.folders <- c(data.folder,
                  cropped.folder,
                  ampdriftcorr.folder,
                  baselinecorr.folder)
results.folder <- "C:/Users/pruehr.EVOLUTION/Documents/forceR_bkp_2022-03-01/vignettes/example_data/corrected"

## ----eval=FALSE, warning=FALSE, message=FALSE---------------------------------
#  data.folders <- c(data.folder,
#                    file.path(data.folder, "/cropped"),
#                    file.path(data.folder, "/cropped/ampdriftcorr"),
#                    file.path(data.folder, "/cropped/ampdriftcorr/baselinecorr"))
#  
#  results.folder <- file.path(data.folder, "/corrected/")
#  
#  sort_files(data.folders = data.folders,
#             results.folder = results.folder,
#             move = FALSE)

## ----eval=FALSE, warning=FALSE, message=FALSE---------------------------------
#  file.list <- list.files(results.folder, pattern = "csv", full.names = TRUE)
#  df.1 <- load_single(file = file.list[1],
#                      columns = c(1:2))

## ----eval=FALSE, warning=FALSE, message=FALSE---------------------------------
#  df.all <- load_mult(folder = results.folder,
#                      columns = c(1:2),
#                      show.progress = TRUE)

## ----eval=TRUE, warning=FALSE, message=FALSE----------------------------------
# create/replace df.all
df.all <- forceR::df.all
head(df.all)

## ----eval=TRUE, warning=FALSE, message=TRUE, include=TRUE, fig.width = 7, fig.height=6----
# plot simulated measurements
ggplot(df.all,
       aes(x = t ,
           y = y,
           colour=measurement)) +
  geom_line()

## ----eval=TRUE, warning=FALSE, message=FALSE----------------------------------
# reduce frequency to 200 Hz
df.all.200 <- reduce_frq(df = df.all, 
                         Hz = 200,  
                         measurement.col = "measurement")

head(df.all.200)

## ----eval=TRUE, warning=FALSE, message=FALSE----------------------------------
# create a classifier
number_of_species <- 4
number_of_specimens_per_species <- 3
number_of_measurements_per_specimen <- 2
number_of_rows <- number_of_species *
  number_of_specimens_per_species *
  number_of_measurements_per_specimen

species <- sort(rep(paste0("species_", LETTERS[1:number_of_species]),
                    length=number_of_rows))

specimens <- sort(rep(paste0("speciemen_", letters[1:(number_of_species*number_of_specimens_per_species)]),
                      length=number_of_rows))

classifier <- tibble(species = species,
                     specimen = specimens,
                     measurement = paste0("m_",  str_pad(string= 1:number_of_rows, width = 2, pad = "0")),
                     amp = c(rep(0.5, number_of_rows/2), rep(2, number_of_rows/2)),
                     lever.ratio = rep(0.5, number_of_rows))
head(classifier)

## ----eval=TRUE, warning=FALSE, message=FALSE----------------------------------
df.all.200.tax <- y_to_force(df = df.all.200, 
                             classifier = classifier, 
                             measurement.col = "measurement")
head(df.all.200.tax)

## ----eval=TRUE, warning=FALSE, message=FALSE, fig.width = 7, fig.height=6-----
var1 = "measurement"
var2 = "specimen"
df.summary.specimen <- summarize_measurements(df.all.200.tax, 
                                              var1, 
                                              var2)
head(df.summary.specimen)

# boxplot of maximum force in specimens
ggplot(data = df.summary.specimen, mapping = aes(x=specimen,y=max.F.measurement)) +
  geom_jitter(aes(color='blue'),alpha=0.7) +
  geom_boxplot(fill="bisque",color="black",alpha=0.3) +
  # scale_y_log10() +
  labs(y="max(F)/specimen") +
  guides(color="none") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust=1))


## ----eval=TRUE, warning=FALSE, message=FALSE, fig.width = 7, fig.height=6-----
df.summary.species <- df.summary.specimen %>%
  # find max Fs of species
  group_by(species) %>%
  # calculate force values for each species
  mutate(max.F.species = max(max.F.specimen),
         mean.F.species = round(mean(max.F.specimen),6),
         sdv.max.F.species = sd(max.F.specimen)) %>% 
  ungroup() %>% 
  # count specimens / species
  group_by(species) %>% 
  mutate(n.specimens.in.species = length(unique(specimen))) %>% 
  ungroup()
df.summary.species

# boxplot of maximum force in species
ggplot(data = df.summary.species, mapping = aes(x=species,y=max.F.specimen)) +
  geom_jitter(aes(color='blue'),alpha=0.7) +
  geom_boxplot(fill="bisque",color="black",alpha=0.3) +
  # scale_y_log10() +
  labs(x='species', y="max(F)/specimen") +
  guides(color="none") +
  theme_minimal()


## ----eval=FALSE, warning=FALSE, message=FALSE---------------------------------
#  # create folders to save df and results
#  path.plots <- paste0(data.folder, "/plots/")
#  ifelse(!dir.exists(path.plots), dir.create(path.plots), "./plots already exists")
#  
#  path.plots.initial_peak_finding <- paste0(data.folder, "/plots/initial_peak_finding/")
#  ifelse(!dir.exists(path.plots.initial_peak_finding), dir.create(path.plots), "./plots/initial_peak_finding already exists")
#  
#  path.data <- paste0(data.folder, "/data/")
#  ifelse(!dir.exists(path.data), dir.create(path.data), "./data already exists")
#  

## ----eval=FALSE, warning=FALSE, message=FALSE---------------------------------
#  peaks.df <- find_strongest_peaks(df = df.all.200.tax,
#                                   no.of.peaks = 5,
#                                   plot.to.screen = TRUE,
#                                   path.data = path.data,
#                                   path.plots = path.plots,
#                                   show.progress = TRUE)
#  peaks.df

## ----eval=FALSE, warning=FALSE, message=FALSE---------------------------------
#  plot_peaks(df.peaks = peaks.df,
#             df.data = df.all.200.tax,
#             additional.msecs = 2000,
#             path.plots = path.plots)

## ----eval=FALSE, warning=FALSE, message=FALSE---------------------------------
#  peaks.df <- correct_peak(df.peaks = peaks.df,
#                           df.data = df.all.200.tax,
#                           measurement = "m_01",
#                           peak = 1,
#                           additional.msecs = 100,
#                           path.data = path.data)

