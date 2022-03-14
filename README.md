# forceR

**A vignette, which guides you through all functions of the package, is is available [here (HTML)](https://htmlpreview.github.io/?https://github.com/Peter-T-Ruehr/forceR/blob/main/vignettes/forceR.html) and [here (PDF)](https://github.com/Peter-T-Ruehr/forceR/blob/main/vignettes/forceR.pdf)**.

## Functionality
The package `forceR` has originally been written for insect bite force data preparation and analysis, but it can be used for any kind of time series measurements. Functions include 

* loading, plotting, and cropping of data
* correction of charge amplifier drifts
* correction of baseline drifts
* reduction of sampling frequency
* automatic extraction of single peaks
* rescaling (normalization) of curves
* reduction of curves to 100 time steps each
* finding of best polynomial fits to describe all curves

## This is the development verison
This github repository holds a copy of the current development version of the R package `forceR` on CRAN.

This development version is as or more recent than the official release of GET on the Comprehensive R Archive Network (CRAN) at https://cran.r-project.org/package=GET

## Installation
<!-- # Official release -->
<!-- You can install the officially  -->
<!-- You can install the development version of `forceR` from [GitHub](https://github.com/Peter-T-Ruehr/forceR): -->
# ```{r warning=FALSE, message=FALSE, eval=F}
# install.packages('forceR')
# ```

# Development version
You can install the development version of `forceR` from [GitHub](https://github.com/Peter-T-Ruehr/forceR):
```{r warning=FALSE, message=FALSE, eval=F}
require(devtools)
devtools::install_github("https://github.com/Peter-T-Ruehr/forceR")
```

## Citation
If you use this package, please cite the original publication (currently under review):

Rühr, PT & Blanke, A (**2021**): `forceX` and `forceR`: a mobile setup and R package to measure and analyze a wide range of animal closing forces. *XXX* **XXX**: pp.XX-XX. doi: XXX
