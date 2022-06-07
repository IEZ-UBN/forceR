-- R CMD check results -------------------------------------- forceR 1.0.15 ----
Duration: 45.7s

0 errors v | 0 warnings v | 0 notes v

R CMD check succeeded


### Major changes
  * Optimized peak finding by adding/subtracting one time step from bite start/end, respectively, and by making sure that numbers are numeric within the 'find_strongest_peaks()' function
  * Added `peak_duration_max_force()` to calculate duration and maximum force per peak and store it in a tibble with one peak per row

### Minor changes:
  * Added checks of output folder existence to `rescale_peaks()`, `red_peaks_100()`, and `avg_peaks()`
  * Removed the necessity of having the species name in df.peaks of `rescale_peaks()`
  * Added some internal data checks with meaningful error messages to `rescale_peaks()` and `correct_peak()`
  * Disentangled in `plot.to.screen` and `path.plots` `find_best_fits()`
  * Updated vignette
  * Updated README.md
  * added DOI to CITATION

### Bug fixes
  * Corrected input tests of `y_to_force()`
