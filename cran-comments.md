#### notes:

-   removed the necessity for the classifier to contain `specimen` and `species` columns, since the only importand columns is the `measurement` columns. This happened as a response GitHub issue #3 (<https://github.com/Peter-T-Ruehr/forceR/issues/3>).
-   reduces df.peaks when plot_peaks() now to save time for plotting fewer measurements.

#### checks performed:

-   devtools::check_win_release()
-   devtools::check_win_devel()
-   devtools::check_win_oldrelease()
-   devtools::check_rhub()
-   devtools::check()
-   rhub::check_for_cran()

#### R CMD check results

Duration: 1m 16s

0 errors ✔ | 0 warnings ✔ | 0 notes ✔

R CMD check succeeded
