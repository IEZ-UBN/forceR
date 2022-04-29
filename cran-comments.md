## -- R CMD check results ----------------------------------- forceR 1.0.13 ----
Duration: 45.9s

0 errors v | 0 warnings v | 0 notes v

R CMD check succeeded


## Misc
Issue:
  Please make sure that you do not change the user's options, par or
  working directory. If you really have to do so within functions, please
  ensure with an *immediate* call of on.exit() that the settings are reset
  when the function is exited. e.g.:
  ...
  oldpar <- par(no.readonly = TRUE)    # code line i
  on.exit(par(oldpar))            # code line i + 1
  ...
  par(mfrow=c(2,2))            # somewhere after
  ...
  Please also do this in the following files:
    R/normalization.R
    R/peak_finding.R
    R/polynomial.R

Solution:
I added this to the beginning of the functions. Sorry I missed that.


Kind regards,
Peter
