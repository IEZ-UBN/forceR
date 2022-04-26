## -- R CMD check results ----------------------------------- forceR 1.0.12 ----
Duration: 44s

0 errors v | 0 warnings v | 0 notes v

R CMD check succeeded


## Misc
Many many thanks for the careful checking of my package. I'll address the issues raised by Gregor Seyer and Uwe Ligges one by one:

* Issue 1)
Please always write package names, software names and API (application programming interface) names in single quotes in title and description. e.g: --> 'forceR'

* Solution:
changed forceR to 'forceR' in DESCRIPTION: Description. I did not find any other instances of packages, software or APIs.



* Issue 2)
\dontrun{} should only be used if the example really cannot be executed (e.g. because of missing additional software, missing API keys, ...) by the user. That's why wrapping examples in \dontrun{} adds the comment ("# Not run:") as a warning for the user. Does not seem necessary in all cases. Please unwrap the examples if they are executable in < 5 sec, or replace \dontrun{} with \donttest{}.  

* Solution:
I got rid of all dontrun{} calls by restructuring the functions away from writing files, and by adding three example files in inst/extdata that can actually be loaded as files. I out-commented the two function examples that need interactive user input, because otherwise donttest{} throws errors.



* Issue 3)
Please ensure that your functions do not write by default or in your examples/vignettes/tests in the user's home filespace (including the package directory and getwd()). This is not allowed by CRAN policies. In your examples/vignettes/tests you can write to tempdir().

* Solution:
I changed the structure of all previously mandatory file-writing functions so that instead they return() their results, and file writing is optional. Nothing writes files by default anymore, and if the user wants files to be written, they must explicitly specify the path. The only exception is 'sort_ _files()', which creates a single folder and either copies (default) or moves files from user-defined folders. I out-commented the function example as well, because otherwise the 'check' directory would not be empty after package testing.



* Issue 4)
You write information messages to the console that cannot be easily suppressed. It is more R like to generate objects that can be used to extract the information a user is interested in, and then print() that object. Instead of print()/cat() rather use message()/warning()  or if(verbose)cat(..) (or maybe stop()) if you really have to write text to the console. (except for print, summary, interactive functions)

* Solution:
I have removed all 'print()' calls. I only kept console output, e.g. printing progress for functions that typically take quite a bit of time, to let the user know about the progress. In these cases, I added the argument (show_progress = FALSE) to let the user decide if that is wanted. In interactive functions I kept the info printed to the console to an informative minimum and used message() instead of print.



* Issue 5)
Please make sure that you do not change the user's options, par or working directory. If you really have to do so within functions, please ensure with an *immediate* call of on.exit() that the settings are reset when the function is exited. e.g.:
...
oldpar <- par(no.readonly = TRUE)    # code line i
on.exit(par(oldpar))            # code line i + 1
...
par(mfrow=c(2,2))            # somewhere after
...
e.g.:
If you're not familiar with the function, please check ?on.exit. This function makes it possible to restore options before exiting a function even if the function breaks. Therefore it needs to be called immediately after the option change within a function.

* Solution 5:
I did as suggested in plot_peaks():

oldpar <- par(no.readonly = TRUE)    # code line i 
on.exit(par(oldpar), add = TRUE)    # code line i + 1
par(mfrow=c(3,2))                    # code line i + 2


* Issue 6)
The vignette creation tried to save data locally - I have forgotten to set `eval=FALSE` in one of the code elements. I'm sorry for that.

Kind regards,
Peter
