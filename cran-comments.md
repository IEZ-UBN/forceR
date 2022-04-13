## -- R CMD check results ------------------------------------ forceR 1.0.9 ----
Duration: 39.1s

0 errors v | 0 warnings v | 0 notes v

R CMD check succeeded

## Misc
This is the second submission (of a new release).
we have solved the 2 issues addressed by Uwe Wiggins on 
  our first submission attmempt (forceR 1.0.7):

Issue 1:
  License components with restrictions and base license permitting such:
     MIT + file LICENSE
   File 'LICENSE':
     MIT License

     Copyright (c) 2022 Peter T. RÃ¼hr

     Permission is...

  Please only ship the CRAN template for the MIT license.
  
Solution 1:
  we have changed the content of LICENSE to only contain the following lines:


Issue 2:
  Found the following (possibly) invalid file URI:
     URI: xxx
       From: inst/doc/forceR.html
       
Solution 2:
       changed "[CRAN](xxx)" to "CRAN" in vignette source file.


Question 1:
  Is there some reference about the method you can add in the Description
  field in the form Authors (year) <doi:10.....> or <arXiv:.....>?
  
Answer 1:
  We just submitted the revised version of a manuscript in 'Methods in Ecology 
    and Evolution' which had good comments in the first round of reviews. As 
    soon as it gets published, we would update the info on the CRAN package.
