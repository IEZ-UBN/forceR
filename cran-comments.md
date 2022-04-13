## -- R CMD check results ----------------------------------- forceR 1.0.10 ----
Duration: 39s

0 errors v | 0 warnings v | 0 notes v

R CMD check succeeded

## Misc
This is the third submission (of a new release).

Issue 1:
  Thanks, please omit the redundant "An R package".
       
Solution 1:
  done!
  


Question 1:
  Is there some reference about the method you can add in the Description
  field in the form Authors (year) <doi:10.....> or <arXiv:.....>?
  
Answer 1:
  We have changed the line
  year         = "in press",
  to
  year         = "2022",
  
  However, the article describing this package was just accepted in 'Methods in 
    Ecology and Evolution' today. As soon as we get one a DOI, we will update 
    the info on the CRAN package.
