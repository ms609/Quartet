## Test environments
* local Windows 10 install, R 3.5.1
* win-builder, with `check_win_devel()`
* R-hub, with `check_rhub()`

  * Installation failed on rhub/ubuntu-gcc-release, on account of a failure to 
    install the required package `phangorn`.  This seems to be an issue outside
    of my control.


## R CMD check results
There were no ERRORs or WARNINGs.

There was 1 NOTE:

* checking CRAN incoming feasibility ... NOTE
  Maintainer: 'Smith Martin R. <martin.smith@durham.ac.uk>'
  
  New submission
  
This package is a new submission.

## Downstream dependencies
There are currently no downstream dependencies for this package.
