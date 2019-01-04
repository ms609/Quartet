## Test environments
* local Windows 10 install, R 3.5.2
* win-builder, with `check_win_devel()`
* Ubuntu 14.04.5 LTS, R 3.4 and devel, via Travis CI (https://travis-ci.org/ms609/Quartet)
* R-hub, with `check_rhub()`

  * Installation failed with PREPERROR on rhub/ubuntu-gcc-release and
    rhub/fedora-clang-devel, on account of failures to install the required 
    packages (`phangorn`; `git2r`).  This seems to be an issue outside
    of my control; the Debian rhub installation reports success.


## R CMD check results
There were no ERRORs or WARNINGs.

There was 1 NOTE:

* checking CRAN incoming feasibility ... NOTE
  Maintainer: 'Smith Martin R. <martin.smith@durham.ac.uk>'
  
  New submission
  
This package is a new submission.

## Downstream dependencies
There are currently no downstream dependencies for this package.
