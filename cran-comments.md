## Test environments
* local Windows 10 install, R 3.5.2
* win-builder, with `check_win_devel()`
* Ubuntu 14.04.5 LTS, R 3.4 and devel, via [Travis CI](https://travis-ci.org/ms609/Quartet)
* R-hub, with `check_rhub(platforms = platforms())`

  * Installation failed with PREPERROR on rhub/ubuntu-gcc-release and
    rhub/fedora-clang-devel, on account of failures to install a required 
    dependency (`phangorn`).  This seems to be an issue outside
    of my control; the Debian rhub installation reports success.

## R CMD check results
There were no ERRORs or WARNINGs.

There was one NOTE:

 > Days since last update: 5

This release comes soon after the release of v1.0.0 on 2019-01-16, which failed
certain CRAN package checks -- Brian Ripley requested that these be fixed 
immediately.  After following Prof. Ripley's suggestions and testing the 
errant platforms using `check_rhub`, it appears that the issues have been 
resolved.

## Downstream dependencies
There are currently no downstream dependencies for this package.
