## Test environments
* local Windows 10 install, R 3.5.2
* win-builder, with `check_win_devel()`
* Ubuntu 14.04.5 LTS, R 3.4 and devel, via [Travis CI](https://travis-ci.org/ms609/Quartet)
* R-hub, with `check_rhub(platforms = platforms())`

  * Installation failed with PREPERROR on rhub/ubuntu-gcc-release and
    rhub/fedora-clang-devel (and other platforms), on account of failures 
    to install a required dependency (`phangorn`).  
    This seems to be an issue outside of my control.
  * Installation failed with ERROR on x86_64-pc-linux-gnu:
    > "there is no package called `lattice`"
    The failure to install the dependency `lattice` seems to be outwith my 
    control.

## R CMD check results
There were no ERRORs or WARNINGs.

There was one NOTE:

 > Days since last update: 6

This release comes on the heels of v1.0.0 on 2019-01-16, which failed
certain CRAN package checks -- Brian Ripley requested that these be fixed 
immediately and kindly proposed some suggestions for how portability could be
improved.
After implementing these and testing the relevant platforms using `check_rhub`, 
I believe that the issues have been resolved.

## Downstream dependencies
There are currently no downstream dependencies for this package.
