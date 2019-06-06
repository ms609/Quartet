This release addresses the RNG issue flagged by Kurt Hornik as requiring an 
immediate fix.

## Test environments
* local Windows 10 install, R 3.5.2
* win-builder, with `check_win_devel()`
* Ubuntu 14.04.5 LTS, R 3.4 and devel, via [Travis CI](https://travis-ci.org/ms609/Quartet)
* R-hub, with `check_rhub(platforms = rhub::platforms()[[1]])`

  * Installation failed with PREPERROR on rhub/ubuntu-gcc-release and
    rhub/fedora-clang-devel (and other platforms), on account of failures 
    to install a required dependencies.  
    This seems to be an issue outside of my control.
  * Installation failed with ERROR on ubuntu-gcc-devel and debian-gcc-patched:
    > "there is no package called `lattice`"
    The failure to install the dependency `lattice` seems to be outwith my 
    control.

## R CMD check results
There were no ERRORs or WARNINGs.

There was one NOTE:

> Found the following (possibly) invalid URLs:
>   URL: http://doi.org/10.2307/2413326
>     From: man/CompareQuartets.Rd
>           man/CompareSplits.Rd
>           man/QuartetState.Rd
>           man/QuartetStatus.Rd
>           man/SimilarityMetrics.Rd
>           man/TQDist.Rd
>     Status: 403
>     Message: Forbidden
>   URL: http://doi.org/10.2307/2413347
>     From: man/SplitStatus.Rd
>     Status: 403
>     Message: Forbidden

These URLs are generated from DOIs in citations, and are valid.

## Downstream dependencies

The modifications only affect documentation objects and thus do not impact
the reverse dependency `CongreveLamsdell2016` (which I maintain).
