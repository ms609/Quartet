## Test environments
* local Windows 10 install, R 3.6.1
* Ubuntu 16.04.6 LTS, R 3.4.0, release and devel, via [Travis CI](https://travis-ci.org/ms609/Quartet)
* Mac OS X 10.13.3, R release, via Travis
* win-builder, with `check_win_devel()`
* R-hub, with `check_rhub(platforms = rhub::platforms()[[1]])`

## R CMD check results
There was one ERROR and WARNING:

> * checking package dependencies ... ERROR
> Package required but not available: 'TreeTools'

> Strong dependencies not in mainstream repositories:
>   TreeTools

The `TreeTools` package is currently pending approval for CRAN submission;
issues raised by Martina Schmirl on 2019-11-25 were fixed and resubmitted.

I am submitting Quartet now so that the package is not removed from CRAN on
Dec 1st, per Brian Ripley's request to remove the deprecated ptr_fun.  
(I am unlikely to be available between today and Dec 1st to wait for TreeTools
to progress on to CRAN).

Hopefully it will be possible for you to run the incoming tests once the 
TreeTools package is available on CRAN.


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
