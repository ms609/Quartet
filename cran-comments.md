## Test environments

* Local PC:
  - Windows 10, R 4.2.0

* [GitHub Actions](https://github.com/ms609/Quartet/actions)
  - Ubuntu 20.04
    - R 3.4
    - R release (tests, examples & vignettes run with valgrind)
    - R devel
  - Mac OS X 10.15.7, R release
  - Microsoft Windows Server 2019 10.0.17763, R release
  
* R-hub, with `rhub::check_for_cran()` and `devtools::check_win_devel()`


## R CMD check results

There were no ERRORs or WARNINGs or NOTEs.

## Downstream dependencies

The modifications do not impact the reverse dependencies `CongreveLamsdell2016`
or `TreeDist` (which I maintain).  
