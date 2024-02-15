## Test environments

* Local PC:
  - Windows 10, R Under development (unstable) (2023-08-17 r84984 ucrt)

* [GitHub Actions](https://github.com/ms609/Quartet/actions)
  - Ubuntu 22.04
    - R 3.6
    - R release (tests, examples & vignettes run with valgrind)
    - R devel
  - Mac OS X, R release
  - Microsoft Windows Server, R release
  
* R-hub, with `rhub::check_for_cran()` and `devtools::check_win_devel()`


## R CMD check results

There were no ERRORs or WARNINGs or NOTEs.

## Downstream dependencies

Reverse dependencies were tested using
[RevDepCheck](https://github.com/ms609/Quartet/actions/workflows/revdepcheck.yml)

The modifications do not impact the reverse dependencies `CongreveLamsdell2016`,
`TreeDist` or `TreeSearch` (all of which which I maintain).

