## Test environments

* Local PC:
  - Windows 10 & 11, R Under development (unstable) (2026-06-02 r90096 ucrt)

* [GitHub Actions](https://github.com/ms609/Quartet/actions)
  - Ubuntu 24.04
    - R 4.1
    - R release (tests, examples & vignettes run with valgrind & ASan)
    - R devel
  - macOS-latest, R release
  - Microsoft Windows Server, R release
* [R-hub](https://github.com/ms609/Quartet/actions/workflows/rhub.yaml)
  - Windows, Mac & Linux

## R CMD check results

There were no ERRORs or WARNINGs or NOTEs.

## Downstream dependencies

Reverse dependencies were tested using
[RevDepCheck](https://github.com/ms609/Quartet/actions/workflows/revdepcheck.yml)

The modifications do not impact the reverse dependencies `CongreveLamsdell2016`,
`ConsTree`, `TreeDist` or `TreeSearch` (all of which which I maintain).

