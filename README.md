[![Build Status](https://travis-ci.org/ms609/SlowQuartet.svg?branch=master)](https://travis-ci.org/ms609/SlowQuartet)
[![codecov](https://codecov.io/gh/ms609/SlowQuartet/branch/master/graph/badge.svg)](https://codecov.io/gh/ms609/SlowQuartet)
[![CRAN Status Badge](http://www.r-pkg.org/badges/version/SlowQuartet)](https://cran.r-project.org/package=SlowQuartet)
[![CRAN Downloads](http://cranlogs.r-pkg.org/badges/SlowQuartet)](https://cran.r-project.org/package=SlowQuartet)<!--
[![Research software impact](http://depsy.org/api/package/cran/SlowQuartet/badge.svg)](http://depsy.org/package/r/SlowQuartet)-->
[![Project Status: Active.](http://www.repostatus.org/badges/latest/active.svg)](http://www.repostatus.org/#active)

# SlowQuartet

SlowQuartet is an R package that calculates the Quartet distance between two trees:
a measure of their similarity based on the number of shared four-taxon subtrees.

It's named Slow Quartet as it calculates the metric rather inefficiently; it may not be of use for large trees. 
For bifurcating trees, this package provides a gateway to the R package `tqDist`,
which calculates quartet distances much more rapidly.

What other implementations don't seem to do is to distinguish between quartets
that are contradicted by one tree, and quartets that are simply absent due to
a lack of resolution (i.e. the presence of polytomies).  `SlowQuartet` makes
this distinction in both the quartet metric (function `MatchingQuartets`) and the
partition metric (i.e. Robinson-Foulds distance; function `MatchingSplits`).


## Using the package
The package will soon be compiled and uploaded to the CRAN repository.  
Meanwhile, you can install the latest version of the package into R thus:

```r
# Install the devtools package from CRAN
install.packages('devtools')

# Install the inapplicable package from github
devtools::install_github('ms609/SlowQuartet', args='--no-multiarch')

# Load the package into R
library('SlowQuartet')
```

You will need Rtools installed (https://cran.r-project.org/bin/windows/Rtools/) to allow R to build the package.

## References
- Brodal G.S., Fagerberg R., Pedersen C.N.S. 2004. Computing the quartet distance between evolutionary trees in time O(_n_ log _n_). Algorithmica. 38:377–395.

- Estabrook G.F., McMorris F.R., Meacham C.A. 1985. Comparison of undirected phylogenetic trees based on subtrees of four evolutionary units. Syst. Zool. 34:193–200.

- Robinson D.F., Foulds L.R. 1981. Comparison of phylogenetic trees. Math. Biosci. 53:131–147.

- Sand A., Holt M.K., Johansen J., Brodal G.S., Mailund T., Pedersen C.N.S. 2014. tqDist: a library for computing the quartet and triplet distances between binary or general trees. Bioinformatics. 30:2079–2080. https://doi.org/10.1093/bioinformatics/btu157

- Smith, M.R. (forthcoming)

- Steel, M. and Penny, D. Distributions of tree comparison metrics: some new results. Syst. Biol. (1993) 42 (2): 126-141. https://doi.org/10.1093/sysbio/42.2.126
