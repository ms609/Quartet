[![Project Status: Inactive.](http://www.repostatus.org/badges/latest/inactive.svg)](http://www.repostatus.org/#inactive)
[![Build Status](https://travis-ci.org/ms609/Quartet.svg?branch=master)](https://travis-ci.org/ms609/Quartet)
[![codecov](https://codecov.io/gh/ms609/Quartet/branch/master/graph/badge.svg)](https://codecov.io/gh/ms609/Quartet)
[![CRAN Status Badge](http://www.r-pkg.org/badges/version/Quartet)](https://cran.r-project.org/package=Quartet)
[![CRAN Downloads](http://cranlogs.r-pkg.org/badges/Quartet)](https://cran.r-project.org/package=Quartet)
[![DOI](https://zenodo.org/badge/80424189.svg)](https://zenodo.org/badge/latestdoi/80424189)

# Quartet

Quartet is an R package that calculates the Quartet distance between two trees:
a measure of their similarity based on the number of shared four-taxon subtrees.

The package uses the `tqDist` algorithm.  Unlike many other implementations,
it distinguishes between quartets that are contradicted by one tree,
and quartets that are simply absent due to a lack of resolution (i.e. the presence
of polytomies).  `Quartet` makes this distinction in both the quartet metric
(function `QuartetStatus`) and the partition metric (i.e. Robinson-Foulds distance;
function `SplitStatus`).


## Using the package
The package will soon be compiled and uploaded to the CRAN repository.  
Meanwhile, you can install the latest version of the package into R thus:

```r
# Install the devtools package from CRAN
install.packages('devtools')

# Install the package from github
devtools::install_github('ms609/Quartet', args='--no-multiarch')

# Load the package into R
library('Quartet')
```

You will need [Rtools](https://cran.r-project.org/bin/windows/Rtools/) installed to allow R to build the package.

## References
- Brodal G.S., Fagerberg R., Pedersen C.N.S. 2004. Computing the quartet 
  distance between evolutionary trees in time O(_n_ log _n_). 
  Algorithmica. 38:377–395.

- Estabrook G.F., McMorris F.R., Meacham C.A. 1985. Comparison of undirected 
  phylogenetic trees based on subtrees of four evolutionary units. 
  Syst. Zool. 34:193–200.

- Robinson D.F., Foulds L.R. 1981. Comparison of phylogenetic trees. 
  Math. Biosci. 53:131–147.

- Sand A., Holt M.K., Johansen J., Brodal G.S., Mailund T., Pedersen C.N.S. 2014.
  tqDist: a library for computing the quartet and triplet distances between 
  binary or general trees. 
  Bioinformatics. 30:2079–2080. https://doi.org/10.1093/bioinformatics/btu157

- Smith, M.R. (2019) Bayesian and parsimony approaches reconstruct 
  informative trees from simulated morphological datasets. Biology Letters. 
  In production.

- Steel, M. and Penny, D. Distributions of tree comparison metrics: some new results.
  Syst. Biol. (1993) 42 (2): 126-141. https://doi.org/10.1093/sysbio/42.2.126
