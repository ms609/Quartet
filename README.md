[![Project Status: Inactive.](https://www.repostatus.org/badges/latest/inactive.svg)](https://www.repostatus.org/#inactive)
[![Build Status](https://travis-ci.org/ms609/Quartet.svg?branch=master)](https://travis-ci.org/ms609/Quartet)
[![codecov](https://codecov.io/gh/ms609/Quartet/branch/master/graph/badge.svg)](https://codecov.io/gh/ms609/Quartet)
[![CRAN Status Badge](https://www.r-pkg.org/badges/version/Quartet)](https://cran.r-project.org/package=Quartet)
[![CRAN Downloads](https://cranlogs.r-pkg.org/badges/Quartet)](https://cran.r-project.org/package=Quartet)
[![DOI](https://zenodo.org/badge/80424189.svg)](https://zenodo.org/badge/latestdoi/80424189)

# Quartet

'[Quartet](https://ms609.github.io/Quartet/)' is an R package that calculates 
the Quartet distance between two trees:
a measure of their similarity based on the number of shared four-taxon subtrees.

The package uses the `tqDist` algorithm.  Unlike many other implementations,
it distinguishes between quartets that are contradicted by one tree,
and quartets that are simply absent due to a lack of resolution (i.e.
the presence of polytomies).
'Quartet' makes this distinction in both the quartet metric (function
`QuartetStatus()`) and the partition metric (i.e. Robinson-Foulds distance;
function `SplitStatus()`).

## Using the package
Install and load the library from CRAN as follows:
```
install.packages('Quartet')
library('Quartet')
```

If you're feeling brave, you can install the development version thus:
```r
if(!require(devtools)) install.packages("devtools")
devtools::install_github('ms609/Quartet')
```

You will need [Rtools](https://cran.r-project.org/bin/windows/Rtools/) installed 
in order to build the development version from source.

View the [function reference](https://ms609.github.io/Quartet/reference/)
and [basic usage instructions](https://ms609.github.io/Quartet/articles/Using-Quartet.html).

## Known limitations

Quartet supports trees with up to 477 leaves.  Larger trees contain more 
quartets than can be represented by R's signed 32-bit integers.

The underlying 'tqDist' library may handle trees with up to 568 leaves,
and 64-bit integer representations could increase this number further.  
Making either of these improvements within the R package 
would require substantial additional work, but could be implemented -- do
[file an issue](http://github.com/ms609/Quartet/issues/new/) if this would be
useful to you.

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
  informative trees from simulated morphological datasets. Biol. Lett.
  15:20180632. https://doi.org/10.1098/rsbl.2018.0632

- Steel, M. and Penny, D. Distributions of tree comparison metrics: some new results.
  Syst. Biol. (1993) 42 (2): 126-141. https://doi.org/10.1093/sysbio/42.2.126

Please note that the 'Quartet' project is released with a
[Contributor Code of Conduct](CODE_OF_CONDUCT.md).
By contributing to this project, you agree to abide by its terms.
