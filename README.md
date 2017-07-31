# SlowQuartet

SlowQuartet is an R package that calculates the Quartet distance between two trees:
a measure of their similarity based on the number of shared four-taxon subtrees.

It's named Slow Quartet as it calculates the metric rather inefficiently; it may not be of use for large trees. 
Faster algorithms are available: try the R package tqDist.

The package will soon be compiled and uploaded to the CRAN repository.  
Meanwhile, you can install the latest version of the package into R thus:

```r
# Install the devtools package from CRAN
install.packages('devtools')

# Install the inapplicable package from github
devtools::install_github('ms609/SlowQuartet')

# Load the package into R
library('SlowQuartet')
```

You will need Rtools installed (https://cran.r-project.org/bin/windows/Rtools/) to allow R to build the package.

## References
ESTABROOK, G. F., F. R. MCMORRIS, AND C. A. MEACHAM. 1985. Comparison of undirected phylogenetic trees based on subtrees of four evolutionary units. Syst. Zool. 34:193-200.

Andreas Sand, Morten K. Holt, Jens Johansen, Gerth Stølting Brodal, Thomas Mailund and Christian N.S. Pedersen; tqDist: A Library for Computing the Quartet and Triplet Distances Between Binary or General Trees, Bioinformatics, 2014, 30 (14): 2079-2080, doi: 10.1093/bioinformatics/btu157

Steel, M. and Penny, D. Distributions of tree comparison metrics: some new results. Syst. Biol. (1993) 42 (2): 126-141. https://doi.org/10.1093/sysbio/42.2.126

Gerth Stølting, Brodal Rolf Fagerberg, Christian N.S. Pedersen. Computing the Quartet Distance between Evolutionary Trees in Time O(n log n) Algorithmica 38:2, pp 377–395

Smith, M.R. (forthcoming)
