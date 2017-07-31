# SlowQuartet

SlowQuartet is an R package that calculates the Quartet distance between two trees:
a measure of their similarity based on the number of shared four-taxon subtrees.

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

Steel, M. and Penny, D. Distributions of tree comparison metrics: some new results. Syst. Biol. (1993) 42 (2): 126-141. https://doi.org/10.1093/sysbio/42.2.126

Smith, M.R. (forthcoming)
