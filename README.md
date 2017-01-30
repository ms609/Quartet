# Tetrad

Tetrad is an R package that calculates the similarity of two trees based on the number of shared four-taxon subtrees.
(This is as far as I know a novel approach to calculating tree similarity.)

The package will soon be compiled and uploaded to the CRAN repository.  
Meanwhile, you can install the latest version of the package into R thus:

```r
# Install the devtools package from CRAN
install.packages('devtools')

# Install the inapplicable package from github
devtools::install_github('ms609/Tetrad')

# Load the package into R
library('Tetrad')
```

You will need Rtools installed (https://cran.r-project.org/bin/windows/Rtools/) to allow R to build the package.

## References
Smith, M.R. (forthcoming)
