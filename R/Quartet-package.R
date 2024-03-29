#' Quartet
#' 
#' '[Quartet](https://ms609.github.io/Quartet/)' is an R package that calculates 
#' the quartet distance between two trees (Estabrook _et al._ 1985),
#' a measure of their similarity based on the number of shared four-taxon subtrees.
#' 
#' The quartet distance outperforms a number of widely used tree distances 
#' (e.g. the Robinson–Foulds, path, and rearrangement distances) against a number
#' theoretical and practical measures (Steel & Penny 1993; Smith 2020),
#' and is particularly valuable in the construction of tree spaces (Smith 2021).
#' 
#' 'Quartet' uses the 'tqDist' algorithm (Brodal _et al._ 2004; Sand _et al._ 2014).
#' Unlike many other implementations, it distinguishes between quartets that are
#' contradicted by one tree,
#' and quartets that are simply absent due to a lack of resolution (i.e.
#' the presence of polytomies; see Smith 2019).
#' 'Quartet' makes this distinction in both the quartet metric (function
#' `QuartetStatus()`) and the partition metric (i.e. Robinson-Foulds distance;
#' function `SplitStatus()`).
#'  
#' ## Using Quartet
#' 
#' View the [function reference](https://ms609.github.io/Quartet/reference/)
#' and [basic usage instructions](https://ms609.github.io/Quartet/articles/Using-Quartet.html).
#' 
#' ## Known limitations
#' 
#' Quartet supports trees with up to 477 leaves.  Larger trees contain more 
#' quartets than can be represented by R's signed 32-bit integers.
#' 
#' The underlying 'tqDist' library may handle trees with up to 568 leaves,
#' and 64-bit integer representations could increase this number further.  
#' Making either of these improvements within the R package 
#' would require substantial additional work, but could be implemented -- do
#' [file an issue](https://github.com/ms609/Quartet/issues/new/) if this would
#' be useful to you.
#' 
#' @references
#' - Brodal G.S., Fagerberg R., Pedersen C.N.S. 2004. Computing the quartet 
#'   distance between evolutionary trees in time O(_n_ log _n_). 
#'   Algorithmica. 38:377–395.
#' 
#' - Estabrook G.F., McMorris F.R., Meacham C.A. 1985. Comparison of undirected 
#'   phylogenetic trees based on subtrees of four evolutionary units. 
#'   Syst. Zool. 34:193–200.
#' 
#' - Sand A., Holt M.K., Johansen J., Brodal G.S., Mailund T., Pedersen C.N.S. 2014.
#'   tqDist: a library for computing the quartet and triplet distances between 
#'   binary or general trees. 
#'   Bioinformatics. 30:2079–2080. https://doi.org/10.1093/bioinformatics/btu157
#' 
#' - Smith, M.R. 2019. Bayesian and parsimony approaches reconstruct 
#'   informative trees from simulated morphological datasets. Biol. Lett.
#'   15:20180632. https://doi.org/10.1098/rsbl.2018.0632
#' 
#' - Smith, M.R. 2020. Information theoretic generalized Robinson–Foulds metrics 
#'   for comparing phylogenetic trees. Bioinformatics 36:5007–5013.
#'   https://dx.doi.org/10.1093/bioinformatics/btaa614/5866976
#' 
#' - Smith, M.R. 2022. Robust analysis of phylogenetic tree space. 
#'   Syst. Biol. 71: 1255-1270.
#'   https://dx.doi.org/10.1093/sysbio/syab100
#' 
#' - Steel, M. and Penny, D. 1993. Distributions of tree comparison metrics:
#'   some new results. Syst. Biol. 42: 126-141.
#'   https://doi.org/10.1093/sysbio/42.2.126
#'   
#' @keywords internal
"_PACKAGE"

# The following block is used by usethis to automatically manage
# roxygen namespace tags. Modify with care!
## usethis namespace: start
## usethis namespace: end
NULL
