#' '[Quartet](https://ms609.github.io/Quartet/)' is an R package that calculates
#'  the Quartet distance between two trees: a measure of their similarity based
#'  on the number of shared four-taxon subtrees.
#' 
#' The package uses the `tqDist` algorithm.  Unlike many other implementations,
#' it distinguishes between quartets that are contradicted by one tree,
#' and quartets that are simply absent due to a lack of resolution (i.e. the presence
#' of polytomies).  'Quartet' makes this distinction in both the quartet metric
#' (function [`QuartetStatus()`]) and the partition metric (i.e. Robinson-Foulds
#'  distance; function [`SplitStatus()`]).
#' 
#' ## Known limitations
#' 
#' Quartet supports trees with up to 477 leaves.  Larger trees contain more 
#' quartets than can be represented by R's signed 32-bit integers.
#' 
#' 'tqDist' may handle trees with up to 568 leaves (not tested), and 64-bit 
#' integer representations could increase this number further.  Either of these
#' would require substantial additional work, but could be implemented -- do
#' [file an issue](https://ms609.github.io/Quartet/issues) if this would be useful
#' to you.
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
#' - Robinson D.F., Foulds L.R. 1981. Comparison of phylogenetic trees. 
#'   Math. Biosci. 53:131–147.
#' 
#' - Sand A., Holt M.K., Johansen J., Brodal G.S., Mailund T., Pedersen C.N.S. 2014.
#'   tqDist: a library for computing the quartet and triplet distances between 
#'   binary or general trees. 
#'   Bioinformatics. 30:2079–2080. https://doi.org/10.1093/bioinformatics/btu157
#' 
#' - Smith, M.R. (2019) Bayesian and parsimony approaches reconstruct 
#'   informative trees from simulated morphological datasets. Biol. Lett.
#'   15:20180632. https://doi.org/10.1098/rsbl.2018.0632
#' 
#' - Steel, M. and Penny, D. Distributions of tree comparison metrics: some new results.
#'   Syst. Biol. (1993) 42 (2): 126-141. https://doi.org/10.1093/sysbio/42.2.126
#'   
#' @keywords internal
"_PACKAGE"

# The following block is used by usethis to automatically manage
# roxygen namespace tags. Modify with care!
## usethis namespace: start
## usethis namespace: end
NULL
