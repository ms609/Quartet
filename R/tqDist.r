#' Status of quartets
#' 
#' Determines the number of quartets that are consistent within pairs of trees.
#' 
#' Given a list of trees, returns the number of quartet statements 
#' \insertCite{Estabrook1985}{Quartet} present in the
#' reference tree (the first entry in `trees`, if `cf` is not specified)
#' that are also present in each other tree.  A random pair of fully resolved 
#' trees is expected to share `choose(n_tip, 4) / 3` quartets.
#' 
#' If trees do not bear the same number of tips, `SharedQuartetStatus()` will 
#' consider only the quartets that include taxa common to both trees.
#' 
#' From this information it is possible to calculate how many of all possible
#' quartets occur in one tree or the other, though there is not yet a function
#' calculating this; [let us know](https://github.com/ms609/Quartet/issues/new)
#' if you would appreciate this functionality.
#' 
#' The status of each quartet is calculated using the algorithms of
#' \insertCite{Brodal2013;textual}{Quartet} and 
#' \insertCite{Holt2014;textual}{Quartet}, implemented in the
#' tqdist C library \insertCite{Sand2014}{Quartet}.
#' 
#' @template treesParam
#' @template treesCfParam
#' @param nTip Integer specifying number of tips that could have occurred
#' in `trees`.  Useful if comparing trees from different data sources that
#' contain non-overlapping tips.
#' If `NULL`, the default, then trees are assumed to contain the same tips.
#' If `TRUE`, then a vector is generated automatically by counting all unique
#' tip labels found in `trees` or `cf`.
#' 
#' @templateVar intro `QuartetStatus()` returns a two dimensional array. Rows correspond to the input trees; the first row will report a perfect match if the first tree is specified as the comparison tree (or if `cf` is not specified).  Columns list the status of each quartet:
#' @template returnEstabrook
#'         
#' @template MRS
#' 
#' @examples
#' data("sq_trees")
#' # Calculate the status of each quartet relative to the first entry in 
#' # sq_trees
#' sq_status <- QuartetStatus(sq_trees)
#' 
#' # Calculate the status of each quartet relative to a given tree
#' two_moved <- sq_trees[5:7]
#' sq_status <- QuartetStatus(two_moved, sq_trees$ref_tree)
#' 
#' # Calculate Estabrook et al's similarity measures:
#' SimilarityMetrics(sq_status)
#' 
#' # Compare trees that include a subset of the taxa 1..10
#' library("TreeTools", quietly = TRUE, warn.conflict = FALSE)
#' QuartetStatus(BalancedTree(1:5), BalancedTree(3:8), nTip = 10)
#' 
#' # If all taxa studied occur in `trees` or `cf`, set `nTip = TRUE`
#' QuartetStatus(BalancedTree(1:5), BalancedTree(3:10), nTip = TRUE)
#'  
#' @family element-by-element comparisons
#' @seealso 
#' - Use splits (groups/clades defined by nodes or edges of the tree) instead
#'   of quartets as the unit of comparison: [`SplitStatus()`].
#'  
#' - Generate distance metrics from quartet statuses: [`SimilarityMetrics()`].
#' 
#' @references \insertAllCited{}
#'
#' @importFrom Rdpack reprompt
#' @importFrom ape keep.tip
#' @importFrom TreeTools AllTipLabels TipLabels
#' @name QuartetStatus
#' @export
QuartetStatus <- function (trees, cf = trees[[1]], nTip = NULL) {
  if (is.null(nTip)) {
    SingleTreeQuartetAgreement(trees, comparison = cf)
  } else {
    if (isTRUE(nTip)) {
      nTip <- length(AllTipLabels(c(list(cf), c(trees))))
    }
    Q <- choose(nTip, 4)
    status <- vapply(c(trees), function (x) {
      commonLabels <- intersect(TipLabels(x), TipLabels(cf))
      resolvedX <- ResolvedQuartets(x)
      resolvedCf <- ResolvedQuartets(cf)
      if (length(commonLabels) > 3L) {
        reducedX <- keep.tip(x, commonLabels)
        reducedCf <- keep.tip(cf, commonLabels)
        resolvedReducedX <- ResolvedQuartets(reducedX)
        resolvedReducedCf <- ResolvedQuartets(reducedCf)
        commonStatus <- SingleTreeQuartetAgreement(reducedX, reducedCf)
        quartetsIn1Only <- resolvedX - c(sum(commonStatus[, c("s", "d", "r1")]),
                                         sum(commonStatus[, c("r2", "u")]))
        quartetsIn2Only <- resolvedCf - c(sum(commonStatus[, c("s", "d", "r2")]),
                                          sum(commonStatus[, c("r1", "u")]))
        
        commonStatus[, c("r1", "u")] <- commonStatus[, c("r1", "u")] + quartetsIn1Only
        commonStatus[, c("r2", "u")] <- commonStatus[, c("r2", "u")] + quartetsIn2Only
        commonStatus[, "u"] <- commonStatus[, "u"] + Q - sum(resolvedX, resolvedCf, -resolvedReducedX)
        commonStatus
      } else {
        c(0, 0, 0, 0, resolvedX[1], resolvedCf[1], Q - resolvedX[1] - resolvedCf[1])
      }
    }, c("N" = 0, "Q" = 0, "s" = 0, "d" = 0, "r1" = 0, "r2" = 0, "u" = 0))
    status[c("N", "Q"), ] <- c(Q + Q, Q)
    
    # Return:
    t(status)
  }
}

#' Wrapper for tqDist
#' 
#' `TQDist()` and `TQAE()` are convenience functions that writes a list of
#' trees to text files that can be processed by the C implementation of tqDist 
#' \insertCite{Sand2014}{Quartet}.
#' tqDist is then called, and the temporary file is deleted when analysis is
#' complete.
#' 
#' Quartets can be resolved in one of five ways, which
#' \insertCite{Brodal2013;textual}{Quartet} and
#' \insertCite{Holt2014;textual}{Quartet} distinguish using the letters A-E,
#' and \insertCite{Estabrook1985;textual}{Quartet} refer to as:
#'  
#' \describe{
#'  \item{A}{_s_ = resolved the **s**ame in both trees;}
#'  \item{B}{_d_ = resolved **d**ifferently in both trees;}
#'  \item{C}{_r1_ = **r**esolved only in tree **1**;}
#'  \item{D}{_r2_ = **r**esolved only in tree **2** (the comparison tree);}
#'  \item{E}{_u_ = **u**nresolved in both trees.}
#' }
#'  
#' 
#' @param trees List of phylogenetic trees, of class \code{list} or
#'                 \code{\link[ape:read.tree]{multiPhylo}}.
#' @return `TQDist()` returns the quartet distance between each pair of trees.
#' 
#' @references \insertAllCited{}
#' 
#' @seealso [`CompareQuartets()`], [`QuartetStatus()`]
#' 
#' @importFrom ape write.tree
#' @template MRS
#' @useDynLib Quartet, .registration = TRUE
#' @export
TQDist <- function (trees) {
  .CheckSize(trees)
  .Call("_Quartet_tqdist_AllPairsQuartetDistanceEdge", .TreeToEdge(trees))
}

#' @rdname TQDist
#' @return `TQAE()` returns the number of resolved quartets in agreement between
#' each pair of trees ("A" in Brodal _et al_. 2013) and the number of quartets
#' that are unresolved in both trees ("E" in Brodal _et al_. 2013).
#' @export 
TQAE <- function (trees) {
  .CheckSize(trees)
  result <- .Call("_Quartet_tqdist_AllPairsQuartetAgreementEdge",
                  .TreeToEdge(trees))
  nTrees <- nrow(result)
  array(result, c(nTrees, nTrees, 2), dimnames = list(NULL, NULL, c("A", "E")))
}

#' Check tree size
#' 
#' Trees with > 477 leaves may have counts > .Machine$integer.max so cannot 
#' be reliably evaluated.
#' 
#' It may be possible to increase this number to 568 by converting what R 
#' represents as negative integers to the unsigned equivalent that is sent 
#' from C.
#' 
#' @keywords internal
#' @export
.CheckSize <- function (tree) UseMethod(".CheckSize")

#' @rdname dot-CheckSize
#' @keywords internal
#' @export
.CheckSize.phylo <- function (tree) {
  if (length(tree$tip.label) > 477L) {
    warning("Trees with > 477 tips may produce integer overflow errors.")
  }
}

#' @rdname dot-CheckSize
#' @export
#' @keywords internal
.CheckSize.list <- function (tree) {
  lapply(tree, .CheckSize)
}

#' @rdname dot-CheckSize
#' @export
#' @keywords internal
.CheckSize.multiPhylo  <- .CheckSize.list

#' @describeIn QuartetStatus Agreement of each quartet, comparing each pair of
#' trees in a list.
#' @return `ManyToManyQuartetAgreement()` returns a three-dimensional array 
#' listing, for each pair of trees in turn, the number of quartets in each
#' category.
#' @examples 
#' # Calculate Quartet Divergence between each tree and each other tree in a 
#' # list
#' QuartetDivergence(ManyToManyQuartetAgreement(two_moved))
#' @importFrom TreeTools PairwiseDistances
#' @export 
ManyToManyQuartetAgreement <- function (trees, nTip = NULL) {
  treeNames <- names(trees)
  dimNames <- list(treeNames, treeNames, c("N", "Q", "s", "d", "r1", "r2", "u"))
  if (is.null(nTip)) {
    AE <- TQAE(trees)
    nTree <- dim(AE)[1]
    A   <- AE[, , 1]
    E   <- AE[, , 2]
    ABD <- matrix(diag(A), nTree, nTree)
    CE  <- matrix(diag(E), nTree, nTree)
    DE  <- t(CE)
    C   <- CE - E
    D   <- DE - E
    B   <- ABD - A - D
    Q   <- ABD + CE
    N   <- Q + Q
    
    # Return:
    array(c(N, Q, A, B, C, D, E), dim = c(nTree, nTree, 7),
          dimnames = dimNames)
  } else {
    if (isTRUE(nTip)) nTip <- length(AllTipLabels(trees))
    treeNames <- names(trees)
    ret <- vapply(PairwiseDistances(trees, QuartetStatus, 7, nTip = nTip),
                  as.matrix, matrix(0, length(trees), length(trees), 
                                    dimnames = dimNames[1:2]))
    dimnames(ret)[[3]] <- c("N", "Q", "s", "d", "r1", "r2", "u")
    
    diag(ret[, , "N"]) <- 2L * choose(nTip, 4)
    diag(ret[, , "Q"]) <- choose(nTip, 4)
    
    resolved <- vapply(trees, ResolvedQuartets, double(2))
    diag(ret[, , "s"]) <- resolved[1, ]
    diag(ret[, , "u"]) <- choose(nTip, 4) - resolved[1, ]
    
    swapTri <- lower.tri(ret[, , "r1"])
    tmp <- ret[, , "r1"][swapTri]
    ret[, , "r1"][swapTri] <- ret[, , "r2"][swapTri]
    ret[, , "r2"][swapTri] <- tmp

    # Return:
    ret
  }
}

#' @describeIn QuartetStatus Agreement of each quartet in trees in one list with
#' each quartet in trees in a second list.
#' @param trees1,trees2 List or `multiPhylo` objects containing
#'   trees of class `phylo`.
#' @return `TwoListQuartetAgreement()` returns a three-dimensional array listing,
#'   for each pair of trees in turn, the number of quartets in each category.
#' @examples 
#' # Calculate Quartet Divergence between each tree in one list and each 
#' # tree in another
#' QuartetDivergence(TwoListQuartetAgreement(sq_trees[1:3], sq_trees[10:13]))
#' @export
TwoListQuartetAgreement <- function (trees1, trees2) {
  aperm(vapply(trees2, function (cf) SingleTreeQuartetAgreement(trees1, cf),
         matrix(0L, length(trees1), 7)), c(1, 3, 2))
  
}

#' @describeIn QuartetStatus Agreement of each quartet in trees in a list with
#' the quartets in a comparison tree.
#' @param comparison A tree of class \code{\link[ape:read.tree]{phylo}} against
#' which to compare `trees`.
#' @return `SingleTreeQuartetAgreement()` returns a two-dimensional array listing,
#'   for tree in `trees`, the total number of quartets and the 
#'   number of quartets in each category.  
#'   The `comparison` tree is treated as `tree2`.
#' @export 
SingleTreeQuartetAgreement <- function (trees, comparison) {
  .CheckSize(trees)
  if (inherits(trees, "phylo")) trees <- list(trees)
  
  comparison <- Preorder(comparison)
  trees[] <- lapply(trees, Preorder)
  
  rq <- ResolvedQuartets(comparison)
  DE <- vapply(trees, ResolvedQuartets, integer(2))[2, ]
  
  AE <- matrix(.Call("_Quartet_tqdist_OneToManyQuartetAgreementEdge",
                     .TreeToEdge(comparison),
                     .TreeToEdge(trees, comparison$tip.label)),
               ncol = 2L, dimnames = list(NULL, c("A", "E")))
  
  A   <- AE[, 1]
  E   <- AE[, 2]
  ABD <- rq[1]
  CE <-  rq[2]
  C   <- CE - E
  D   <- DE - E
  
  B   <- ABD - A - D
  Q   <- sum(ABD, CE)
  
  nTree <- length(DE)
  
  # Return:
  array(c(rep(2L * Q, nTree), rep(Q, nTree), A, B, C, D, E), dim=c(nTree, 7L),
        dimnames = list(names(trees), c("N", "Q", "s", "d", "r1", "r2", "u")))
}

#' tqDist file generator
#' 
#' Creates a temporary file corresponding to a list of trees,
#' to be processed with tqDist.  Files should be destroyed using
#' `on.exit(file.remove(fileName))` by the calling function.
#' 
#' Should now only be necessary for testing purposes.
#' 
#' @return Name of the created file
#' @keywords internal
#' @export
TQFile <- function (treeList) {
  if (inherits(treeList, "list")){
    class(treeList) <- "multiPhylo"
  }
  if (!inherits(treeList, c("phylo", "multiPhylo"))) {
    stop("treeList must be a tree of class phylo, or a list of phylogenetic trees")
  }
  fileName <- tempfile()
  write.tree(treeList, file = fileName)
  
  # Return:
  fileName
}

#' Direct entry points to "tqDist" functions
#' 
#' Wrappers for functions in "tqDist", which calculate triplet and quartet
#' distances between pairs of trees.
#' 
#' @param file,file1,file2 Paths to files containing a tree or trees in Newick
#'  format, possibly created using [`TQFile()`].
#' 
#' @return `...Distance()` functions return the distance between the requested trees.
#'  
#'  `...Agreement()` functions return the number of triplets or quartets that are:
#'  * `A`, resolved in the same fashion in both trees;
#'  * `E`, unresolved in both trees.
#'  
#' Comparing a tree against itself yields the totals (`A+B+C`) and (`D+E`) 
#' referred to by \insertCite{Brodal2013;textual}{Quartet} and 
#' \insertCite{Holt2014;textual}{Quartet}.
#' 
#' @author 
#'   * Algorithms: \insertCite{Brodal2013;textual}{Quartet}; 
#' \insertCite{Holt2014;textual}{Quartet}.
#' 
#'   * C implementation: \insertCite{Sand2014;textual}{Quartet}; 
#'   modified for portability by Martin R. Smith.
#'   
#'   * R interface: Martin R. Smith.
#' 
#' @seealso 
#' * [`QuartetStatus()`] takes trees, rather than files, as input.
#' * [`TQFile()`] creates a temporary file containing specified trees.
#' 
#' 
#' @references \insertAllCited{}
#' 
#' @concept Tree distances
#' @name Distances
NULL

#' @describeIn Distances Returns the quartet distance between the tree.
#' in `file1` and the tree in `file2`.
#' @export
QuartetDistance <- function(file1, file2) {
  ValidateQuartetFile(file1)
  ValidateQuartetFile(file2)
  .Call("_Quartet_tqdist_QuartetDistance",
        as.character(file1), as.character(file2));
}

#' @describeIn Distances Returns a vector of length two, listing \[1\]
#' the number of resolved quartets that agree (`A`);
#' \[2\] the number of quartets that are unresolved in both trees (`E`).
#' See \insertCite{Brodal2013;textual}{Quartet}.
#'  
#' @export
QuartetAgreement <- function(file1, file2) {
  ValidateQuartetFile(file1)
  ValidateQuartetFile(file2)
  .Call("_Quartet_tqdist_QuartetAgreement",
        as.character(file1), as.character(file2));
}

#' @importFrom ape read.tree
#' @describeIn Distances Quartet distance between the tree on each line of `file1`
#'   and the tree on the corresponding line of `file2`.
#' @export
PairsQuartetDistance <- function(file1, file2) {
  ValidateQuartetFile(file1)
  ValidateQuartetFile(file2)
  trees1 <- read.tree(file1)
  trees2 <- read.tree(file2)
  if (length(trees1) != length(trees2) || !inherits(trees1, class(trees2)[1])) {
    stop("file1 and file2 must contain the same number of trees")
  }
  .Call("_Quartet_tqdist_PairsQuartetDistance", 
        as.character(file1), as.character(file2));
}

#' @export
#' @importFrom ape read.tree
#' @describeIn Distances Quartet distance between the tree in 
#'  `file1` and the tree on each line of `file2`.
OneToManyQuartetAgreement <- function(file1, file2) {
  ValidateQuartetFile(file1)
  ValidateQuartetFile(file2)
  trees1 <- read.tree(file1)
  trees2 <- read.tree(file2)
  if (!inherits(trees1, "phylo")) {
    stop("file1 must contain a single tree")
  }
  if (length(trees2) < 1) {
    stop("file2 must contain at least one tree")
  }
  matrix(.Call("_Quartet_tqdist_OneToManyQuartetAgreement", 
               as.character(file1), as.character(file2)),
         ncol = 2, dimnames = list(NULL, c("A", "E")))
}

#' @export
#' @describeIn Distances Quartet distance between each tree listed in `file` and 
#'   each other tree therein.
AllPairsQuartetDistance <- function(file) {
  ValidateQuartetFile(file)
  .Call("_Quartet_tqdist_AllPairsQuartetDistance", as.character(file));
}

#' @keywords internal
.TreeToEdge <- function (trees, tipOrder) UseMethod(".TreeToEdge")

#' @keywords internal
.TreeToEdge.list <- function (trees, tipOrder = trees[[1]]$tip.label) {
  lapply(trees, .SortTree, tipOrder)
}
#' @keywords internal
.TreeToEdge.multiPhylo <- .TreeToEdge.list

#' @keywords internal
#' @importFrom TreeTools RenumberTips RenumberTree
.TreeToEdge.phylo <- function (trees, tipOrder = NULL) {
  if (is.null(tipOrder)) {
    edge <- trees$edge
    RenumberTree(edge[, 1], edge[, 2])
  } else {
    .SortTree(trees, tipOrder)
  }
}

#' @importFrom TreeTools RenumberTips RenumberTree
#' @keywords internal
.SortTree <- function (tree, tipOrder) {
  edge <- RenumberTips(tree, tipOrder)$edge
  RenumberTree(edge[, 1], edge[, 2])
}

#' @export
#' @describeIn Distances Quartet status for each pair of trees in `file`.
AllPairsQuartetAgreement <- function(file) {
  ValidateQuartetFile(file)
  result <- .Call("_Quartet_tqdist_AllPairsQuartetAgreement", as.character(file));
  nTrees <- nrow(result)
  array(result, c(nTrees, nTrees, 2), dimnames=list(NULL, NULL, c("A", "E")))
}

#' @export
#' @describeIn Distances Triplet distance between the single tree given 
#'   in each file.
TripletDistance <- function(file1, file2) {
  ValidateQuartetFile(file1)
  ValidateQuartetFile(file2)
  .Call("_Quartet_tqdist_TripletDistance", as.character(file1), as.character(file2));
}

#' @export
#' @describeIn Distances Triplet distance between the tree on each line of `file1`
#'   and the tree on the corresponding line of `file2`.
PairsTripletDistance <- function(file1, file2) {
  ValidateQuartetFile(file1)
  ValidateQuartetFile(file2)
  trees1 <- read.tree(file1)
  trees2 <- read.tree(file2)
  if (length(trees1) != length(trees2) || !inherits(trees1, class(trees2)[1])) {
    stop("file1 and file2 must contain the same number of trees")
  }
  .Call("_Quartet_tqdist_PairsTripletDistance", as.character(file1), as.character(file2));
}

#' @export
#' @describeIn Distances Triplet distance between each tree listed in `file` and 
#'   each other tree therein.
AllPairsTripletDistance <- function(file) {
  ValidateQuartetFile(file)
  .Call("_Quartet_tqdist_AllPairsTripletDistance", as.character(file));
}

#' Validate filenames
#' 
#' Verifies that file parameters are character strings describing files that exist
#' 
#' @param file Variable to validate
#' 
#' @return `TRUE` if `file` is a character vector of length one describing 
#'   a file that exists, a fatal error otherwise.
#' 
#' @template MRS
#' 
#' @export
#' @keywords internal
ValidateQuartetFile <- function (file) {
  if (length(file) != 1) {
    stop("file must be a character vector of length one")
  }
  if (!file.exists(file)) {
    stop("file ", file, " not found")
  }
}
