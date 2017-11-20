WHICH_OTHER_NODE <- 2:4

#' Tree2Splits
#' Converts a phylogenetic tree to an array of bipartition splits.
#' 
#' @param tr A tree of class \code{\link[ape]{phylo}}, with tips bearing integer labels (i.e. tr$tip.label == 1:N).
#' @return Returns a two-dimensional array, with columns corresponding to bipartitions and rows corresponding
#' to tips 1:N.
#'
#' @author Martin R. Smith
#' 
#' @seealso \code{phangorn:::bip}
#'
#' @examples Tree2Splits(ape::rtree(6, tip.label=1:6, br=NULL))
#'
#' @export
Tree2Splits <- function (tr) {
  tip_label <- tr$tip.label
  n_tip <- length(tip_label)
  root <- length(tip_label) + 1
  vapply(phangorn:::bip(tr)[-seq_len(root)], function (x) seq_len(n_tip) %in% x, logical(n_tip))[as.double(tip_label), , drop=FALSE]
}

#' Number Tips
#' Renumber the tips of a tree to match a list of labels
#' @param tr A tree of class phylo.
#' @param sorted.labels A character vector listing tip labels in the desired order
#' @export
#' @keywords internal
#' @author Martin R. Smith
NumberTips <- function (tr, sorted.labels) {
  tr$tip.label <- match(tr$tip.label, sorted.labels)
  return(tr)
}

#' Column Sums
#' An accellerated version of the R function \code{colSums(x, na.rm = FALSE, dims = 1L)}.
#' Using this function makes \code{MatchingQuartets} 8% faster.
#' But it is \emph{very} naughty to call \code{.Internal}, so I use the 
#' internal R colSums function instead.
#' @param x Matrix whose columns are to be summed.
#' @param n_cols Number of columns in said matrix.
#' @author Martin R. Smith
#' @keywords internal
# ColSums <- function (x, n_cols = ncol(x)) .Internal(colSums(x, 4, n_cols, FALSE))


#' Choices
#'
#'  List all choices of four taxa from a tree.
#'  A more computationally efficient alternative to \code{\link{combn}}.
#'  Uses \code{\link{memoise}} to make repeated calls faster.
#'
#' @param n_tips Integer, specifying the number of tips in a tree.
#' @return Returns a list of length \code{choose(n_tips, 4)}, with each entry 
#' corresponding to a unique selection of four different integers <= n_tips
#' @author Martin R. Smith
#'
#' @ seealso \code{\link{combn}}
#' @examples{
#'  n_tips <- 6
#'  choice_list <- Choices(n_tips)
#'  choice_list
#'  combn(n_tips, 4) # Provides the same information, but for large 
#'                   # values of n_tips is significantly slower.
#' }
#' @importFrom memoise memoise
#' @export
Choices <- memoise(function (n_tips) {
  ret <- unlist(lapply(seq_len(n_tips - 3), function (i) {
    unlist(lapply((i + 1):(n_tips - 2), function (j) {
      unlist(lapply((j + 1):(n_tips - 1), function (k) {
        lapply((k + 1):n_tips, function (l) {
          c(i, j, k, l)
        })
      }), recursive=FALSE)
    }), recursive=FALSE)
  }), recursive=FALSE)
})

#' Quartet State
#' State of quartets
#'
#' Report the status of a given tetrad.
#' @param tips A four-element array listing the tips of the four-taxon tree (tetrad).
#' @param bips bipartitions to evaluate.
#'
#'
#'  One of the three possible four-taxon trees will be consistent with any set of bipartitions 
#'  generated from a fully resolved tree.  If the taxa are numbered 1 to 4, this tree can be 
#'  identified by naming the sister taxon to taxon 1.
#'  If a set of bipartitions is generated from a tree that contains polytomies, it is possible
#'  that all three three four-taxon trees are consistent with the set of bipartitions.
#'
#' @return Returns 0 if the relationships of the four taxa are not constrained by the provided 
#' bipartitions, or the index of the closest relative to tips[1], otherwise.
#'
#' @author Martin R. Smith
#' @seealso \code{\link{CompareQuartets}}, used to compare sets of bipartitions
#' @examples{
#'   n_tip <- 6
#'   trees <- list(ape::rtree(n_tip, tip.label=seq_len(n_tip), br=NULL),
#'                 ape::rtree(n_tip, tip.label=seq_len(n_tip), br=NULL))
#'   splits <- lapply(trees, Tree2Splits)
#'   QuartetState(c(1, 3, 4, 6), splits[[2]])  
#'   QuartetState(1:4, splits[[1]]) == QuartetState(1:4, splits[[2]])
#'   vapply(Choices(n_tip), QuartetState, bips=splits[[1]], double(1))
#' }
#' @export
QuartetState <- function (tips, bips) {
  tetra_splits <- bips[tips, , drop=FALSE]
  statement <- tetra_splits[, colSums(tetra_splits) == 2, drop=FALSE]
  if (length(statement)) {
    statement <- statement[, 1]
    if (statement[1]) return (WHICH_OTHER_NODE[statement[WHICH_OTHER_NODE]])
    if (statement[2]) if (statement[3]) return (4) else return (3)
    return (2)
  } else {
    return (0)
  }
}

#' describeIn QuartetStates A wrapper that need only be provided with a list of splits
#' @param splits a list of bipartition splits, perhaps generated using 
#' \code{\link{Tree2Splits}}.
#' @author Martin R. Smith
#' @export
QuartetStates <- function (splits) {
  n_tips <- dim(splits[[1]])[1]
  lapply(splits, function (bips) {
    vapply(Choices(n_tips), QuartetState, double(1), bips=bips)
  })
}

#' Compare Quartets
#' Compare tetrad states between trees
#'
#'  Compares two lists of quartet states, detailing how many are identical and 
#'  how many are unresolved.
#' 
#' @param x A list of tetrad states, perhaps generated in
#'  \code{\link{CompareQuartets}}.
#' @param cf a second such list.
#'
#' Compares each tetrad in a list, calculating how many statements are identical
#'  in both lists.
#' @return {
#'   Returns an array of two elements:
#'   1: the number of quartets that are present in both trees
#'   2: the number of quartets that are not resolved in both trees
#'   The number of quartets that are present in one tree and contradicted in the
#'   other can be calcluated by deducting the total from the total number of 
#'   quartets, \code{choose(n_tip, 4)}.
#' }
#'
#' @author Martin R. Smith
#'
#' @seealso \code{\link{MatchingQuartets}}, generates this output from a list of
#'  trees.
#'
#' @examples{
#'   n_tip <- 6
#'   trees <- list(ape::rtree(n_tip, tip.label=seq_len(n_tip), br=NULL),
#'                 ape::rtree(n_tip, tip.label=seq_len(n_tip), br=NULL))
#'   splits <- lapply(trees, Tree2Splits)
#'   quartets <- QuartetStates(splits)
#'   compare_result <- CompareQuartets(quartets[[1]], quartets[[2]])
#'   dissimilar_quartets <- choose(n_tip, 4) - sum(compare_result)  
#'   result <- c(compare_result, dissimilar_quartets)
#'   names(result) <- c('Shared', 'Unresolved', 'Dissimilar')
#'   result
#' }
#' @export
CompareQuartets <- function (x, cf) {
  resolved <- as.logical(x) & as.logical(cf)
  c(sum(x[resolved] == cf[resolved]), sum(!resolved))
}

#' Matching Quartets
#' Count matching quartets
#' Determines the number of four-taxon trees consistent with multiple cladograms
#' 
#' Given a list of trees, returns the nubmer of quartet statements present in the first tree
#' in the list also present in each other tree.
#'
#' 
#'   At present the trees must bear the same number of tips, and each tree$tip.label must use
#'   the integers 1:n_tip.  Support for different-sized trees will be added if there is demand; 
#'   contact the author if you would appreciate this functionality.
#' 
#' @param trees A list of trees of class \code{\link[ape]{phylo}}, with identically-labelled tips.
#'
#' @return Returns a two dimensional array; columns correspond to the input trees;
#'       rows report the number of four-taxon trees that : 1, are present in 
#'       \code{trees[[1]]} and the corresponding input tree;
#'       2: are unresolved in (at least) one of trees[[1]] and the corresponding 
#'       input tree. Tetrads that DIFFER between the two relevant trees can be 
#'       calculated by deducting the quartets in either of the other two
#'       categories from the total number of quartets, given by
#'        \code{\link{choose}(n_tip, 4)}.
#'       
#'       A random pair of fully-resolved trees is expected to share 
#'       \code{choose(n_tip, 4) / 3} four-taxon trees.
#'
#' @author Martin R. Smith
#' @examples{
#'  n_tip <- 6
#'  trees <- lapply(1:12, function (x) ape::rtree(n_tip, tip.label=seq_len(n_tip), br=NULL))
#'  compare_result <- MatchingQuartets(trees)
#'  dissimilar_quartets <- choose(n_tip, 4) - colSums(compare_result)  
#'  result <- rbind(compare_result, dissimilar_quartets)
#'  rownames(result) <- c('Shared', 'Unresolved', 'Dissimilar')
#'  result
#' }
#' @export
MatchingQuartets <- function (trees) {
  tree1.labels <- trees[[1]]$tip.label
  if (class(tree1.labels) == 'character') trees <- lapply(trees, NumberTips, sorted.labels = tree1.labels)
  quartets <- QuartetStates(lapply(trees, Tree2Splits))
  vapply(quartets[-1], CompareQuartets, cf=quartets[[1]], double(2))
}
