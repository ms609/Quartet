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
#' @examples Tree2Splits(ape::rtree(6, tip.label=1:6, br=NULL))
#'
#' @importFrom ape reorder.phylo
#' @useDynLib SlowQuartet, .registration = TRUE
#' @export
Tree2Splits <- function (tr) {
  tr <- reorder.phylo(tr, 'postorder')
  tip_label <- tr$tip.label
  n_tip <- as.integer(length(tip_label))
  root <- length(tip_label) + 1
  bipartitions <- phangorn_bipCPP(tr$edge, n_tip)
  vapply(bipartitions[-seq_len(root)], function (x) seq_len(n_tip) %in% x, logical(n_tip))[as.double(tip_label), , drop=FALSE]
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
#' An accelerated version of the R function \code{colSums(x, na.rm = FALSE, dims = 1L)}.
#' Using this function makes \code{MatchingQuartets} 8% faster.
#' But it is \emph{very} naughty to call \code{.Internal}, so I use the 
#' internal R colSums function instead.
#' @param x Matrix whose columns are to be summed.
#' @param n_cols Number of columns in said matrix.
#' @author Martin R. Smith
#' @keywords internal
# ColSums <- function (x, n_cols = ncol(x)) .Internal(colSums(x, 4, n_cols, FALSE))


#' Plot Quartet
#' 
#' Plots a given quartet
#' @param tree A tree of class \code{phylo}, or a list of such trees.
#' @param quartet A vector of four integers, corresponding to numbered tips on
#'                the tree.
#' @author Martin R. Smith
#' @importFrom graphics par plot text
#' @importFrom TreeSearch RenumberTips
#' @export
PlotQuartet <- function (tree, quartet) {
  if (class(tree) == 'phylo') tree <- list(tree)
  n_tip <- length(tree[[1]]$tip.label)
  originalPar <- par(mfrow=c(1, length(tree)), mar=rep(1, 4))
  on.exit(par(originalPar))
  labelOrder <- tree[[1]]$tip.label
  state1 <- QuartetState(quartet, Tree2Splits(tree[[1]]))
  tip_colours <- integer(n_tip) + 1L
  tip_colours[quartet] <- 3L
  tip_colours[c(quartet[1], quartet[state1])] <- 2L
  for (tr in tree) {
    tr <- RenumberTips(tr, labelOrder)
    plot(tr, tip.color=tip_colours)
    text(1.1, 1.1, 
         if (QuartetState(quartet, Tree2Splits(tr)) == state1) "Same" else "Different")
  }
}

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
#' @seealso \code{\link{combn}}
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
  unlist(lapply(seq_len(n_tips - 3), function (i) {
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
#' Report the status of a given quartet.
#' @param tips A four-element array listing a quartet of tips.
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
  quartet_splits <- bips[tips, , drop=FALSE]
  statement <- quartet_splits[, colSums(quartet_splits) == 2, drop=FALSE]
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
#' Compare quartet states between trees
#'
#'  Compares two lists of quartet states, detailing how many are identical and 
#'  how many are unresolved.
#' 
#' @param x A list of quartet states, perhaps generated in
#'  \code{\link{CompareQuartets}}.
#' @param cf a second such list.
#'
#' Compares each quartet in a list, calculating how many statements are identical
#'  in both lists.
#' @return {
#'   Returns an array of two elements:
#'   1: the number of quartets that are present in both trees
#'   2: the number of quartets that are not resolved in both trees
#'   The number of quartets that are present in one tree and contradicted in the
#'   other can be calculated by deducting the total from the total number of 
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

#' tqDist wrapper
#' 
#' @param treeList List of phylogenetic trees, of class \code{list} or
#'                 \code{phylo}. All trees must be bifurcating.
#' @return Quartet distances between each pair of trees
#' @references {
#'   @template refTqDist
#' }
#' @importFrom ape write.tree
#' @importFrom stats runif
#' @export
TQDist <- function (treeList) {
  if (class(treeList) == 'list') class(treeList) <- 'multiPhylo'
  if (class(treeList) != 'multiPhylo') stop("treeList must be a list of phylogenetic trees")
  fileName <- paste0('~temp', substring(runif(1), 3), '.trees')
  write.tree(treeList, file=fileName)
  on.exit(file.remove(fileName))
  rtqdist::allPairsQuartetDistance(fileName)
}

#' Matching Quartets
#' Count matching quartets
#' Determines the number of four-taxon trees consistent with multiple cladograms
#' 
#' Given a list of trees, returns the number of quartet statements present in the first tree
#' in the list also present in each other tree.
#' 
#' If all trees are bifurcating and the package `rtqdist` is installed, then the
#' function will use the faster `rtqdist`` package to generate quartet distances.
#' Otherwise the distances will be generated (slowly) within R.
#' 
#' rtqDist can be installed by copying the following code into your console:
#' \code{install.packages(
#' 'http://users-cs.au.dk/cstorm/software/tqdist/files/tqDist-1.0.0.tar.gz',
#'  repos=NULL, type='source')}
#'  Or by [downloading the package](http://users-cs.au.dk/cstorm/software/tqdist/)
#'  and extracting the zipped directory into your library directory (which you 
#'  can locate by typing `.libPaths()` into your console).
#'   
#'   At present the trees must bear the same number of tips, and each tree$tip.label must use
#'   the integers 1:n_tip.  Support for different-sized trees will be added if there is demand; 
#'   contact the author if you would appreciate this functionality.
#'       
#'       A random pair of fully-resolved trees is expected to share 
#'       \code{choose(n_tip, 4) / 3} four-taxon trees.
#' 
#' @template treesParam
#' @param use.tqDist Logical specifying whether to attempt to use the tqDist algorithm.
#'               Requires that the `rtqdist` package is installed.
#'
#' @return Returns a two dimensional array. 
#'         Columns correspond to the input trees; the first column will always
#'         report a perfect match as it compares the first tree to itself.
#'         Rows report the number of quartets that : 1, are present in 
#'         \code{trees[[1]]} and the corresponding input tree;
#'         2: are unresolved in (at least) one of trees[[1]] and the corresponding 
#'         input tree. Quartets that DIFFER between the two relevant trees can be 
#'         calculated by deducting the quartets in either of the other two
#'         categories from the total number of quartets, given by
#'         \code{\link{choose}(n_tip, 4)}.
#'
#' @author Martin R. Smith
#' @examples{
#'  n_tip <- 6
#'  trees <- lapply(logical(12), function (x) ape::rtree(n_tip, tip.label=seq_len(n_tip), br=NULL))
#'  compare_result <- MatchingQuartets(trees)
#'  dissimilar_quartets <- choose(n_tip, 4) - colSums(compare_result)  
#'  result <- rbind(compare_result, dissimilar_quartets)
#'  rownames(result) <- c('Shared', 'Unresolved', 'Dissimilar')
#'  result
#' }
#' 
#' @references {
#'   @template refEstabrook1985
#'   @template refTqDist
#' }
#' @importFrom TreeSearch RenumberTips
#' @importFrom utils installed.packages
#' @export
MatchingQuartets <- function (trees, use.tqDist=TRUE) {
  treeStats <- vapply(trees, function (tr)
    c(tr$Nnode, length(tr$tip.label)), double(2))
  if (length(unique(treeStats[2, ])) > 1) {
    stop("All trees must have the same number of tips")
  }
  if (use.tqDist && length(unique(treeStats[1, ])) == 1 && treeStats[2, 1] - treeStats[1, 1] == 1) {
    if ('rtqdist' %in% installed.packages()[, 'Package']) {
      tqDistances <- TQDist(trees)
      tqMatches <- choose(length(trees[[1]]$tip.label), 4) - tqDistances[1, ]
      ret <- rbind(tqMatches, rep(0, length(trees)))
      rownames(ret) <- NULL
      return (ret)
    } else {
      cat("Faster results can be obtained by installing rtqDist;",
          "see ?MatchingQuartets for installation instructions")
    }
  }
  tree1Labels <- trees[[1]]$tip.label
  trees <- lapply(trees, RenumberTips, tipOrder = tree1Labels)
  quartets <- QuartetStates(lapply(trees, Tree2Splits))
  vapply(quartets, CompareQuartets, cf=quartets[[1]], double(2))
}
