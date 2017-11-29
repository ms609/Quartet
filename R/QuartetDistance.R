WHICH_OTHER_NODE <- 2:4

#' Tree2Splits
#' 
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
  ret <- vapply(bipartitions[-seq_len(root)], 
         function (x) seq_len(n_tip) %in% x, 
         logical(n_tip))[seq_len(n_tip), , drop=FALSE]
  rownames(ret) <- tip_label
  
  # Return:
  ret
}

#' Column Sums
#' 
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
#'                the tree; or a character vector specifying the labels of four
#'                tips.
#' @param overwritePar Logical specifying whether to use existing 
#'                     \code{\link[graphics]{par} mfrow} and \code{mar} parameters (\code{FALSE}),
#'                     or to plot trees side-by-side in a new graphical device.
#' @param \dots Additional parameters to send to \code{\link[graphics]{plot}} 
#'                
#' @author Martin R. Smith
#' @importFrom graphics par plot text
#' @importFrom TreeSearch RenumberTips
#' 
#' @export
PlotQuartet <- function (tree, quartet, overwritePar=TRUE, ...) { # nocov start
  cbPalette <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
  
  if (class(tree) == 'phylo') tree <- list(tree)
  
  n_tip <- length(tree[[1]]$tip.label)
  
  if (overwritePar) {
    originalPar <- par(mfrow=c(1, length(tree)), mar=rep(1, 4))
    on.exit(par(originalPar))
  }
  
  labelOrder <- tree[[1]]$tip.label
  state1 <- QuartetState(quartet, Tree2Splits(tree[[1]]))
  tip_colours <- integer(n_tip) + 1L
  names(tip_colours) <- tree[[1]]$tip.label
  tip_colours[quartet] <- 3L
  tip_colours[c(quartet[1], quartet[state1])] <- 2L
  for (tr in tree) {
    tr <- RenumberTips(tr, labelOrder)
    plot(tr, tip.color=cbPalette[tip_colours], ...)
    text(1.1, 1.1, 
         if (QuartetState(quartet, Tree2Splits(tr)) == state1) "Same" else "Different")
  }
  return <- NULL
} #nocov end

#' Choices
#'
#'  List all choices of four taxa from a tree.
#'  
#'  A more computationally efficient alternative to \code{\link{combn}}.
#'  Uses \code{\link{memoise}} to make repeated calls faster.
#'
#' @param n_tips Integer, specifying the number of tips in a tree.
#' 
#' @return Returns a list of length \code{choose(n_tips, 4)}, with each entry 
#' corresponding to a unique selection of four different integers <= n_tips
#' 
#' @author Martin R. Smith
#'
#' @seealso \code{\link{combn}}
#' 
#' @examples{
#'  n_tips <- 6
#'  choice_list <- Choices(n_tips)
#'  choice_list
#'  combn(n_tips, 4) # Provides the same information, but for large 
#'                   # values of n_tips is significantly slower.
#' }
#' 
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
#' 
#' State of quartets
#'
#' Report the status of a given quartet.
#' @param tips A four-element array listing a quartet of tips, either by their
#'             number (if class `numeric`) or their name (if class `character`).
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
#' bipartitions, or the index of the closest relative to `tips[1]`, otherwise.
#'
#' @author Martin R. Smith
#' 
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
#' 
#' @references 
#'   \insertRef{Estabrook1985}{SlowQuartet}
#' 
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

#' @describeIn QuartetState A wrapper that need only be provided with a list of splits
#' @param splits a list of bipartition splits, perhaps generated using 
#'        \code{\link{Tree2Splits}}, with row names corresponding to taxon labels.
#'        If a tree or list of trees (of class phylo) is sent instead, it will be silently converted
#'        to its constituent splits.
#'        
#' @author Martin R. Smith
#' @export
QuartetStates <- function (splits) {
  if (class(splits) == 'phylo') {
    splits <- list(Tree2Splits(splits))
  } else if (class(splits) == 'multiPhylo') {
    splits <- lapply(splits, Tree2Splits)
  }
  
  if (class(splits) != 'list') splits <- list(splits)
  
  if (class(splits[[1]]) == 'phylo') splits <- lapply(splits, Tree2Splits)
  
  n_tips <- dim(splits[[1]])[1]
  lapply(splits, function (bips) {
    vapply(Choices(n_tips), QuartetState, double(1), bips=bips[sort(rownames(bips)), , drop=FALSE])
  })
}

#' Compare Quartets
#' 
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
#'  
#' @return 
#' Returns an array of six numberic elements, each corresponding to the quantities of 
#'   Estabrook _et al_. (1985):
#' @template returnEstabrook
#' 
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
#' 
#'@references {
#' \insertRef{Estabrook1985}{SlowQuartet}
#'}
#' 
#' @export
CompareQuartets <- function (x, cf) {
  x_resolved <- as.logical(x)
  cf_resolved <- as.logical(cf)
  both_resolved <- x_resolved & cf_resolved
  n_both_resolved <- sum(both_resolved)
  n_same <- sum(x[both_resolved] == cf[both_resolved])
  c(
    Q = length(x),
    s = n_same,
    d = n_both_resolved - n_same,
    r1 = sum(x_resolved) - n_both_resolved,
    r2 = sum(cf_resolved) - n_both_resolved,
    u = sum(!x_resolved & !cf_resolved)
  )
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

#' Unshift Tree
#' 
#' Add a tree to the start of a list of trees
#' 
#' This function is useful where the class of a list of trees is unknown.
#' Adding a tree to a multiPhylo object whose own attributes apply to all trees,
#' for example trees read from a nexus file, causes data to be lost..
#' 
#' @param add Tree to add to the list, of class \code{phylo}
#' @param treeList A list of trees, of class \code{list}, \code{mulitPhylo},
#' or, if a single tree, \code{phylo}.
#' 
#' @return A list of class \code{list} or \code{multiPhylo} (following the 
#' original class of \code{treeList}), whost first element is the tree specified
#' as \code{add}.
#' 
#' @author Martin R. Smith
#' 
#' @keywords internal
#' @export
UnshiftTree <- function(add, treeList) {
  if (class(treeList) == 'multiPhylo') {
    treeList <- c(list(add), lapply(treeList, function (X) X))
    class(treeList) <- 'multiPhylo'
    treeList
  } else if (class(treeList) == 'phylo') {
    treeList <- list(add, treeList)
  } else { # including: if (class(trees) == 'list') {
    c(list(add), treeList)
  }
}

#' Matching Quartets
#' 
#' Counts matching quartets
#' 
#' Determines the number of four-taxon trees consistent with multiple cladograms
#' 
#' Given a list of trees, returns the number of quartet statements present in the first tree
#' in the list also present in each other tree.
#' 
#' If all trees are bifurcating and the package `rtqdist` (Sand 2014) is installed, then the
#' function will use the faster `rtqdist`` package to generate quartet distances.
#' Otherwise the distances will be generated (slowly) within R.
#' 
#' rtqDist can be installed by copying the following code into your console:
#' \code{install.packages(
#' 'http://users-cs.au.dk/cstorm/software/tqdist/files/tqDist-1.0.0.tar.gz',
#'  repos=NULL, type='source')}
#'  or by [downloading the package](http://users-cs.au.dk/cstorm/software/tqdist/)
#'  and extracting the zipped directory into your library directory (which you 
#'  can locate by typing `.libPaths()` into your console).
#'   
#'   At present the trees must bear the same number of tips, and each tree$tip.label must use
#'   the integers 1:n_tip.  Support for different-sized trees will be added if there is demand; 
#'   contact the author if you would appreciate this functionality.
#'       
#'    A random pair of fully-resolved trees is expected to share 
#'    \code{choose(n_tip, 4) / 3} four-taxon trees.
#' 
#' @template treesParam
#' @template treesCfParam
#' @param use.tqDist Logical specifying whether to attempt to use the tqDist algorithm.
#'               Requires that the `rtqdist` package is installed.
#'
#' @return Returns a two dimensional array. 
#'         Columns correspond to the input trees; the first column will always
#'         report a perfect match as it compares the first tree to itself.
#'         Rows list the status of each quartet:
#' @template returnEstabrook
#'         
#'
#' @author Martin R. Smith
#' @examples{
#'  n_tip <- 6
#'  data(sq_trees)
#'  qt <- MatchingQuartets(sq_trees)
#'
#'  # Calculate Estabrook et al's similarity measures:
#'  do_not_conflict = qt[]
#' }
#' 
#' @references
#' \insertRef{Estabrook1985}{SlowQuartet}
#' \insertRef{Sand2014}{SlowQuartet}
#'
#' @importFrom Rdpack reprompt 
#' @importFrom TreeSearch RenumberTips
#' @importFrom utils installed.packages
#' @export
MatchingQuartets <- function (trees, cf=NULL, use.tqDist=TRUE) {
  if (!is.null(cf)) trees <- UnshiftTree(cf, trees)
  
  treeStats <- vapply(trees, function (tr)
    c(tr$Nnode, length(tr$tip.label)), double(2))
  if (length(unique(treeStats[2, ])) > 1) {
    stop("All trees must have the same number of tips")
  }
  if (use.tqDist && length(unique(treeStats[1, ])) == 1 && treeStats[2, 1] - treeStats[1, 1] == 1) {
    if ('rtqdist' %in% installed.packages()[, 'Package']) {
      tqDistances <- TQDist(trees)
      nTrees <- length(trees)
      nQuartets <- choose(length(trees[[1]]$tip.label), 4)
      tqDiffs <- tqDistances[1, ]
      t(data.frame(
        Q = rep(nQuartets, nTrees),
        s = nQuartets - tqDiffs,
        d = tqDiffs,
        r1 = integer(nTrees),
        r2 = integer(nTrees),
        u = integer(nTrees)
      ))
    } else {
      cat("Faster results can be obtained by installing rtqDist;",
          "see ?MatchingQuartets for installation instructions\n")
    }
  }
  tree1Labels <- trees[[1]]$tip.label
  trees <- lapply(trees, RenumberTips, tipOrder = tree1Labels)
  quartets <- QuartetStates(lapply(trees, Tree2Splits))
  ret <- vapply(quartets, CompareQuartets, cf=quartets[[1]], double(6))
  
  # Return:
  if (is.null(cf)) ret else ret[, -1]
}

#' Quartet Similarity Metrics
#' 
#' Functions to calculate the quartet metrics proposed by Estabrook _et al_.
#' (1985, table 2).
#'
#' @template treesParam
#' @param mq Counts of matching quartets generated by the function
#' [MatchingQuartets].
#' @param similarity Logical specifying whether to caluclate the similarity
#'                   or dissimilarity.
#'
#' @references 
#' \insertRef{Estabrook1985}{SlowQuartet}
#' 
#' @template MRS
#' 
#' @name QuartetMetrics
#' @export
QuartetMetrics <- function (trees, similarity=TRUE) {
  mq <- MatchingQuartets(trees)
  result <- data.frame(
    DoNotConflict = mq['d', ] / mq['Q', ],
    ExplicitlyAgree = 1 - (mq['s', ] / mq['Q', ]),
    StrictJointAssertions =  mq['d', ] / colSums(mq[c('d', 's'), ]),
    SemiStrictJointAssertions = mq['d', ] / colSums(mq[c('d', 's', 'u'), ]),
    QuartetDivergence =  colSums(mq[c('d', 'd', 'r1', 'r2'), ]) / (2 * mq['Q', ])
  )
  if (similarity) 1 - result else result
}

#' @rdname QuartetMetrics
#' @export
DoNotConflict <- function (mq, similarity=TRUE) {
  if (is.null(dim(mq))) mq <- as.matrix(mq)
  result <- mq['d', ] / mq['Q', ]
  if (similarity) 1 - result else result
}

#' @rdname QuartetMetrics
#' @export
ExplicitlyAgree <- function (mq, similarity=TRUE) {
  if (is.null(dim(mq))) mq <- as.matrix(mq)
  result <- mq['s', ] / mq['Q', ]
  if (similarity) result else 1 - result
}

#' @rdname QuartetMetrics
#' @export
StrictJointAssertions <- function (mq, similarity=TRUE) {
  if (is.null(dim(mq))) mq <- as.matrix(mq)
  result <- mq['d', ] / colSums(mq[c('d', 's'), ])
  if (similarity) 1 - result else result
}

#' @rdname QuartetMetrics
#' @export
SemiStrictJointAssertions <- function (mq, similarity=TRUE) {
  if (is.null(dim(mq))) mq <- as.matrix(mq)
  result <- mq['d', ] / colSums(mq[c('d', 's', 'u'), ])
  if (similarity) 1 - result else result
}

#' @rdname QuartetMetrics
#' @references \insertRef{ThisStudy}{SlowQuartet}
#' @export
QuartetDivergence <- function (mq, similarity=TRUE) {
  if (is.null(dim(mq))) mq <- as.matrix(mq)
  result <- colSums(mq[c('d', 'd', 'r1', 'r2'), ]) / ( 2 * mq['Q', ])
  if (similarity) 1 - result else result
}
