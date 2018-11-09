WHICH_OTHER_NODE <- 2:4

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
#' @export
PlotQuartet <- function (tree, quartet, overwritePar=TRUE, ...) { # nocov start
  cbPalette <- c("#000000", "#E69F00", "#56B4E9", "#009E73",
                 "#F0E442", "#0072B2", "#D55E00", "#CC79A7") 
  
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
#'   \insertRef{Estabrook1985}{Quartet}
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
#' @templateVar intro Returns an array of six numeric elements, each corresponding to the quantities of Estabrook _et al_. (1985):
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
#' \insertRef{Estabrook1985}{Quartet}
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

#' Unshift Tree
#' 
#' Add a tree to the start of a list of trees
#' 
#' This function is useful where the class of a list of trees is unknown.
#' Adding a tree to a multiPhylo object whose own attributes apply to all trees,
#' for example trees read from a nexus file, causes data to be lost..
#' 
#' @param add Tree to add to the list, of class \code{phylo}
#' @param treeList A list of trees, of class \code{list}, \code{multiPhylo},
#' or, if a single tree, \code{phylo}.
#' 
#' @return A list of class \code{list} or \code{multiPhylo} (following the 
#' original class of \code{treeList}), whose first element is the tree specified
#' as \code{add}.
#' 
#' @author Martin R. Smith
#' 
#' @keywords internal
#' @export
UnshiftTree <- function(add, treeList) {
  if (class(treeList) == 'multiPhylo') {
    structure(c(list(add), lapply(treeList, function (X) X)), class= 'multiPhylo')
  } else if (class(treeList) == 'phylo') {
    treeList <- structure(list(add, treeList), class='multiPhylo')
  } else { # including: if (class(trees) == 'list') {
    c(list(add), treeList)
  }
}


#' Quartet Similarity Metrics
#' 
#' Functions to calculate the quartet metrics proposed by Estabrook _et al_.
#' (1985, table 2).
#'
#' @template treesParam
#' @param mq Counts of matching quartets generated by the function
#' [MatchingQuartets].
#' @param similarity Logical specifying whether to calculate the similarity
#'                   or dissimilarity.
#'
#' @seealso [MatchingSplits], [CompareSplits]
#'
#' @references 
#' \insertRef{Estabrook1985}{Quartet}
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
#' @references \insertRef{ThisStudy}{Quartet}
#' @export
QuartetDivergence <- function (mq, similarity=TRUE) {
  if (is.null(dim(mq))) mq <- as.matrix(mq)
  result <- colSums(mq[c('d', 'd', 'r1', 'r2'), ]) / ( 2 * mq['Q', ])
  if (similarity) 1 - result else result
}
