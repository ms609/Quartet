WHICH_OTHER_NODE <- 2:4

#' Plot quartet on a tree topology
#' 
#' Draws a tree, highlighting the members of a specified quartet in colour.
#' 
#' 
#' @param tree A tree of class \code{\link[ape:read.tree]{phylo}},
#'   or a list of such trees.
#' @param quartet A vector of four integers, corresponding to numbered tips on
#'                the tree; or a character vector specifying the labels of four
#'                tips.
#' @param overwritePar Logical specifying whether to use existing 
#'                     `\link[graphics]{par}` `mfrow` and `mar` parameters
#'                      (\code{FALSE}),
#'                     or to plot trees side-by-side in a new graphical device (`TRUE`).
#' @param caption Logical specifying whether to annotate each plot to specify
#'   whether the quartet selected is in the same or a different state to the 
#'   reference tree.
#' @param \dots Additional parameters to send to \code{\link[graphics]{plot}}.
#'                
#' @author Martin R. Smith
#' 
#' @return Returns `invisible()`, having plotted a tree in which the first two members
#' of `quartet` are highlighted in orange, and the second two highlighted in 
#' blue.
#' 
#' @examples 
#'   data('sq_trees')
#'   
#'   par(mfrow=c(3, 5), mar=rep(0.5, 4))
#'   PlotQuartet(sq_trees, c(2, 5, 3, 8), overwritePar = FALSE)
#' 
#' @importFrom graphics par plot legend
#' @importFrom TreeSearch RenumberTips Tree2Splits
#' @export
PlotQuartet <- function (tree, quartet, overwritePar=TRUE, caption=TRUE, ...) { # nocov start
  cbPalette <- c("#000000", "#E69F00", "#56B4E9", "#009E73",
                 "#F0E442", "#0072B2", "#D55E00", "#CC79A7") 
  
  if (class(tree) == 'phylo') tree <- structure(list(tree), class='multiPhylo')
  
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
    if (caption) legend('bottomleft', bty='n', cex=0.9,
         if (QuartetState(quartet, Tree2Splits(tr)) == state1) "Same" else "Different")
  }
  invisible()
} #nocov end

#' List all quartets
#'
#' Lists all choices of four taxa from a tree.
#'  
#' A more computationally efficient alternative to \code{\link[utils]{combn}},
#' `AllQuartets` uses \code{\link[memoise]{memoise}} to make repeated calls faster.
#'
#' @param n_tips Integer, specifying the number of tips in a tree.
#' 
#' @return Returns a list of length \code{choose(n_tips, 4)}, with each entry 
#' corresponding to a unique selection of four different integers less than
#' or equal to `n_tips`
#' 
#' @author Martin R. Smith
#'
#' @family quartet counting functions
#' @seealso \code{\link[utils]{combn}}
#' 
#' @examples
#'  n_tips <- 6
#'  AllQuartets(n_tips)
#'  
#'  combn(n_tips, 4) # Provides the same information, but for large 
#'                   # values of n_tips is significantly slower.
#' 
#' @importFrom memoise memoise
#' @export
AllQuartets <- memoise(function (n_tips) {
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

#' Quartet State(s)
#' 
#' Report the status of the specified quartet(s).
#' 
#' One of the three possible four-taxon trees will be consistent with any set of bipartitions 
#' generated from a fully resolved tree.  If the taxa are numbered 1 to 4, this tree can be 
#' identified by naming the tip most closely related to taxon 1.
#' If a set of bipartitions is generated from a tree that contains polytomies, it is possible
#' that all three four-taxon trees are consistent with the set of bipartitions.
#'
#' @param tips A four-element array listing a quartet of tips, either by their
#'             number (if class `numeric`) or their name (if class `character`).
#' @param bips Bipartitions to evaluate.
#'
#' @return Returns `0` if the relationships of the four taxa are not constrained by the provided 
#' bipartitions, or the index of the closest relative to `tips[1]`, otherwise.
#'
#' @author Martin R. Smith
#' 
#' @family element-by-element comparisons
#' @seealso \code{\link{CompareQuartets}}, used to compare quartet states between
#'   trees.
#' @examples{
#'   n_tip <- 6
#'   trees <- list(ape::rtree(n_tip, tip.label=seq_len(n_tip), br=NULL),
#'                 ape::rtree(n_tip, tip.label=seq_len(n_tip), br=NULL))
#'   splits <- lapply(trees, TreeSearch::Tree2Splits)
#'   QuartetState(c(1, 3, 4, 6), splits[[2]])  
#'   QuartetState(1:4, splits[[1]]) == QuartetState(1:4, splits[[2]])
#'   vapply(AllQuartets(n_tip), QuartetState, bips=splits[[1]], double(1))
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

#' @describeIn QuartetState A convenience wrapper that need only be provided
#'  with a tree or a list of splits.
#' @param splits A list of bipartition splits, perhaps generated using 
#'        \code{\link[TreeSearch]{Tree2Splits}}, with row names corresponding 
#'        to taxon labels.
#'        If a tree or list of trees (of class \code{\link[ape:read.tree]{phylo}}) is sent instead, 
#'        it will be silently converted to its constituent splits.
#'        
#' @importFrom TreeSearch Tree2Splits
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
    vapply(AllQuartets(n_tips), QuartetState, double(1), 
           bips=bips[sort(rownames(bips)), , drop=FALSE])
  })
}

#' Compare quartet states by explicit enumeration
#' 
#' Uses explicit enumeration to compare two lists of quartet states, 
#' detailing how many are identical and how many are unresolved.
#' For most purposes, the faster function \code{\link{QuartetStatus}} will be preferable.
#' 
#' @param x,cf List of quartet states, perhaps generated by
#'  \code{\link{QuartetStates}}.
#'
#' @templateVar intro Returns an array of seven numeric elements, corresponding to the quantities of Estabrook _et al_. (1985):
#' @template returnEstabrook
#' 
#' @author Martin R. Smith
#'
#' @family element-by-element comparisons
#' @seealso \code{\link{QuartetStatus}}, generates this output from a list of
#'  trees.
#'
#' @examples
#'   n_tip <- 6
#'   trees <- list(ape::rtree(n_tip, tip.label=seq_len(n_tip), br=NULL),
#'                 ape::rtree(n_tip, tip.label=seq_len(n_tip), br=NULL))
#'   splits <- lapply(trees, TreeSearch::Tree2Splits)
#'   quartets <- QuartetStates(splits)
#'   CompareQuartets(quartets[[1]], quartets[[2]])
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
    N = 2L * length(x),
    Q = length(x),
    s = n_same,
    d = n_both_resolved - n_same,
    r1 = sum(x_resolved) - n_both_resolved,
    r2 = sum(cf_resolved) - n_both_resolved,
    u = sum(!x_resolved & !cf_resolved)
  )
}

#' @describeIn QuartetStatus Reports split statistics obtained after removing all
#'   tips that do not occur in both trees being compared.
#' @export
SharedQuartetStatus <- function (trees, cf=trees[[1]]) {
  t(vapply(trees, PairSharedQuartetStatus, tree2=cf, 
           c(N = 0L, Q = 0L, s = 0L, d = 0L, r1 = 0L, r2 = 0L, u = 0L)))
}

#' Status of quartets that exist in two trees
#' 
#' Removes all tips that do not occur in both `tree1` and `tree2`, then calculates 
#' the status of the remaining quartets.
#' 
#' @param tree1,tree2 Trees of class \code{\link[ape:read.tree]{phylo}}
#'  to compare.
#' 
#' @templateVar intro Returns a named array of six integers corresponding to the quantities of Estabrook _et al_. (1985):
#' @template returnEstabrook
#' 
#' @family element-by-element comparisons
#' @keywords internal
#' @importFrom ape drop.tip
#' @author Martin R. Smith
#' @export
PairSharedQuartetStatus <- function (tree1, tree2) {
  tips1 <- tree1$tip.label
  tips2 <- tree2$tip.label
  
  pruned1 <- drop.tip(tree1, setdiff(tips1, tips2))
  pruned2 <- drop.tip(tree2, setdiff(tips2, tips1))
  pruned2 <- RenumberTips(pruned2, tipOrder = intersect(tips1, tips2))
  
  # Return:
  SingleTreeQuartetAgreement(pruned1, pruned2)
}
