WHICH_OTHER_NODE <- 2:4

#' Plot quartet on tree topologies
#' 
#' Draws a tree, highlighting the members of a specified quartet in colour.
#' 
#' 
#' @param tree A tree of class \code{\link[ape:read.tree]{phylo}},
#'   or a list of such trees.  The first member of `tree` will be considered
#'   the "reference" tree.
#' @param quartet A vector of four integers, corresponding to numbered leaves on
#'  the tree; or a character vector specifying the labels of four leaves.
#' @param overwritePar Logical specifying whether to use existing `mfrow` and 
#' `mar` parameters from \code{\link[graphics]{par}()} (`FALSE`),
#' or to plot trees side-by-side in a new graphical device (`TRUE`).
#' @param caption Logical specifying whether to annotate each plot to specify
#'   whether the quartet selected is in the same or a different state to the 
#'   reference tree.
#' @param \dots Additional parameters to send to 
#' \code{\link[ape:plot.phylo]{plot}()}.
#'                
#' @template MRS
#' 
#' @return `PlotQuartet()` returns `invisible()`, having plotted a tree in 
#' which the first two members of `quartet` are highlighted in orange, and the
#' second two highlighted in blue.
#' 
#' @examples 
#' data("sq_trees")
#'
#' oPar <- par(mfrow = c(3, 6), mar = rep(0.5, 4))
#' PlotQuartet(sq_trees, c(2, 5, 3, 8), overwritePar = FALSE)
#' par(oPar)
#' 
#' @family visualization
#' @importFrom graphics par plot legend
#' @importFrom TreeTools NTip RenumberTips
#' @export
PlotQuartet <- function (tree, quartet, overwritePar = TRUE, 
                         caption = TRUE, ...) {
  cbPalette <- c("#000000", "#E69F00", "#56B4E9", "#009E73",
                 "#F0E442", "#0072B2", "#D55E00", "#CC79A7") 
  
  if (inherits(tree, "phylo")) {
    tree <- c(tree)
  }
  tree1 <- tree[[1]]
  n_tip <- NTip(tree1)
  
  if (overwritePar) {
    originalPar <- par(mfrow = c(1, length(tree)), mar = rep(1, 4))
    on.exit(par(originalPar))
  }
  
  labelOrder <- tree1$tip.label
  state1 <- QuartetState(quartet, tree1)
  tip_colours <- integer(n_tip) + 1L
  names(tip_colours) <- tree1$tip.label
  tip_colours[quartet] <- 2L
  if (state1) tip_colours[quartet[c(state1, 4L)]] <- 3L
  plot(tree1, tip.color = cbPalette[tip_colours], ...)
  if (caption) legend("bottomleft", bty = "n", cex = 0.9, "Reference")
  for (tr in tree[-1]) {
    tr <- RenumberTips(tr, labelOrder)
    plot(tr, tip.color = cbPalette[tip_colours], ...)
    if (caption) {
      trState <- QuartetState(quartet, tr)
      legend("bottomleft", bty = "n", cex = 0.9,
         if (trState == state1) {
           "Same"
         } else if (trState == 0L) {
           "Lost resolution"
         } else if (state1 == 0L) {
           "Gained resolution"
         } else {
           "Different"
         })
    }
  }
  invisible()
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
#' @template MRS
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
