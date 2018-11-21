#' Plot Tree Differences on Ternary Plots
#' 
#' Generate points to depict tree difference (in terms of resolution
#' and similarity) on a ternary plot.
#' 
#' @template treesParam
#' @template treesCfParam
#' @return A data frame listing the ternary coordinates of trees, based on the
#'         amount of information that they have in common with the comparison
#'         tree (which defaults to the first member of the list, if unspecified).
#'         
#'         If the comparison tree `cf` contains polytomies, then it is possible
#'         that trees within `trees` will be more resolved than `cf`.  A split
#'         that is resolved in `trees[[i]]` but not in `cf` will contribute 
#'         `+1` to the `Contradicted` count, and `-1` to the `Unresolved` count.
#' 
#' @examples {
#'   library('Ternary')
#'   data('sq_trees')
#'   
#'   TernaryPlot('Consistent', 'Contradicted', 'Unresolved')
#'   TernaryLines(list(c(1/3, 2/3, 0), c(0, 0, 1)), col='red', lty='dotted')
#'   TernaryPoints(QuartetPoints(sq_trees), col=Ternary::cbPalette8[2])
#'   TernaryPoints(SplitPoints(sq_trees), col=Ternary::cbPalette8[3])
#' }
#' 
#' @references 
#' \insertRef{Smith2019}{Quartet}
#' 
#' @author Martin R. Smith
#' 
#' @export
QuartetPoints <- function (trees, cf = trees[[1]]) {
  status <- QuartetStatus(trees)
  
  # Return: 
  data.frame(Unresolved   = rowSums(status[, c('r1', 'r2', 'u')]), 
             Contradicted = status[, 'd'],
             Consistent   = status[, 's'])
}

#' @describeIn QuartetPoints Uses partition distance instead of quartet metric.
#' @export
SplitPoints <- function (trees, cf = trees[[1]]) {
  status <- SplitStatus(trees, cf)

    # Return: 
  data.frame(Unresolved   = status[, 'ref_not_cf'] - status[, 'cf_not_ref'],
             Contradicted = status[, 'cf_not_ref'],
             Consistent   = status[, 'cf_and_ref'])
}
#' @rdname QuartetPoints
#' @export
BipartitionPoints <- SplitPoints
