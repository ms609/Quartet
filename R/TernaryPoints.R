#' Plot Tree Differences on Ternary Plots
#' 
#' Generate points to depict tree difference (in terms of resolution
#' and similarity) on a ternary plot.
#' 
#' @template treesParam
#' @return A data frame listing the ternary coordinates of trees, based on the
#'         amount of information that they have in common with the first member 
#'         of the list.
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
QuartetPoints <- function (trees) {
  status <- QuartetStatus(trees)
  
  # Return: 
  data.frame(Unresolved   = rowSums(status[, c('r1', 'r2', 'u')]), 
             Contradicted = status[, 'd'],
             Consistent   = status[, 's'])
}

#' @describeIn QuartetPoints Uses partition distance instead of quartet metric.
#' @export
SplitPoints <- function (trees) {
  status <- SplitStatus(trees)
  # TODO Delete following line (?)
  # status <- rbind(status, status[1] - colSums(status))
  
  # This probably assumes that ref is bifurcating.  #TODO Add resilience
  # Return: 
  data.frame(Unresolved   = status[, 'ref_not_cf'] - status[, 'cf_not_ref'],
             Contradicted = status[, 'cf_not_ref'],
             Consistent   = status[, 'ref'] - status[, 'ref_not_cf'])
}
#' @rdname QuartetPoints
#' @export
BipartitionPoints <- SplitPoints
