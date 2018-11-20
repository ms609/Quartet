#' Quartet Points
#' 
#' Generate points to add to a ternary plot.
#' 
#' @template treesParam
#' @return A data frame listing the ternary coordinates of trees, based on the
#'         amount of information that they have in common with the first member 
#'         of the list.
#' 
#' @examples {
#'   data('sq_trees')
#'   library('Ternary')
#'   TernaryPlot('Consistent', 'Contradicted', 'Unresolved')
#'   TernaryLines(list(c(1/3, 2/3, 0), c(0, 0, 1)), col='red', lty='dotted')
#'   TernaryPoints(QuartetPoints(sq_trees), pch=cbPalette8[2])
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
SplitsPoints <- function (trees) {
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
#' @keywords internal
BipartitionPoints <- SplitsPoints
