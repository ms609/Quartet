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
#' \insertRef{ThisStudy}{SlowQuartet}
#' 
#' @author Martin R. Smith
#' 
#' @export
QuartetPoints <- function (trees) {
  status <- MatchingQuartets(trees)
  
  # Return: 
  data.frame(Consistent   = status['s', ], 
             Contradicted = status['d', ],
             Unresolved   = colSums(status[c('r1', 'r2', 'u'), ]))
}

#' @describeIn QuartetPoints Uses partition distance instead of quartet metric.
#' @export
SplitsPoints <- function (trees) {
  status <- MatchingSplits(trees)
  status <- rbind(status, status[1] - colSums(status))
  
  # This probably assumes that ref is bifurcating.  #TODO Add resilience
  # Return: 
  data.frame(Consistent   = status['ref', ] - status['ref_not_cf', ], 
             Contradicted = status['cf_not_ref', ],
             Unresolved   = status['ref_not_cf', ] - status['cf_not_ref', ])
}
