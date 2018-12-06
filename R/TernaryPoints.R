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

#' Plot lines of equal Symmetric Difference on a ternary plot
#' 
#' Assumes that tree 1 is perfectly resolved, but that the resolution
#' of tree 2 can vary.
#' 
#' @param nsd Vector specifying normalized symmetric differences to plot
#' 
#' @return Returns a matrix of dim `(length(nsd), 6)``, with columns named `r2a`, `da`, `sa`,
#' `r2b`, `db` ans `sb`.  Lines from `a` to `b` in each row connect points
#' of equal symmetric difference.
#' 
#' @author Martin R. Smith
#' @export
SymmetricDifferenceLineEnds <- function (nsd) {
  if (any(nsd < 0 | nsd > 1)) stop("nsd must be between 0 and 1")
  r2a <- 0
  r2b <- (2L * nsd) / (1L + nsd)
  sa <- (1 - nsd) * (1 - (r2a / 2))
  sb <- (1 - nsd) * (1 - (r2b / 2))
  da <- 1 - sa - r2a
  db <- 0
  # Return:
  cbind(r2a, da, sa, r2b, db, sb)
}

#' @describeIn SymmetricDifferenceLineEnds Plot the lines onto the active ternary plot
#' @param \dots Further parameters to pass to 
#' \code{\link[Ternary:AddToTernary]{TernaryLines}}
#' @importFrom Ternary TernaryLines
#' @export
SymmetricDifferenceLines <- function (nsd, ...) {
  apply(SymmetricDifferenceLineEnds(nsd), 1, function (ends) {
    TernaryLines(rbind(ends[1:3], ends[4:6]), ...)
  })
  # Return:
  invisible()
}

