#' Plot tree differences on ternary plots
#' 
#' Generate points to depict tree difference (in terms of resolution
#' and accuracy) on a ternary plot.
#' 
#' The ternary plot will depict the number of quartets or splits that are:
#' - resolved in the reference tree (`cf`), but neither present nor contradicted
#'   in each comparison tree (`trees`);
#' - resolved differently in the reference and the comparison tree;
#' - resolved in the same manner in the reference and comparison trees. 
#' 
#' If the reference tree (`cf`) is taken to represent the best possible knowledge
#' of the 'true' topology, then polytomies in the reference tree represent
#' uncertainty.  If a tree in `trees` resolves relationships within this 
#' polytomy, it is not possible to establish (based only on the reference tree)
#' whether this resolution is correct or erroneous.  As such, extra resolution
#' in `trees` that is neither corroborated nor contradicted by `cf` is ignored.
#'
#' @template treesParam
#' @template treesCfParam
#' @return A data frame listing the ternary coordinates of trees, based on the
#' amount of information that they have in common with the comparison
#' tree (which defaults to the first member of the list, if unspecified).
#' 
#' @examples
#' library('Ternary')
#' data('sq_trees')
#' 
#' TernaryPlot(alab = 'Unresolved', blab = 'Contradicted', clab = 'Consistent',
#'             point = 'right')
#' TernaryLines(list(c(0, 2/3, 1/3), c(1, 0, 0)), col = 'red', lty = 'dotted')
#' TernaryText(QuartetPoints(sq_trees, cf = sq_trees$collapse_one), 1:15, 
#'             col = Ternary::cbPalette8[2], cex = 0.8)
#' TernaryText(SplitPoints(sq_trees, cf = sq_trees$collapse_one), 1:15, 
#'             col = Ternary::cbPalette8[3], cex = 0.8)
#' legend('bottomright', c("Quartets", "Splits"), bty = 'n', pch = 1, cex = 0.8,
#'        col = Ternary::cbPalette8[2:3])
#' 
#' @references 
#' - \insertRef{Smith2019}{Quartet}
#' 
#' @template MRS
#' 
#' @export
QuartetPoints <- function (trees, cf = trees[[1]]) {
  status <- QuartetStatus(trees, cf)
  
  # Return: 
  data.frame(Unresolved   = status[, 'r2'], 
             Contradicted = status[, 'd'],
             Consistent   = status[, 's'])
}

#' @rdname QuartetPoints
#' @export
SplitPoints <- function (trees, cf = trees[[1]]) {
  status <- SplitStatus(trees, cf)

  # Return: 
  data.frame(Unresolved   = status[, 'r2'],
             Contradicted = status[, 'd2'],
             Consistent   = status[, 's'])
}
#' @rdname QuartetPoints
#' @export
BipartitionPoints <- SplitPoints

#' Plot contours of equal symmetric difference on a ternary plot
#' 
#' Assumes that tree 1 is perfectly resolved, but that the resolution
#' of tree 2 can vary.
#' 
#' @param nsd Vector specifying normalized symmetric differences to plot.
#' 
#' @return Returns a matrix of dim `(length(nsd), 6)`, with columns named
#' `r2a`, `da`, `sa`, `r2b`, `db` and `sb`.
#' Lines from `a` to `b` in each row connect points of equal symmetric difference.
#' 
#' @template MRS
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

#' @describeIn SymmetricDifferenceLineEnds Plot the lines onto the active ternary plot.
#' @param \dots Further parameters to pass to 
#' \code{\link[Ternary:AddToTernary]{TernaryLines}()}.
#' @importFrom Ternary TernaryLines
#' @export
SymmetricDifferenceLines <- function (nsd, ...) { #nocov start
  apply(SymmetricDifferenceLineEnds(nsd), 1, function (ends) {
    TernaryLines(rbind(ends[1:3], ends[4:6]), ...)
  })
  # Return:
  invisible()
} #nocov end

