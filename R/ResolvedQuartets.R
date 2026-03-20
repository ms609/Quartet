#' Count resolved quartets
#' 
#' Counts how many quartets are resolved or unresolved in a given tree, 
#' following \insertCite{Brodal2013;textual}{Quartet}.
#' 
#' Trees with more than 477 leaves risk encountering integer overflow errors,
#' as the number of quartets is larger than can be stored in R's signed 
#' 32-bit integer representation.  If warnings are thrown, check subsequent
#' calculations for errors.
#' 
#' @template treeParam
#' @param countTriplets Logical; if `TRUE`, the function will return the number
#' of triplets instead of the number of quartets.
#' 
#' @return `ResolvedQuartets()` returns a vector of length two, listing the
#' number of quartets (or triplets) that are `[1]` resolved; `[2]` unresolved 
#' in the specified tree.
#'         
#' @template MRS
#' 
#' @family enumeration
#' 
#' @examples
#' data(sq_trees)
#' 
#' ResolvedTriplets(sq_trees$collapse_some)
#' # Equivalent to:
#' ResolvedQuartets(sq_trees$collapse_some, countTriplets = TRUE)
#' 
#' vapply(sq_trees, ResolvedQuartets, integer(2))
#'
#' 
#' @references \insertAllCited{}
#' 
#' @importFrom TreeTools Preorder
#' @export 
ResolvedQuartets <- function (tree, countTriplets = FALSE) {
  .CheckSize(tree)
  tree <- Preorder(tree)
  .resolvedQuartetsCounts(tree, countTriplets)
}

# Core computation for ResolvedQuartets; assumes tree is already Preorder and
# validated via .CheckSize.  Called directly from hot loops that have already
# done both steps.  Delegates to C++ for quartets, falls back to R for triplets.
.resolvedQuartetsCounts <- function (tree, countTriplets = FALSE) {
  if (!countTriplets) {
    return(resolved_quartets(tree$edge, length(tree$tip.label)))
  }
  # Triplet path: R-only (not performance-critical)
  nTip <- length(tree$tip.label)
  nNode <- tree$Nnode

  edge <- tree$edge
  parent <- edge[, 1]
  child  <- edge[, 2]

  children <- unname(split(child, parent - nTip))

  n <- rep(1, nTip + nNode)
  unresolvedTripletsRootedHere <- integer(nNode)
  for (node in rev(seq_len(nNode)) + nTip) {
    nodeChildren <- children[[node - nTip]]
    n[node] <- sum(n[nodeChildren])
    s_vi <- n[nodeChildren[1L]]
    p_vi <- t_vi <- 0L
    for (i in seq_along(nodeChildren)[-1]) {
      n_vi <- n[nodeChildren[i]]
      t_vi <- t_vi + (n_vi * p_vi)
      p_vi <- p_vi + (n_vi * s_vi)
      s_vi <- s_vi + n_vi
    }
    unresolvedTripletsRootedHere[node - nTip] <- t_vi
  }
  unresolved <- sum(unresolvedTripletsRootedHere)
  resolved <- choose(nTip, 3) - unresolved

  if (any(c(resolved, unresolved) > .Machine$integer.max)) {
    stop("Sorry: trees too large for integer representation") # nocov
  } else if (resolved + unresolved > .Machine$integer.max) {
    warning("Large numbers: integer overflow likely") # nocov
  }

  as.integer(c(resolved, unresolved))
}


#' @describeIn ResolvedQuartets Convenience function to calculate the number of 
#' resolved/unresolved triplets.
#' @export
ResolvedTriplets <- function (tree) ResolvedQuartets(tree = tree,
                                                     countTriplets = TRUE)
