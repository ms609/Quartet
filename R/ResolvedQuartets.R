#' Count Resolved Quartets
#' 
#' Counts how many quartets are resolved or unresolved in a given tree, 
#' following Brodal _et al._ 2013.
#' 
#' @template treeParam
#' @param countTriplets Logical; if `TRUE`, the function will return the number
#' of triplets instead of the number of quartets
#' 
#' @return A vector of length two, listing the number of quartets (or triplets)
#'  that are \[1\] resolved; \[2\] unresolved in the specified tree.
#'         
#' @author Martin R. Smith
#' @references \insertRef{Brodal2013}{Quartet}
#' 
#' @importFrom TreeSearch Renumber
#' @export 
ResolvedQuartets <- function (tree, countTriplets = FALSE) {
  tree <- Renumber(tree)
  nTip <- length(tree$tip.label)
  nNode <- tree$Nnode
  
  edge <- tree$edge
  parent <- edge[, 1]
  child  <- edge[, 2]
  
  children <- lapply(nTip + seq_len(nNode), function (node) {
    child[parent == node]
  })
  
  # Algebraic terms follow Brodal et al 2013
  
  n <- rep(1, nTip + nNode) # Will be overwritten
  unresolvedTripletsRootedHere <-
    unresolvedQuartetsRootedHere <- integer(nNode)
  for (node in rev(seq_len(nNode)) + nTip) {
    nodeChildren <- children[[node - nTip]]
    n[node] <- sum(n[nodeChildren])
    # s = subtree size
    s_vi <- n[nodeChildren[1L]]
    # p = pairs; t = triplets; q = quartets
    p_vi <- t_vi <- q_vi <- 0L
    for (i in seq_along(nodeChildren)[-1]) {
      n_vi <- n[nodeChildren[i]]
      # Order is important: we need to use previous values in each sum
      q_vi <- q_vi + (n_vi * t_vi)
      t_vi <- t_vi + (n_vi * p_vi)
      p_vi <- p_vi + (n_vi * s_vi)
      s_vi <- s_vi + n_vi
    }
    unresolvedTripletsRootedHere[node - nTip] <- t_vi
    unresolvedQuartetsRootedHere[node - nTip] <- q_vi + t_vi * (nTip - s_vi)
  }
  unresolved <- ifelse(countTriplets, sum(unresolvedTripletsRootedHere),
                           sum(unresolvedQuartetsRootedHere))
  resolved <- choose(nTip, ifelse(countTriplets, 3, 4)) - unresolved
  # Return:
  as.integer(c(resolved, unresolved))
}


#' @describeIn ResolvedQuartets Counts the number of resolved/unresolved triplets
#' @export
ResolvedTriplets <- function (tree) ResolvedQuartets(tree=tree, countTriplets=TRUE)