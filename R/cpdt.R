#' @describeIn Distances Triplet distance between two trees of class
#'   \code{phylo}.  Uses the CPDT algorithm
#'   \insertCite{Jansson2017jcb}{Quartet}.
#' @importFrom TreeTools RenumberTips
#' @export
TripletDistance.phylo <- function(tree1, tree2 = NULL) {
  if (is.null(tree2)) {
    stop("`tree2` is required when `tree1` is a single tree")
  }
  if (!inherits(tree2, "phylo")) {
    stop("`tree2` must be of class phylo")
  }
  tree2 <- RenumberTips(tree2, tree1)
  .Call("_Quartet_cpdt_pair",
        tree1[["edge"]][, 1], tree1[["edge"]][, 2],
        tree2[["edge"]][, 1], tree2[["edge"]][, 2])
}

#' @describeIn Distances Triplet distance between each pair of trees in a list.
#'   If \code{tree2} is provided, returns pairwise distances between
#'   corresponding trees in \code{tree1} and \code{tree2}.
#'   If \code{tree2 = NULL}, returns a distance matrix of all pairwise
#'   distances within \code{tree1}.
#' @importFrom TreeTools RenumberTips
#' @export
TripletDistance.list <- function(tree1, tree2 = NULL) {
  if (is.null(tree2)) {
    .AllPairsTripletDist(tree1)
  } else {
    .PairsTripletDist(tree1, tree2)
  }
}

#' @describeIn Distances Triplet distance between each pair of trees in a
#'   \code{multiPhylo} object.
#' @export
TripletDistance.multiPhylo <- TripletDistance.list

#' All pairwise triplet distances
#' @importFrom TreeTools RenumberTips
#' @keywords internal
.AllPairsTripletDist <- function(trees) {
  nTrees <- length(trees)
  treeNames <- names(trees)
  if (nTrees < 2L) {
    return(matrix(0L, nTrees, nTrees,
                  dimnames = list(treeNames, treeNames)))
  }
  # Renumber every tree to a common leaf ordering so that leaf `i` denotes the
  # same taxon in each tree; the trees are then parsed once in C++ and reused
  # across all pairs, rather than being re-parsed for each comparison.
  trees <- lapply(trees, RenumberTips, trees[[1]])
  edges <- lapply(trees, function(tr) tr[["edge"]])
  result <- .Call("_Quartet_cpdt_all_pairs", edges)
  dimnames(result) <- list(treeNames, treeNames)
  result
}

#' Pairwise triplet distances between corresponding trees
#' @keywords internal
.PairsTripletDist <- function(trees1, trees2) {
  if (inherits(trees1, "phylo")) trees1 <- list(trees1)
  if (inherits(trees2, "phylo")) trees2 <- list(trees2)
  if (length(trees1) != length(trees2)) {
    stop("`tree1` and `tree2` must contain the same number of trees")
  }
  if (!length(trees1)) {
    return(integer(0))
  }
  # `unlist()` (rather than vapply(integer(1))) preserves a distance that
  # overflows R's integer range and is returned as a double.
  unlist(lapply(seq_along(trees1), function(i) {
    TripletDistance(trees1[[i]], trees2[[i]])
  }))
}
