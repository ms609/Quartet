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
#' @keywords internal
.AllPairsTripletDist <- function(trees) {
  nTrees <- length(trees)
  result <- matrix(0L, nTrees, nTrees,
                   dimnames = list(names(trees), names(trees)))
  for (r in seq_len(nTrees)) {
    for (c in seq_len(r - 1L)) {
      d <- TripletDistance(trees[[r]], trees[[c]])
      result[r, c] <- d
      result[c, r] <- d
    }
  }
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
  vapply(seq_along(trees1), function(i) {
    TripletDistance(trees1[[i]], trees2[[i]])
  }, integer(1))
}

#' Wrapper for CPDT-dist
#'
#' `r lifecycle::badge("deprecated")`
#'
#' Use [TripletDistance()] instead.
#'
#' @param tree1,tree2 Phylogenetic tree of class \code{phylo}.
#' @return `CPDTDist()` returns the triplet distance between `tree1` and
#' `tree2`.
#'
#' @examples
#' tree1 <- TreeTools::RootTree(TreeTools::BalancedTree(8), 1)
#' tree2 <- TreeTools::PectinateTree(8)
#' CPDTDist(tree1, tree2)
#'
#' @template MRS
#' @export
CPDTDist <- function(tree1, tree2) {
  lifecycle::deprecate_warn("1.4.0", "CPDTDist()", "TripletDistance()")
  TripletDistance(tree1, tree2)
}
