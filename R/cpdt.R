#' Wrapper for CPDT-dist
#' 
#' Convenience function that passes a pair of trees to a C++ implementation
#' of the centroid path decomposition tree distance algorithm
#' \insertCite{Janssen2017jcb}{Quartet}.
#' 
#' @param tree1,tree2 Phylogenetic tree of class \code{phylo}.
#' @return `CPDTDist()` returns the quartet distance between `tree1` and
#' `tree2`.
#' 
#' @references
#' \insertAllCited{}
#' 
#' @examples
#' tree1 <- TreeTools::BalancedTree(8)
#' tree2 <- TreeTools::PectinateTree(8)
#' CPDTDist(tree1, tree2)
#' 
#' @importFrom ape write.tree
#' @importFrom TreeTools RenumberTips
#' @template MRS
#' @author C++ code written by Ramesh Rajaby.
#' @useDynLib Quartet, .registration = TRUE
#' @export
CPDTDist <- function (tree1, tree2) {
  if (!inherits(tree1, "phylo")) {
    stop("`tree1` must be of class phylo")
  }
  if (!inherits(tree2, "phylo")) {
    stop("`tree2` must be of class phylo")
  }
  t1 <- tree1
  t2 <- RenumberTips(tree2, tree1)
  t1[["tip.label"]] <- t2[["tip.label"]] <- seq_len(NTip(t1))
  
  file1 <- tempfile("tree-", fileext = ".nwk")
  write.tree(t1, file = file1)
  on.exit(unlink(file1))
  file2 <- tempfile("tree-", fileext = ".nwk")
  write.tree(t2, file = file2)
  on.exit(unlink(file2), add = TRUE)
  
  .Call("_Quartet_cpdt_dist_file", file1, file2)
}

