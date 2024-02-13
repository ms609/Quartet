#' Wrapper for CPDT-dist
#' 
#' Convenience function that passes a pair of trees to a C++ implementation
#' of the centroid path decomposition tree distance algorithm
#' \insertCite{Janssen2017jcb}{Quartet}.
#' 
#' @param tree1,tree2 Phylogenetic tree, either as a \code{phylo} object or
#' a Newick-format string.
#' @return `CPDTDist()` returns the quartet distance between `tree1` and
#' `tree2`.
#' 
#' @references
#' \insertAllCited{}
#' 
#' @examples
#' tree1 <- TreeTools::BalancedTree(8)
#' tree2 <- "(((t1,t2),(t3,t4)),((t5,t7),(t6,t8)));"
#' CPDTDist(tree1, tree2)
#' 
#' @importFrom ape write.tree
#' @template MRS
#' @author C++ code written by Ramesh Rajaby.
#' @useDynLib Quartet, .registration = TRUE
#' @export
CPDTDist <- function (tree1, tree2) {
  if (inherits(tree1, "phylo")) {
    tree1 <- write.tree(tree1)
  }
  if (inherits(tree2, "phylo")) {
    tree2 <- write.tree(tree2)
  }
  if (!is.character(tree1)) {
    stop("`tree1` must be of class character or phylo")
  }
  if (!is.character(tree2)) {
    stop("`tree2` must be of class character or phylo")
  }
  
  file1 <- tempfile("tree-", fileext = ".nwk")
  write(tree1, file1)
  on.exit(unlink(file1))
  file2 <- tempfile("tree-", fileext = ".nwk")
  write(tree2, file2)
  on.exit(unlink(file2), add = TRUE)
  
  .Call("_Quartet_cpdt_dist_file", file1, file2)
}

