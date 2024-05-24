#' Wrapper for CPDT-dist
#' 
#' Convenience function that passes a pair of trees to a C++ implementation
#' of the centroid path decomposition tree distance algorithm
#' \insertCite{Jansson2017jcb}{Quartet}.
#' 
#' @param tree1,tree2 Phylogenetic tree of class \code{phylo}.
#' @return `CPDTDist()` returns the quartet distance between `tree1` and
#' `tree2`.
#' 
#' @references
#' \insertAllCited{}
#' 
#' @examples
#' tree1 <- TreeTools::RootTree(TreeTools::BalancedTree(8), 1)
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
  
  .Call("_Quartet_cpdt_pair", tree1[["edge"]],
        RenumberTips(tree2, tree1)[["edge"]])
}

