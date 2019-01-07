#' Unshift Tree
#' 
#' Add a tree to the start of a list of trees
#' 
#' This function is useful where the class of a list of trees is unknown.
#' Adding a tree to a multiPhylo object whose own attributes apply to all trees,
#' for example trees read from a nexus file, causes data to be lost.
#' 
#' @param add Tree to add to the list, of class \code{phylo}
#' @param treeList A list of trees, of class \code{list}, \code{multiPhylo},
#' or, if a single tree, \code{phylo}.
#' 
#' @return A list of class \code{list} or \code{multiPhylo} (following the 
#' original class of \code{treeList}), whose first element is the tree specified
#' as \code{add}.
#' 
#' @author Martin R. Smith
#' 
#' @keywords internal
#' @export
UnshiftTree <- function(add, treeList) {
  if (class(treeList) == 'multiPhylo') {
    structure(c(list(add), lapply(treeList, function (X) X)), class= 'multiPhylo')
  } else if (class(treeList) == 'phylo') {
    treeList <- structure(list(add, treeList), class='multiPhylo')
  } else { # including: if (class(trees) == 'list') {
    c(list(add), treeList)
  }
}
