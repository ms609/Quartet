#' tqDist wrapper
#' 
#' Convenience function that takes a list of trees, writes them to a text file,
#' and calls tqDist on the generated file (which is deleted on completion).
#' 
#' @param treeList List of phylogenetic trees, of class \code{list} or
#'                 \code{phylo}. All trees must be bifurcating.
#' @return `TQDist` returns the quartet distance between each pair of trees
#' @references {
#'   @template refTqDist
#' }
#' @importFrom ape write.tree
#' @importFrom stats runif
#' @author Martin R. Smith
#' @export
TQDist <- function (treeList) {
  fileName <- TQFile(treeList)
  on.exit(file.remove(fileName))
  AllPairsQuartetDistance(fileName)
}

#' @describeIn TQDist
#' @author Martin R. Smith
#' @return `TQDist` returns the number of resolved quartets in agreement between 
#'   each pair of trees (A in Brodal _et al_. 2013) and the number of quartets 
#'   that are unresolved in both trees (E in Brodal _et al_. 2013).
#' @export 
TQAE <- function (treeList) {
  fileName <- TQFile(treeList)
  on.exit(file.remove(fileName))
  AllPairsQuartetStatus(fileName)
}

#' tqDist file generator
#' 
#' Creates a temporary file corresponding to a list of trees,
#' to be processed with tqDist.  Files should be destroyed using
#' `on.exit(file.remove(fileName))` by the calling function.
#' @return Name of the created file
#' @keywords internal
#' @export
TQFile <- function (treeList) {
  if (class(treeList) == 'list') class(treeList) <- 'multiPhylo'
  if (class(treeList) != 'multiPhylo') stop("treeList must be a list of phylogenetic trees")
  fileName <- paste0('~temp', substring(runif(1), 3), '.trees')
  write.tree(treeList, file=fileName)
  # Return:
  fileName
}

#' Triplet and quartet distances with tqDist
#' 
#' Functions to calculate triplet and quartet distances between pairs of trees.
#' 
#' @param file,file1,file2 Paths to files containing a tree or trees in Newick format.
#' 
#' @return `Distance` functions return the distance between the requested trees.
#' `Status` functions return the number of triplets or quartets that are:
#'  `A`, resolved in the same fashion in both trees;
#'  `E`, unresolved in both trees.
#'  Comparing a tree against itself yields the totals (A+B+C) and (D+E) 
#'  referred to by Brodal _et al_. (2013) and Holt _et al_. (2014).
#' 
#' @author Martin R. Smith, after Andreas Sand
#' 
#' @references \insertRef{Sand2014}{Quartet}
#'   \insertRef{Brodal2013}{Quartet}
#'   \insertRef{Holt2014}{Quartet}
#' @export
QuartetDistance <- function(file1, file2) {
  ValidateQuartetFile(file1)
  ValidateQuartetFile(file2)
  .Call('_Quartet_tqdist_QuartetDistance', as.character(file1), as.character(file2));
}

#' @describeIn QuartetDistance Returns a vector of length two, listing \[1\]
#' the number of resolved quartets that agree ('A');
#' \[2\] the number of quartets that are unresolved in both trees ('E').
#' See Brodal et al. (2013).
#'  
#' @export
QuartetStatus <- function(file1, file2) {
  ValidateQuartetFile(file1)
  ValidateQuartetFile(file2)
  .Call('_Quartet_tqdist_QuartetStatus', as.character(file1), as.character(file2));
}

#' @export
#' @importFrom ape read.tree
#' @describeIn QuartetDistance Quartet distance between the tree on each line of `file1`
#'   and the tree on the corresponding line of `file2`
PairsQuartetDistance <- function(file1, file2) {
  ValidateQuartetFile(file1)
  ValidateQuartetFile(file2)
  trees1 <- read.tree(file1)
  trees2 <- read.tree(file2)
  if (length(trees1) != length(trees2) || class(trees1) != class(trees2)) {
    stop("file1 and file2 must contain the same number of trees")
  }
  .Call('_Quartet_tqdist_PairsQuartetDistance', as.character(file1), as.character(file2));
}

#' @export
#' @describeIn QuartetDistance Quartet distance between each tree listed in `file` and 
#'   each other tree therein
AllPairsQuartetDistance <- function(file) {
  ValidateQuartetFile(file)
  .Call('_Quartet_tqdist_AllPairsQuartetDistance', as.character(file));
}

#' @export
#' @describeIn QuartetDistance Quartet status for each pair of trees in `file`
AllPairsQuartetStatus <- function(file) {
  ValidateQuartetFile(file)
  result <- .Call('_Quartet_tqdist_AllPairsQuartetStatus', as.character(file));
  nTrees <- nrow(result)
  array(result, c(nTrees, nTrees, 2), dimnames=list(NULL, NULL, c('A', 'E')))
}

#' @export
#' @describeIn QuartetDistance Triplet distance between the single tree given 
#'   in each file
TripletDistance <- function(file1, file2) {
  ValidateQuartetFile(file1)
  ValidateQuartetFile(file2)
  .Call('_Quartet_tqdist_TripletDistance', as.character(file1), as.character(file2));
}

#' @export
#' @describeIn QuartetDistance Triplet distance between the tree on each line of `file1`
#'   and the tree on the corresponding line of `file2`
PairsTripletDistance <- function(file1, file2) {
  ValidateQuartetFile(file1)
  ValidateQuartetFile(file2)
  .Call('_Quartet_tqdist_PairsTripletDistance', as.character(file1), as.character(file2));
}

#' @export
#' @describeIn QuartetDistance Triplet distance between each tree listed in `file` and 
#'   each other tree therein
AllPairsTripletDistance <- function(file) {
  ValidateQuartetFile(file)
  .Call('_Quartet_tqdist_AllPairsTripletDistance', as.character(file));
}

#' Validate filenames
#' 
#' Verifies that file parameters are character strings describing files that exist
#' 
#' @param file Variable to validate
#' 
#' @return `TRUE` if `file` is a character vector of length one describing 
#'   a file that exists, a fatal error otherwise.
#' 
#' @author Martin R. Smith
#' 
#' @export
#' @keywords internal
ValidateQuartetFile <- function (file) {
  if (length(file) != 1) {
    stop("file must be a character vector of length one")
  }
  if (!file.exists(file)) {
    stop("file ", file, " not found")
  }
}