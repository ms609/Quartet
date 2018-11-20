#' tqDist wrapper
#' 
#' Convenience function that takes a list of trees, writes them to a text file,
#' and calls tqDist on the generated file (which is deleted on completion).
#' 
#' Quartets can be resolved in one of five ways, which 
#'  Brodal _et al_. (2013) and Holt _et al_. (2014) distinguish using the letters
#'  A--E, and Estabrook (1985) refers to as:
#'  
#'  A: $s$ = resolved the ***s***ame in both tres
#'  B: $d$ = resolved ***d***ifferently in both trees
#'  C: $r_1$ = ***r***esolved only in tree ***1***
#'  D: $r_2$ = ***r***esolved only in tree ***2***
#'  E: $u$ = ***u***nresolved in both trees
#'  
#' 
#' @param treeList List of phylogenetic trees, of class \code{list} or
#'                 \code{phylo}. All trees must be bifurcating.
#' @return `TQDist` returns the quartet distance between each pair of trees
#' @references
#'   \insertRef{Brodal2013}{Quartet}
#'   \insertRef{Estabrook1985}{Quartet}
#'   \insertRef{Holt2014}{Quartet}
#'   \insertRef{Sand2014}{Quartet}
#' @importFrom ape write.tree
#' @importFrom stats runif
#' @author Martin R. Smith
#' @export
TQDist <- function (treeList) {
  fileName <- TQFile(treeList)
  on.exit(file.remove(fileName))
  AllPairsQuartetDistance(fileName)
}

#' @describeIn TQDist Number of agreeing quartets that are resolved / unresolved
#' @author Martin R. Smith
#' @return `TQDist` returns the number of resolved quartets in agreement between 
#'   each pair of trees (A in Brodal _et al_. 2013) and the number of quartets 
#'   that are unresolved in both trees (E in Brodal _et al_. 2013).
#' @export 
TQAE <- function (treeList) {
  fileName <- TQFile(treeList)
  on.exit(file.remove(fileName))
  AllPairsQuartetAgreement(fileName)
}

#' @describeIn TQDist Agreement of each quartet, comparing each pair of trees in a list
#' @author Martin R. Smith
#' @return `ManyToManyQuartetAgreement` returns a three-dimensional array listing,
#'   for each pair of trees in turn, the number of quartets in each category.
#' @export 
ManyToManyQuartetAgreement <- function (treeList) {
  AE <- TQAE(treeList)
  nTree <- dim(AE)[1]
  A   <- AE[, , 1]
  E   <- AE[, , 2]
  ABD <- matrix(diag(A), nTree, nTree)
  CE  <- matrix(diag(E), nTree, nTree)
  DE  <- t(DE)
  C   <- CE - E
  D   <- DE - E
  B   <- ABD - A - D
  
  # Return:
  array(c(A, B, C, D, E), dim=c(nTree, nTree, 5),
        dimnames = list(NULL, NULL, c('s', 'd', 'r1', 'r2', 'u')))
}

#' @describeIn TQDist Agreement of each quartet in trees in a list with the
#' quartets in a comparison tree
#' @author Martin R. Smith
#' @param comparison A single tree against which to compare the trees in treeList
#' @return `SingleTreeQuartetAgreement` returns a two-dimensional array listing,
#'   for tree in `treeList`, the total number of quartets and the 
#'   number of quartets in each category.  
#'   The `comparison` tree is treated as `tree2`.
#' @export 
SingleTreeQuartetAgreement <- function (treeList, comparison) {
  singleFile <- TQFile(comparison)
  multiFile  <- TQFile(treeList)
  on.exit(file.remove(singleFile, multiFile))
  AE <- OneToManyQuartetAgreement(singleFile, multiFile)
  DE <- vapply(treeList, ResolvedQuartets, integer(2))[2, ]
  nTree <- length(DE)
  
  A   <- AE[, 1]
  E   <- AE[, 2]
  rq <- ResolvedQuartets(comparison)
  ABD <- rq[1]
  CE <-  rq[2]
  C   <- CE - E
  D   <- DE - E
  
  B   <- ABD - A - D
  
  # Return:
  array(c(rep(sum(ABD, CE), nTree), A, B, C, D, E), dim=c(nTree, 6),
        dimnames=list(names(treeList), c('Q', 's', 'd', 'r1', 'r2', 'u')))
}

#' Matching Quartets
#' 
#' Counts matching quartets
#' 
#' Determines the number of quartets consistent with multiple cladograms
#' 
#' Given a list of trees, returns the number of quartet statements present in
#'  the first tree in the list also present in each other tree.
#' 
#' At present the trees must bear the same number of tips.  
#' Support for different-sized trees will be added if there is demand; 
#'   contact the maintainer if you would appreciate this functionality.
#'       
#' A random pair of fully-resolved trees is expected to share 
#'    \code{choose(n_tip, 4) / 3} quartets.
#' 
#' @template treesParam
#' @template treesCfParam
#' 
#' @templateVar intro Returns a two dimensional array. Columns correspond to the input trees; the first column will always         report a perfect match as it compares the first tree to itself.         Rows list the status of each quartet:
#' @template returnEstabrook
#'         
#' @author Martin R. Smith
#' @examples{
#'  data(sq_trees)
#'  # Calculate the status of each quartet
#'  QuartetStatus(sq_trees)
#'
#'  # Calculate Estabrook et al's similarity measures:
#'  QuartetMetrics(sq_trees)
#' }
#' 
#' @seealso [SplitStatus]
#' 
#' @references {
#'   \insertRef{Estabrook1985}{Quartet}
#'   \insertRef{Sand2014}{Quartet}
#' }
#'
#' @importFrom Rdpack reprompt 
#' @importFrom TreeSearch RenumberTips
#' @export
QuartetStatus <- function (trees, cf=trees[[1]]) {
  SingleTreeQuartetAgreement(trees, comparison=cf)
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
  if (!class(treeList) %in% c('phylo', 'multiPhylo'))
    stop("treeList must be a tree of class phylo, or a list of phylogenetic trees")
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
#' `Agreement` functions return the number of triplets or quartets that are:
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
QuartetAgreement <- function(file1, file2) {
  ValidateQuartetFile(file1)
  ValidateQuartetFile(file2)
  .Call('_Quartet_tqdist_QuartetAgreement', as.character(file1), as.character(file2));
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
#' @importFrom ape read.tree
#' @describeIn QuartetDistance Quartet distance between the tree in 
#'  `file1` and the tree on each line of `file2`
OneToManyQuartetAgreement <- function(file1, file2) {
  ValidateQuartetFile(file1)
  ValidateQuartetFile(file2)
  trees1 <- read.tree(file1)
  trees2 <- read.tree(file2)
  if (class(trees1) != "phylo") {
    stop("file1 must contain a single tree")
  }
  if (length(trees2) < 1) {
    stop("file2 must contain at least one tree")
  }
  matrix(.Call('_Quartet_tqdist_OneToManyQuartetAgreement', 
               as.character(file1), as.character(file2)),
         ncol=2, dimnames=list(NULL, c('A', 'E')))
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
AllPairsQuartetAgreement <- function(file) {
  ValidateQuartetFile(file)
  result <- .Call('_Quartet_tqdist_AllPairsQuartetAgreement', as.character(file));
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