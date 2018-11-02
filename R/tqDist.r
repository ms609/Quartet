#' Triplet and quartet distances with tqDist
#' 
#' Functions to calculate triplet and quartet distances between pairs of trees.
#' 
#' @param file,file1,file2 Paths to files containing a tree or trees in Newick format.
#' 
#' @return The distance between the requested trees.
#' 
#' @author Martin R. Smith, after Andreas Sand
#' 
#' @references \insertRef{Sand2014}{Quartet}
#' @export
QuartetDistance <- function(file1, file2) {
  ValidateQuartetFile(file1)
  ValidateQuartetFile(file2)
  .Call('_Quartet_tqdist_QuartetDistance', as.character(file1), as.character(file2));
}

#' @export
#' @describeIn QuartetDistance Distance between pairs
PairsQuartetDistance <- function(file1, file2) {
  ValidateQuartetFile(file1)
  ValidateQuartetFile(file2)
  .Call('_Quartet_tqdist_PairsQuartetDistance', as.character(file1), as.character(file2));
}

#' @export
#' @describeIn QuartetDistance Distance between all pairs
AllPairsQuartetDistance <- function(file) {
  ValidateQuartetFile(file)
  .Call('_Quartet_tqdist_AllPairsQuartetDistance', as.character(file));
}

#' @export
#' @describeIn QuartetDistance Triplet distance between trees
TripletDistance <- function(file1, file2) {
  ValidateQuartetFile(file1)
  ValidateQuartetFile(file2)
  .Call('_Quartet_tqdist_TripletDistance', as.character(file1), as.character(file2));
}

#' @export
#' @describeIn QuartetDistance Triplet distance between pairs
PairsTripletDistance <- function(file1, file2) {
  ValidateQuartetFile(file1)
  ValidateQuartetFile(file2)
  .Call('_Quartet_tqdist_PairsTripletDistance', as.character(file1), as.character(file2));
}

#' @export
#' @describeIn QuartetDistance Triplet distance between all pairs
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