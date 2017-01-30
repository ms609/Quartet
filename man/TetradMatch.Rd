\name{TetradMatch}
\alias{TetradMatch}

\title{Count matching tetrads}
\description{Determines the number of four-taxon trees consistent with multiple cladograms}
\usage{
  TetradMatch(trees)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{trees}{A list of trees of class \code{\link{phylo}}. Each tree's tips must be numbered
               using the integers 1:n_tip.}
}
\details{
  At present the trees must bear the same number of tips, and each tree$tip.label must use
  the integers 1:n_tip.  Support for different-sized trees and real-named taxa will be added 
  in case of demand; contact the author if you would appreciate this functionality.
}
\value{Returns a two dimensional array; columns correspond to the input trees; rows report the 
       number of four-taxon trees that : 1, are present in trees[[1]] and the corresponding input tree;
       2: are unresolved in (at least) one of trees[[1]] and the corresponding input tree. Tetrads
       that DIFFER between the two relevant trees can be calculated by deducting the tetrads in
       of the other two categories from the total number of tetrads, 
       given by \code{\link{choose}(n_tip, 4)}.
}
\author{
Martin R. Smith
}
\examples{
  n_tip <- 6
  trees <- list(rtree(n_tip, tip.label=seq_len(n_tip), br=NULL),
                rtree(n_tip, tip.label=seq_len(n_tip), br=NULL))
  compare_result <- TetradMatch(trees)
  dissimilar_tetrads <- choose(n_tip, 4) - colSums(compare_result)  
  result <- rbind(compare_result, dissimilar_tetrads)
  rownames(result) <- c('Shared', 'Unresolved', 'Dissimilar')
  result
}