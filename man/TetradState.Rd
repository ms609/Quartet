\name{TetradState}
\alias{TetradState}

\title{State of tetrads}
\description{Report the status of a given tetrad.}
\usage{
TetradState(tips, bips)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{tips}{A four-element array listing the tips of the four-taxon tree (tetrad);}
  \item{bips}{bipartitions to evaluate.}
}
\details{
  One of the three possible four-taxon trees will be consistent with any set of bipartitions 
  generated from a fully resolved tree.  If the taxa are numbered 1 to 4, this tree can be 
  identified by naming the sister taxon to taxon 1.
  If a set of bipartitions is generated from a tree that contains polytomies, it is possible
  that all three three four-taxon trees are consistent with the set of bipartitions.
}
\value{
  Returns 0 if the relationships of the four taxa are not constrained by the provided 
  bipartitions, or the index of the closest relative to tips[1], otherwise.
}
\author{
Martin R. Smith
}
\seealso{
  \code{\link{CompareTetrads}}, used to compare sets of bipartitions
}
\examples{
  n_tip <- 6
  library(ape)
  trees <- list(rtree(n_tip, tip.label=seq_len(n_tip), br=NULL),
                rtree(n_tip, tip.label=seq_len(n_tip), br=NULL))
  splits <- lapply(trees, Tree2Splits)
  TetradState(c(1, 3, 4, 6), splits[[2]])  
  TetradState(1:4, splits[[1]]) == TetradState(1:4, splits[[2]])
  vapply(Choices(n_tip), TetradState, bips=splits[[1]], double(1))
}
