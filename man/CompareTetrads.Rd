\name{CompareTetrads}
\alias{CompareTetrads}

\title{Compare tetrad states between trees}
\description{
  Compares two lists of tetrad states, detailing how many are identical and how many 
  are unresolved.
}
\usage{
CompareTetrads(x, cf)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{a list of tetrad states, generated using \code{\link{Splits2Tetrads}};}
  \item{cf}{a second such list.}
}
\details{
  Compares each tetrad in a list, calculating how many statements are identical in both lists.
}
\value{
  Returns an array of two elements:
  1: the number of tetrads that are present in both trees
  2: the number of tetrads that are not resolved in both trees
  The number of tetrads that are present in one tree and contradicted in the other can be calcluated
  by deducting the total from the total number of tetrads, \code{choose(n_tip, 4)}.
}
\author{
Martin R. Smith
}
\seealso{
  \code{\link{TetradMatch}}, generates this output from a list of trees.
}
\examples{
  n_tip <- 6
  trees <- list(rtree(n_tip, tip.label=seq_len(n_tip), br=NULL),
                rtree(n_tip, tip.label=seq_len(n_tip), br=NULL))
  splits <- lapply(trees, Tree2Splits)
  tetrads <- Splits2Tetrads(splits)
  compare_result <- CompareTetrads(tetrads[[1]], tetrads[[2]])
  dissimilar_tetrads <- choose(n_tip, 4) - sum(compare_result)  
  result <- c(compare_result, dissimilar_tetrads)
  names(result) <- c('Shared', 'Unresolved', 'Dissimilar')
  result
}
