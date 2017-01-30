\name{Tree2Splits}
\alias{Tree2Splits}

\title{Tree to Splits}
\description{Converts a phylogenetic tree to an array of bipartition splits.}
\usage{
  Tree2Splits(tr)
}

\arguments{
  \item{tr}{A tree of class \code{phylo}, with tips bearing integer labels (i.e. tr$tip.label == 1:N);}
}
\details{}
\value{Returns a two-dimensional array, with columns corresponding to bipartitions and rows corresponding
to tips 1:N.}
\author{
Martin R. Smith
}

\seealso{
\code{phangorn:::bip}
}
\examples{
  library(ape)
  Tree2Splits(rtree(6, tip.label=1:6, br=NULL))
}