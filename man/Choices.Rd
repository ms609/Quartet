\name{Choices}
\alias{Choices}

\title{Choices}
\description{List all choices of four taxa from a tree.}
\usage{
Choices(n_tips)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{n_tips}{integer, specifying the number of tips in a tree.}
}
\description{
  A more computationally efficient alternative to \code{\link{combn}}.
  Uses \code{\link{memoise}} to make repeated calls faster.
}
\value{
  Returns a list of length \code{choose(n_tips, 4)}, with each entry corresponding to a unique 
  selection of four different integers <= n_tips
}
\author{
Martin R. Smith
}

\seealso{
  \code{\link{combn}}
}

\examples{
  n_tips <- 6
  choice_list <- Choices(n_tips)
  choice_list
  combn(n_tips, 4) # Provides the same information, but for large values of n_tips is significantly slower.
}
