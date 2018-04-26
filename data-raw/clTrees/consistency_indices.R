# Determine the consistency index for each simulated dataset

data("congreveLamsdellMatrices")
require('phangorn')
LowerBound <- function (x) {
  tip <- names(x)
  att <- attributes(x)
  nc  <- att$nc
  nr  <- att$nr
  contrast <- att$contrast
  rownames(contrast) <- att$allLevels
  colnames(contrast) <- att$levels
  attr(x, "weight") <- rep(1, nr)
  attr(x, "index") <- NULL
  y <- as.character(x)
  states <- apply(y, 2, unique.default)
  n.states <- if (nr == 1) {
    length(states)
  } else if (class(states) == 'matrix') {
    rep(nrow(states), ncol(states))
  } else {
    sapply(states, length)
  }
  n.states - 1
}

ConsistencyIndex <- function (tree, data) {
  pscore <- fitch(tree, data)
  weight <- attr(data, "weight")
  data <- subset(data, tree$tip.label)
  m <- LowerBound(data)
  sum(m * weight) / pscore
}

clCI <- vapply(congreveLamsdellMatrices,
               function (dat) ConsistencyIndex(referenceTree, dat),
               double(1))
names(clCI) <- FILE_NUMS


devtools::use_data(clCI, overwrite=TRUE)

