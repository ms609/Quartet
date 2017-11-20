WHICH_OTHER_NODE <- 2:4

Tree2Splits <- function (tr) {
  tip_label <- tr$tip.label
  n_tip <- length(tip_label)
  root <- length(tip_label) + 1
  vapply(phangorn:::bip(tr)[-seq_len(root)], function (x) seq_len(n_tip) %in% x, logical(n_tip))[as.double(tip_label), , drop=FALSE]
}

NumberTips <- function (tr, sorted.labels) {
  tr$tip.label <- match(tr$tip.label, sorted.labels)
  return(tr)
}

## An accellerated version of the R function (x, na.rm = FALSE, dims = 1L) 
ColSums <- function (x, n_cols) .Internal(colSums(x, 4, n_cols, FALSE))

Choices <- memoise(function (n_tips) {
  ret <- unlist(lapply(seq_len(n_tips - 3), function (i) {
    unlist(lapply((i + 1):(n_tips - 2), function (j) {
      unlist(lapply((j + 1):(n_tips - 1), function (k) {
        lapply((k + 1):n_tips, function (l) {
          c(i, j, k, l)
        })
      }), recursive=FALSE)
    }), recursive=FALSE)
  }), recursive=FALSE)
})

QuartetState <- function (tips, bips) {
  tetra_splits <- bips[tips, , drop=FALSE]
  statement <- tetra_splits[, ColSums(tetra_splits, n_cols=dim(tetra_splits)[2]) == 2, drop=FALSE]
  if (length(statement)) {
    statement <- statement[, 1]
    if (statement[1]) return (WHICH_OTHER_NODE[statement[WHICH_OTHER_NODE]])
    if (statement[2]) if (statement[3]) return (4) else return (3)
    return (2)
  } else {
    return (0)
  }
}

QuartetStates <- function (splits) {
  n_tips <- dim(splits[[1]])[1]
  lapply(splits, function (bips) {
    vapply(Choices(n_tips), QuartetState, double(1), bips=bips)
  })
}

CompareQuartets <- function (x, cf) {
  resolved <- as.logical(x) & as.logical(cf)
  c(sum(x[resolved] == cf[resolved]), sum(!resolved))
}

## Given a list of trees, returns the nubmer of quartet statements present in the first tree
## in the list also present in each other tree.
MatchingQuartets <- function (trees) {
  tree1.labels <- trees[[1]]$tip.label
  if (class(tree1.labels) == 'character') trees <- lapply(trees, NumberTips, sorted.labels = tree1.labels)
  quartets <- QuartetStates(lapply(trees, Tree2Splits))
  vapply(quartets[-1], CompareQuartets, cf=quartets[[1]], double(2))
}