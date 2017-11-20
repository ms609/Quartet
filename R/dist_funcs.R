WHICH_OTHER_NODE <- 2:4

Tree2Splits <- function (tr) {
  tip_label <- tr$tip.label
  n_tip <- length(tip_label)
  root <- length(tip_label) + 1
  vapply(phangorn:::bip(tr)[-seq_len(root)], function (x) seq_len(n_tip) %in% x, logical(n_tip))[as.double(tr$tip.), ]
}

## An accellerated version of the R function (x, na.rm = FALSE, dims = 1L) 
ColSums <- function (x, n_cols) .Internal(colSums(x, 4, n_cols, FALSE))

QuartetStates <- function (tips, bips) {
  quartets <- bips[tips, ]
  statement <- quartets[, ColSums(quartets, n_cols=dim(quartets)[2]) == 2, drop=FALSE]
  if (length(statement)) {
    statement <- statement[, 1]
    if (statement[1]) return (WHICH_OTHER_NODE[statement[WHICH_OTHER_NODE]])
    if (statement[2]) if (statement[3]) return (4) else return (3)
    return (2)
  } else {
    return (0)
  }
}

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

Splits2Quartets <- function (splits) {
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
QuartetMatch <- function (trees) {
  quartets <- Splits2Quartets(lapply(trees, Tree2Splits))
  vapply(quartets, CompareQuartets, cf=quartets[[1]], double(2))
}
