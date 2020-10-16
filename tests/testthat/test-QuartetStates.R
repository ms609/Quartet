context("CompareQuartets.R: QuartetStates()")
#' Closest to tip 1 in each split
#' 
#' @param tips Vector of length four specifying index of tips to consider.
#' @param splits Splits object.
#' @param nTip Integer specifying number of splits in `splits`.
#' @return Raw vector specifying the closest relative of `tips[1]` in each 
#' split
#' @importFrom TreeTools NTip
#' @keywords internal
.Subsplit4 <- function (tips, splits, nTip = NTip(splits)[1]) {
  
  blankMask <- raw((nTip - 1L) %/% 8L + 1L)
  masks <- as.raw(c(1, 2, 4, 8, 16, 32, 64, 128))
  tipMask <- vapply(tips, function (tip) {
    mask <- blankMask
    element <- (tip - 1L) %/% 8L + 1L
    mask[element] <- masks[(tip - 1L) %% 8L + 1L]
    mask
  }, blankMask)
  if (is.null(dim(tipMask))) tipMask <- matrix(tipMask, 1L)
  
  mask12 <- tipMask[, 1] | tipMask[, 2]
  mask13 <- tipMask[, 1] | tipMask[, 3]
  mask14 <- tipMask[, 1] | tipMask[, 4]
  mask23 <- tipMask[, 2] | tipMask[, 3]
  mask24 <- tipMask[, 2] | tipMask[, 4]
  mask34 <- tipMask[, 3] | tipMask[, 4]
  mask <- mask12 | mask34
  
  subSplits <- t(splits) & mask
  
  ret <- as.raw(0L)
  for (i in seq_len(ncol(subSplits))) {
    # Up to twice as fast if we don't remove duplicates
    split <- subSplits[, i]
    if (identical(split, mask12) || identical(split, mask34)) {
      ret <- as.raw(3L)
      break
    } else if (identical(split, mask13) || identical(split, mask24))  {
      ret <- as.raw(2L)
      break
    } else if (identical(split, mask14) || identical(split, mask23))  {
      ret <- as.raw(1L)
      break
    }
  }
  
  # Return:
  ret
}

QuartetStatesTest <- function (splits, asRaw = FALSE) {
  splits <- as.Splits(splits)
  nTip <- NTip(splits)[1]
  allQuartets <- AllQuartets(nTip)
  
  if (is.list(splits)) {
    nQuartets <- ncol(allQuartets)
    return(t(vapply(splits, QuartetStates,
                    if (asRaw) raw(nQuartets) else integer(nQuartets),
                    asRaw = asRaw)))
  }
  
  ret <- apply(allQuartets, 2, .Subsplit4, unname(splits), nTip)
  
  # Return:
  if (asRaw) {
    ret 
  } else {
    as.integer(ret)
  }
}

test_that('QuartetStates() works', {
  library(TreeTools)
  expect_equal(QuartetStatesTest(BalancedTree(9)), QuartetStates(BalancedTree(9)))
})
