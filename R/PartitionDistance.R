#' Compare status of splits
#' 
#' Reports whether splits are present or contradicted in a set of reference
#' splits.
#' 
#' @template splitsParam
#' @param splits2 Splits against which to compare `splits`.
#' 
#' @return A named vector of eight integers, listing the number of unique splits 
#' that:
#' 
#'   - **N**    exist in total; i.e. the number of splits in `splits1` plus the 
#'   number in `splits2`,
#'   equivalent to 2 _s_ + _d1_ + _d2_ + _r1_ + _r2_;
#' 
#'   - **P1**   occur in `splits1`
#'   
#'   - **P2**   occur in `splits2`
#'   
#'   - **s**    occur in both `splits1` and `splits2`; 
#'   
#'   - **d1**   occur in `splits1` but are contradicted by `splits2`;
#'   
#'   - **d2**   occur in `splits2` but are contradicted by `splits1`;
#'   
#'   - **r1**   occur in `splits1` only, being neither present in nor contradicted by `splits2`;
#'   
#'   - **r2**   occur in `splits2` only, being neither present in nor contradicted by `splits1`;
#'   
#'   - **RF**   occur in one tree only; i.e. _d1_ + _d2_ + _r1_ + _r2_,
#'   the Robinson-Foulds distance.
#'
#' @family element-by-element comparisons
#' @seealso Equivalent function for quartets: [`CompareQuartets()`]
#'         
#' @examples 
#' splits1 <- TreeTools::BalancedTree(8)
#' splits2 <- TreeTools::PectinateTree(8)
#' 
#' CompareSplits(splits1, splits2)
#'         
#' @references
#' 
#'  - \insertRef{Estabrook1985}{Quartet}
#' 
#'  - \insertRef{Robinson1981}{Quartet}
#'  
#' @template MRS
#' @name CompareSplits
#' @importFrom TreeTools as.Splits WithoutTrivialSplits %in% CompatibleSplits
#' @export
CompareSplits <- function (splits, splits2) {
  splits <- as.Splits(splits)
  splits2 <- as.Splits(splits2, splits)
  
  nTip <- attr(splits, "nTip")
  
  splits <- unique(WithoutTrivialSplits(splits, nTip))
  splits2 <- unique(WithoutTrivialSplits(splits2, nTip))
  
  duplicates <- splits %in% splits2
  
  nSplits <- length(splits)
  nSplits2 <- length(splits2)
  nTotal <- nSplits + nSplits2
  nBoth <- sum(duplicates)
  
  incompatibles <- !CompatibleSplits(splits, splits2)
  if (length(incompatibles) > 0) {
    d1 <- sum(apply(incompatibles, 1, any))
    d2 <- sum(apply(incompatibles, 2, any))
  } else {
    d1 <- d2 <- 0
  }

  # Return:
  c(N = nTotal, P1 = nSplits, P2 = nSplits2, s = nBoth,
    d1 = d1, d2 = d2,
    r1 = nSplits - nBoth - d1, r2 = nSplits2 - nBoth - d2)
}

#' @rdname CompareSplits
#' @export
CompareBipartitions <- CompareSplits

#' Matching partitions
#' 
#' Calculates how many of the partitions present in tree 1 are also present in 
#' tree 2 (`s`), 
#' how many of the partitions in tree 1 are absent in tree 2 (`d1`),
#' and how many of the partitions in tree 2 are absent in tree 1 (`d2`).
#' The Robinson-Foulds (symmetric partition) distance is the sum of the 
#' latter two quantities, i.e. `d1` + `d2`.
#' 
#' @inheritParams QuartetStatus
#' 
#' @return Returns a two dimensional array. 
#' Rows correspond to the input trees, and are named if names were present.
#' Columns report:
#'   
#'   **N**: The total number of partitions present in the two trees, 
#'   i.e. _P1_ + _P2_.
#'      
#'   **P1**: The number of partitions present in tree 1.
#'   
#'   **P2**: The number of partitions present in tree 2.
#'   
#'   **s**: The number of partitions present in both trees.
#'   
#'   **d1**: The number of partitions present in tree 1,
#'   but contradicted by tree 2.
#'   
#'   **d2**: The number of partitions present in tree 2,
#'   but contradicted by tree 1.
#'   
#'   **r1**: The number of partitions present in tree 1, and neither 
#'   present nor contradicted in tree 2.
#'   
#'   **r2**: The number of partitions present in tree 2, and neither 
#'   present nor contradicted in tree 1.
#'   
#' @family element-by-element comparisons
#'         
#' @examples
#' data("sq_trees")
#' 
#' # Calculate the status of each quartet
#' splitStatuses <- SplitStatus(sq_trees)
#' 
#' # Calculate the raw symmetric difference (i.e. Robinson–Foulds distance)
#' RawSymmetricDifference(splitStatuses)
#' 
#' # Normalize the Robinson Foulds distance by dividing by the number of 
#' # splits present in the two trees:
#' RawSymmetricDifference(splitStatuses) / splitStatuses[, "N"]
#' 
#' # Normalize the Robinson Foulds distance by dividing by the total number of 
#' # splits that it is possible to resolve for `n` tips:
#' nTip <- length(sq_trees[[1]]$tip.label)
#' nPartitions <- 2 * (nTip - 3L) # Does not include the nTip partitions that 
#'                                # comprise but a single tip
#' RawSymmetricDifference(splitStatuses) / nPartitions
#'
#' 
#' @references 
#' - \insertRef{Robinson1981}{Quartet}
#'   
#' - \insertRef{Penny1985}{Quartet}
#' 
#' 
#' @template MRS
#' @importFrom TreeTools RenumberTips as.Splits NTip UnshiftTree
#' @aliases BipartitionStatus
#' @export
SplitStatus <- function (trees, cf = trees[[1]]) {
  compareWithFirst <- identical(cf, trees[[1]])
  if (!compareWithFirst) trees <- UnshiftTree(cf, trees)
  
  treeStats <- NTip(trees)
  if (length(unique(treeStats)) > 1) {
    stop("All trees must have the same number of tips")
  }
  
  splits <- as.Splits(trees)
  ret <- vapply(splits, CompareSplits, splits2 = splits[[1]], double(8))

  # Return:
  if (compareWithFirst) t(ret) else t(ret[, -1])
}

#' @keywords internal
#' @export
BipartitionStatus <- SplitStatus


#' @describeIn SplitStatus Reports split statistics obtained after removing all
#'   tips that do not occur in both trees being compared.
#' @aliases SharedBipartitionStatus
#' @export
SharedSplitStatus <- function (trees, cf) UseMethod("SharedSplitStatus")

#' @export
SharedSplitStatus.list <-  function (trees, cf = trees[[1]]) {
  t(vapply(trees, PairSharedSplitStatus, cf = cf, 
           c(N = 0L, P1 = 0L, P2 = 0L, s = 0L, 
             d1 = 0L, d2 = 0L, r1 = 0L, r2 = 0L)))
}

#' @export
SharedSplitStatus.multiPhylo <- SharedSplitStatus.list

#' @export
SharedSplitStatus.phylo <- function (trees, cf = trees) {
  PairSharedSplitStatus(trees, cf)
}

#' @keywords internal
#' @export
SharedBipartitionStatus <- SharedSplitStatus

#' Pair shared split status
#' 
#' Removes all tips that do not occur in both `ref` and `cf`, then calculates 
#' the status of the remaining splits.
#' 
#' @param ref,cf Trees of class \code{\link[ape:read.tree]{phylo}} to compare.
#' 
#' @return Named integer of length 6, as per [`CompareSplits()`]
#' 
#' @examples
#' 
#' library("TreeTools")
#' ref <- BalancedTree(letters[1:9])
#' cf <- BalancedTree(letters[3:13])
#' 
#' PairSharedSplitStatus(ref, cf)
#' 
#' @keywords internal
#' @importFrom ape drop.tip
#' @importFrom TreeTools RenumberTips as.Splits
#' @template MRS
#' @export
PairSharedSplitStatus <- function (ref, cf) {
  refTips <- ref$tip.label
  cfTips <- cf$tip.label
  
  prunedRef <- drop.tip(ref, setdiff(refTips, cfTips))
  prunedCf <- drop.tip(cf, setdiff(cfTips, refTips))
  
  refSplits <- as.Splits(prunedRef)
  cfSplits <- as.Splits(prunedCf, prunedRef)
  
  # Return:
  CompareSplits(refSplits, cfSplits)
}
