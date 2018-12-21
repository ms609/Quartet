#' Compare Splits
#' 
#' @template splitsParam
#' @param splits2 A matrix of bipartitions against which to compare `splits`.
#'   If row names are present, then all rows present in `splits` must be present
#'   in `splits2`.  If they are absent, then both matrices must have the same
#'   number of rows, and tips will be assumed to be in the same sequence.
#' 
#' @return A named vector of six integers, listing the number of unique splits that
#'   (_N_) exist in total; i.e. the number of splits in `splits1` plus the number in `splits2`,
#'   equivalent to 2 _s_ + _d1_ + _d2_ + _r1_ + _r2_;
#'   (_s_) occur in both `splits1` and `splits2`; 
#'   (_d1_) occur in `splits1` but are contradicted by `splits2`;
#'   (_d2_) occur in `splits2` but are contradicted by `splits1`;
#'   (_r1_) occur in `splits1` only, being neither present in nor contradicted by `splits2`;
#'   (_r2_) occur in `splits2` only, being neither present in nor contradicted by `splits1`;
#'   (_RF_) the number of splits that occur in one tree only; i.e. _d1_ + _d2_ + _r1_ + _r2_,
#'   the Robinson-Foulds distance.
#'         
#' @references {Quartet
#'  \insertRef{Estabrook1985}{Quartet}
#'  \insertRef{Robinson1981}{Quartet}
#' }       
#' @author Martin R. Smith
#' @importFrom TreeSearch DropSingleSplits UniqueSplits
#' @export
CompareSplits <- function (splits, splits2) {
  tipNames <- rownames(splits)
  if (!is.null(tipNames)) if (!all(tipNames %in% rownames(splits2))) 
    stop ("All taxa named in splits must exist in splits2")
  splits2 <- splits2[tipNames, ]
  
  nTip <- dim(splits)[1]
  if (dim(splits2)[1] != nTip) 
    stop("Both splits and splits2 must relate to the same tips")
  
  splits <- DropSingleSplits(splits)
  splits2 <- DropSingleSplits(splits2)
  
  # preserveParity = FALSE will ensure that parity(splits) == parity(splits2)
  splits2 <- UniqueSplits(splits2, preserveParity=FALSE)
  
  duplicates <- duplicated(rbind(t(splits), t(splits2)))
  
  nSplits <- dim(splits)[2]
  nSplits2 <- dim(splits2)[2]
  nTotal <- nSplits + nSplits2
  nBoth <- sum(duplicates)
  
  InOneOnly <- function (uniques, otherSplits) {
    if (length(uniques) == 0) return (0L)
    splitSizes <- colSums(otherSplits)
    sum(apply(uniques, 2, function (x) {
      all(colSums(x & otherSplits) == splitSizes | colSums(x | otherSplits) == splitSizes |
            colSums(!x & otherSplits) == splitSizes | colSums(!x | otherSplits) == splitSizes)
    }))
  }
  
  r1 <- if (nTip - nSplits2 == 3L) 0L else { # Can't be resolved in 1 only if 2 is perfectly resolved.
    InOneOnly(splits[, !duplicated(rbind(t(splits2), t(splits)))[-seq_len(nSplits2)], drop=FALSE], splits2)
  }
  r2 <- if(nTip - nSplits == 3L) 0L else {
    InOneOnly(splits2[, !duplicates[-seq_len(nSplits)], drop=FALSE], splits)
  }
  
  # Return:
  c(N = nTotal, P1 = nSplits, P2 = nSplits2, s = nBoth,
    d1 = nSplits - nBoth - r1, d2 = nSplits2 - nBoth - r2,
    r1 = r1, r2 = r2)
}
#' @rdname CompareSplits
#' @export
#' @keywords internal
CompareBipartitions <- CompareSplits

#' Matching partitions
#' 
#' Calculates how many of the partitions present in tree A are also present in 
#' tree B, how many of the partitions in tree A are absent in tree B, and how
#' many of the partitions in tree B are absent in tree A.  The Robinson-Foulds
#' (symmetric partition) distance is the sum of the latter two quantities.
#' 
#' @template treesParam
#' @template treesCfParam
#' 
#' @return Returns a two dimensional array. 
#'         Rows correspond to the input trees.
#'         Columns report the number of partitions that :
#'         1: are present in the comparison tree and the corresponding input tree;
#'         2: are unresolved in (at least) one of the comparison tree and the corresponding 
#'         input tree.
#'         
#' @seealso
#'   * [QuartetStatus]: Uses quartets rather than bipartition splits as the unit
#'     of similarity.
#'         
#' @examples{
#'   data('sq_trees')
#'   
#'   # Calculate the status of each quartet
#'   splitStatuses <- SplitStatus(sq_trees)
#'   
#'   # Calculate the Robinson Foulds distances
#'   RobinsonFoulds(splitStatuses)
#'   
#'   # Normalize the Robinson Foulds distance by dividing by the number of 
#'   # splits (bipartitions) resolved in tree 1:
#'   RobinsonFoulds(splitStatuses) / splitStatuses[, 'P1']
#'   
#'   # Normalize the Robinson Foulds distance by dividing by the total number of 
#'   # splits (bipartitions) that it is possible to resolve for `n` tips:
#'   nTip <- length(sq_trees[[1]]$tip.label)
#'   nPartitions <- (nTip - 3L) # Does not include the nTip partitions that 
#'                              # comprise but a single tip
#'   splitStatuses[, 'RF_dist'] / nPartitions
#'   
#' }
#' 
#' @references {
#'   \insertRef{Robinson1981}{Quartet}
#'   
#'   \insertRef{Penny1985}{Quartet}
#' }
#' @author Martin R. Smith
#' @importFrom TreeSearch RenumberTips Tree2Splits
#' @export
SplitStatus <- function (trees, cf=trees[[1]]) {
  if (!is.null(cf)) trees <- UnshiftTree(cf, trees)
  
  treeStats <- vapply(trees, function (tr)
    c(tr$Nnode, length(tr$tip.label)), double(2))
  if (length(unique(treeStats[2, ])) > 1) {
    stop("All trees must have the same number of tips")
  }
  tree1Labels <- trees[[1]]$tip.label
  trees <- lapply(trees, RenumberTips, tipOrder = tree1Labels)
  splits <- lapply(trees, Tree2Splits)
  ret <- vapply(splits, CompareSplits, splits2=splits[[1]], double(6))
  rownames(ret) <- c('N', 'P1', 'P2', 's', 'd1', 'd2', 'r1', 'r2')
  
  # Return:
  if (is.null(cf)) t(ret) else t(ret[, -1])
}

#' @rdname SplitStatus
#' @export
#' @keywords internal
BipartitionStatus <- SplitStatus


#' @describeIn SplitStatus Reports split statistics obtained after removing all
#'   tips that do not occur in both trees being compared.
#' @export
SharedSplitStatus <- function (trees, cf=trees[[1]]) {
  t(vapply(trees, PairSharedSplitStatus, cf=cf, c(N = 0L, s = 0L, d1 = 0L, d2 = 0L, r1 = 0L, r2 = 0L)))
}
#' @rdname SplitStatus
#' @export
#' @keywords internal
SharedBipartitionStatus <- SharedSplitStatus

#' Pair shared split status
#' 
#' Removes all tips that do not occur in both `ref` and `cf`, then calculates 
#' the status of the remaining splits
#' 
#' @param ref,cf Trees of class phylo to compare.
#' 
#' @return Named integer of length 6, as per [CompareSplits]
#' #' 
#' @keywords internal
#' @importFrom ape drop.tip
#' @author Martin R. Smith
#' @export
PairSharedSplitStatus <- function (ref, cf) {
  refTips <- ref$tip.label
  cfTips <- cf$tip.label
  
  prunedRef <- drop.tip(ref, setdiff(refTips, cfTips))
  prunedCf <- drop.tip(cf, setdiff(cfTips, refTips))
  prunedCf <- RenumberTips(prunedCf, tipOrder = intersect(refTips, cfTips))
  
  refSplits <- Tree2Splits(prunedRef)
  cfSplits <- Tree2Splits(prunedCf)
  ret <- CompareSplits(refSplits, cfSplits)
  
  # Return:
  ret
}