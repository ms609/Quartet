BLANK_SPLIT <- double(6)
names(BLANK_SPLIT) <- c('cf', 'ref', 'cf_and_ref', 'cf_not_ref', 'ref_not_cf',
  'RF_dist')

#' Drop Single Splits
#' 
#' Removes splits that pertain only to a single taxon from a splits object.
#' 
#' Bipartition splits are divisions, implied by each edge or node of an unrooted
#' tree topology, that divide the taxa into two groups (one of which is a clade).
#' 
#' By default, a list of splits will include those that separate a single taxon
#' (a leaf) from all others.  Such splits are, by definition, present in all 
#' trees that contain that taxon; they are not of interest when comparing trees.
#' This function removes such splits from a list of bipartitions.
#' 
#' @param split A matrix in which each column corresponds to a bipartition split
#' 
#' @return The input matrix, with any columns that separate only a single pendant tip
#'         removed.
#'         
#' @author Martin R. Smith
#' 
#' @export 
DropSingleSplits <- function (split) {
  split[, colSums(split) > 1 & colSums(!split) > 1, drop=FALSE]
}

#' Tree2Splits
#' 
#' Converts a phylogenetic tree to an array of bipartition splits.
#' 
#' @param tr A tree of class \code{\link[ape:read.tree]{phylo}}, with tips bearing integer labels (i.e. tr$tip.label == 1:N).
#' @return Returns a two-dimensional array, with columns corresponding to unique
#'  bipartitions and rows corresponding to tips 1:N.
#'
#' @author Martin R. Smith
#' 
#' @examples Tree2Splits(ape::rtree(6, tip.label=1:6, br=NULL))
#'
#' @importFrom ape reorder.phylo
#' @useDynLib Quartet, .registration = TRUE
#' @export
Tree2Splits <- function (tr) {
  tr <- reorder.phylo(tr, 'postorder')
  tip_label <- tr$tip.label
  n_tip <- as.integer(length(tip_label))
  root <- length(tip_label) + 1
  bipartitions <- phangorn_bipCPP(tr$edge, n_tip)
  ret <- vapply(bipartitions[-seq_len(root)], 
                function (x) seq_len(n_tip) %in% x, 
                logical(n_tip))[seq_len(n_tip), , drop=FALSE]
  rownames(ret) <- tip_label
  
  ret <- UniqueSplits(ret)
  # Return:
  DropSingleSplits(ret)
}
#' @rdname Tree2Splits
#' @export
#' @keywords internal
Tree2Bipartitions <- Tree2Splits


#' Unique Splits
#' 
#' Removes equivalent duplicates from a matrix of bipartitions.
#' 
#' @template splitsParam
#' @param preserveParity Logical specifying whether to preserve the `TRUE` and
#'  `FALSE` status within each split (which takes marginally longer).  If 
#'  `FALSE`, each split will be defined such that taxa in the same partition
#'  as the first element are marked `FALSE`, and other taxa marked `TRUE`.
#'  
#' @return The splits element, with all duplicate splits removed.
#' 
#' @examples 
#'   set.seed(1)
#'   splits <- Tree2Splits(ape::rtree(6, br=NULL))
#'   UniqueSplits(splits, preserveParity=TRUE)
#' 
#' @author Martin R. Smith
#' @export 
UniqueSplits <- function (splits, preserveParity = FALSE) {
  originalParity <- splits[1, ]
  
  # Set all splits to the same parity
  splits[, originalParity] <- !splits[, originalParity]
  
  # Identify duplicates
  duplicates <- duplicated(t(splits))
  
  # Remove duplicates
  ret <- splits[, !duplicates, drop=FALSE]
  
  # Return: 
  if (preserveParity) {
    ret[, originalParity[!duplicates]] <- !ret[, originalParity[!duplicates]]
  }
    
  ret
}

#' Compare Splits
#' 
#' @template splitsParam
#' @param splits2 A matrix of bipartitions against which to compare `splits`.
#'   If row names are present, then all rows present in `splits` must be present
#'   in `splits2`.  If they are absent, then both matrices must have the same
#'   number of rows, and tips will be assumed to be in the same sequence.
#' 
#' @return A named vector of six integers, listing the number of unique splits that
#'   (1) are present in `splits1`; (2) are present in `splits2`; (3) are present
#'   in both trees; (4) are present in `splits` but not `splits2`; (5) are
#'   present in `splits2` but not `splits`; and (6) the sum of the latter two
#'   values, i.e. the Robinson-Foulds distance.
#'         
#' @references {Quartet
#'  \insertRef{Estabrook1985}{Quartet}
#'  \insertRef{Robinson1981}{Quartet}
#' }       
#' @author Martin R. Smith
#' @export
CompareSplits <- function (splits, splits2) {
  tipNames <- rownames(splits)
  if (!is.null(tipNames)) if (!all(tipNames %in% rownames(splits2))) 
    stop ("All taxa named in splits must exist in splits2")
  splits2 <- splits2[tipNames, ]
  
  if (dim(splits)[1] != dim(splits2)[1]) 
    stop("Both splits and splits2 must relate to the same tips")
  
  splits <- DropSingleSplits(splits)
  splits2 <- DropSingleSplits(splits2)
  
  # preserveParity = FALSE will ensure that parity(splits) == parity(splits2)
  splits2 <- UniqueSplits(splits2, preserveParity=FALSE)
  
  duplicates <- duplicated(rbind(t(splits), t(splits2)))
  
  nSplits <- dim(splits)[2]
  nSplits2 <- dim(splits2)[2]
  nBoth <- sum(duplicates)
  
  c(one = nSplits, two = nSplits2, 
    both = nBoth, one_not_two = nSplits - nBoth, 
    two_not_one = nSplits2 - nBoth,
    RF_dist = nSplits + nSplits2 - (2 * nBoth))
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
#'   # Extract just the Robinson Foulds distances
#'   splitStatuses[, 'RF_dist']
#'   
#'   # Normalize the Robinson Foulds distance by dividing by the number of 
#'   # splits (bipartitions) resolved in the reference tree:
#'   splitStatuses[, 'RF_dist'] / splitStatuses[, 'ref']
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
  rownames(ret) <- names(BLANK_SPLIT)
  
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
  t(vapply(trees, PairSharedSplitStatus, cf=cf, BLANK_SPLIT))
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