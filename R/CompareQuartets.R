#' List all quartets
#'
#' Lists all choices of four taxa from a tree.
#'  
#' A more computationally efficient alternative to \code{\link[utils]{combn}},
#' `AllQuartets` uses \code{\link[memoise]{memoise}} to make repeated calls faster.
#'
#' @param n_tips Integer, specifying the number of tips in a tree.
#' 
#' @return `AllQuartets()` returns a list of length \code{choose(n_tips, 4)}, 
#' with each entry corresponding to a unique selection of four different
#' integers less than or equal to `n_tips`.
#' 
#' @template MRS
#'
#' @family quartet counting functions
#' @seealso \code{\link[utils]{combn}}
#' 
#' @examples
#'  n_tips <- 6
#'  AllQuartets(n_tips)
#'  
#'  combn(n_tips, 4) # Provides the same information, but for large 
#'                   # values of n_tips is significantly slower.
#' 
#' @importFrom memoise memoise
#' @export
AllQuartets <- memoise(function (n_tips) {
  unlist(lapply(seq_len(n_tips - 3), function (i) {
    unlist(lapply((i + 1):(n_tips - 2), function (j) {
      unlist(lapply((j + 1):(n_tips - 1), function (k) {
        lapply((k + 1):n_tips, function (l) {
          c(i, j, k, l)
        })
      }), recursive=FALSE)
    }), recursive=FALSE)
  }), recursive=FALSE)
})

#' Quartet State(s)
#' 
#' Report the status of the specified quartet(s).
#' 
#' One of the three possible four-taxon trees will be consistent with any set of
#' splits generated from a fully resolved tree.  If the taxa are numbered 
#' 1 to 4, this tree can be identified by naming the tip most closely related 
#' to taxon 1.
#' If a set of splits is generated from a tree that contains polytomies, 
#' it is possible that all three four-taxon trees are consistent with the set
#' of splits
#'
#' @param tips A four-element array listing a quartet of tips, either by their
#'             number (if class `numeric`) or their name (if class `character`).
#' @param splits An object that can be induced to a `Splits` object using
#'   \code{\link[TreeTools]{as.Splits}}.
#' @param bips Depreciated; included for compatibility with v1.0.2 and below.
#' @param asRaw Logical specifying whether return format should be `raw`,
#' which uses less memory and can be processed faster than `integer` type.
#' Default is currently set to `FALSE` for backwards compatability; suggest
#' overriding to `TRUE`.
#'
#' @return `QuartetState()` returns `0` if the relationships of the four taxa 
#' are not constrained by the provided splits, or the index of the closest
#' relative to `tips[1]`, otherwise.
#'
#' @template MRS
#' 
#' @family element-by-element comparisons
#' @seealso \code{\link{CompareQuartets}}, used to compare quartet states between
#'   trees.
#' @examples{
#'   nTip <- 6
#'   trees <- list(ape::rtree(nTip, tip.label=seq_len(nTip), br=NULL),
#'                 ape::rtree(nTip, tip.label=seq_len(nTip), br=NULL))
#'   
#'   trees[[3]] <- TreeTools::CollapseNode(trees[[2]], 9:10)
#'   
#'   QuartetState(c(1, 3, 4, 6), trees[[2]])  
#'   QuartetState(1:4, trees[[1]]) == QuartetState(1:4, trees[[2]])
#'   QuartetState(c(1, 3, 4, 6), trees[[3]])  
#'   
#'   QuartetStates(trees[[2]])
#'   QuartetStates(trees[[3]])
#'   
#' }
#' 
#' @references 
#'   \insertRef{Estabrook1985}{Quartet}
#' 
#' @importFrom TreeTools Subsplit as.Splits
#' @export
QuartetState <- function (tips, bips, splits = bips, asRaw = FALSE) {
  statement <- Subsplit(as.Splits(splits), tips, keepAll = FALSE, 
                        unique = TRUE)[1]
  ret <- if (statement == 0L) {
    0L
  } else if (statement == 3L || statement == 12L) {
    2L
  } else if (statement == 5L || statement == 10L) {
    3L
  } else {
    4L
  }
  
  # Return:
  if(asRaw) as.raw(ret) else ret
}

#' @describeIn QuartetState A convenience wrapper that lists the status of all
#' possible quartets for a given `Splits` object.
#'        
#' @importFrom TreeTools as.Splits NTip
#' @export
QuartetStates <- function (splits, asRaw = FALSE) {
  splits <- as.Splits(splits)
  nTip <- NTip(splits)[1]
  allQuartets <- AllQuartets(nTip)
  
  if (mode(splits) == 'list') {
    nQuartets <- length(allQuartets)
    return(t(vapply(splits, QuartetStates, 
                  if (asRaw) raw(nQuartets) else integer(nQuartets),
                  asRaw = asRaw)))
  }
  
  ret <- vapply(allQuartets, .Subsplit4, raw(1L), unname(splits), nTip)
  
  # Return:
  if (asRaw) {
    ret 
  } else {
    as.integer(ret)
  }
}

#' Closest to tip 1 in each split
#' 
#' @param tips Vector of length four specifying index of tips to consider.
#' @param splits Splits object.
#' @param nTip Integer specifying number of splits in `splits`.
#' @return Vector of raws specifying the closest relative of `tips[1]` in each 
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
  
  subSplits <- splits & mask
  
  ret <- as.raw(0L)
  for (i in seq_len(nrow(subSplits))) {
    # Up to twice as fast if we don't remove duplicates
    split <- subSplits[i, ]
    if (identical(split, mask12) || identical(split, mask34)) {
      ret <- as.raw(2L)
      break
    } else if (identical(split, mask13) || identical(split, mask24))  {
      ret <- as.raw(3L)
      break
    } else if (identical(split, mask14) || identical(split, mask23))  {
      ret <- as.raw(4L)
      break
    }
  }
  
  # Return:
  ret
}


#' Compare quartet states by explicit enumeration
#' 
#' Uses explicit enumeration to compare two lists of quartet states, 
#' detailing how many are identical and how many are unresolved.
#' For most purposes, the faster function \code{\link{QuartetStatus}} will be preferable.
#' 
#' @param x,cf List of quartet states, perhaps generated by
#'  \code{\link{QuartetStates}}.
#'
#' @templateVar intro Returns an array of seven numeric elements, corresponding to the quantities of Estabrook _et al_. (1985):
#' @template returnEstabrook
#' 
#' @template MRS
#'
#' @family element-by-element comparisons
#' @seealso \code{\link{QuartetStatus}}, generates this output from a list of
#'  trees.
#'
#' @examples
#'   n_tip <- 6
#'   trees <- list(ape::rtree(n_tip, tip.label=seq_len(n_tip), br=NULL),
#'                 ape::rtree(n_tip, tip.label=seq_len(n_tip), br=NULL))
#'   quartets <- QuartetStates(trees)
#'   CompareQuartets(quartets[[1]], quartets[[2]])
#' 
#'@references {
#' \insertRef{Estabrook1985}{Quartet}
#'}
#' 
#' @export
CompareQuartets <- function (x, cf) {
  x_resolved <- as.logical(x)
  cf_resolved <- as.logical(cf)
  both_resolved <- x_resolved & cf_resolved
  n_both_resolved <- sum(both_resolved)
  n_same <- sum(x[both_resolved] == cf[both_resolved])
  c(
    N = 2L * length(x),
    Q = length(x),
    s = n_same,
    d = n_both_resolved - n_same,
    r1 = sum(x_resolved) - n_both_resolved,
    r2 = sum(cf_resolved) - n_both_resolved,
    u = sum(!x_resolved & !cf_resolved)
  )
}

#' Compare one tree's quartets against many others
#' 
#' Count how many quartets in one tree are resolved in the same way or 
#' different ways in a forest of comparison trees.
#' 
#' This function relies on explicitly enumerating each quartet in each tree.
#' As such its runtime will increase hyper-exponentially with the number of 
#' leaves in trees being compared.  30 leaves will take around 5 seconds; 
#' 40 closer to 20 s, and 50 around a minute.
#' 
#' @param x Tree of interest
#' @param cf Comparison tree of class `phylo`, or list thereof, each with the 
#' same leaves as `x`.
#' 
#' @return `UniqueQuartets()` returns a named integer vector specifying the
#' number of quartets whose resolution in `x` matches all or any of the
#' resolutions in `cf`.
#' 
#' Named elements are:
#' 
#'  \describe{
#'   \item{N}{The total number of quartet _statements_ for the given number of
#'    _n_-leaf trees, i.e. _n_trees_ &times; _Q_.}
#'   \item{Q}{The total number of quartets for _n_ leaves.}
#'   \item{s_all}{The number of quartets that are resolved identically in all 
#'   trees.}
#'   \item{s_any}{The number of quartets that are resolved in `x`, and 
#'   identically in at least one of `cf`.}
#'   \item{d_all}{The number of quartets that are resolved in every tree in 
#'            `cf`, but never in the same way as they are resolved in in `x`.}
#'   \item{d_any}{The number of quartets in `x` that are resolved differently
#'   (i.e. contradicted) in at least one tree in `cf`.}
#'   \item{r1_all}{The number of quartets that are resolved in `x`, but not in 
#'   any of `cf`.}
#'   \item{r1_any}{The number of quartets that are resolved in `x`, but 
#'   unresolved in at least one of `cf`.}
#'   \item{r2_all}{The number of quartets that are resolved in all of `cf`,
#'    but not in `x`.}
#'   \item{r2_any}{The number of quartets that are resolved in at least one of `cf`,
#'    but not in `x`.}
#'   \item{u_all}{The number of quartets that are unresolved in all trees.}
#'   \item{u_any}{The number of quartets that are unresolved in `x` and
#'   at least one tree in `cf`.}
#'   \item{x_only}{The number of quartets in `x` that are not resolved the 
#'   same way in any of `cf`.}
#' }
#' 
#' @template MRS
#' @family element-by-element comparisons
#' 
#' @examples 
#' library('TreeTools')
#' CompareQuartetsMulti(x  = CollapseNode(as.phylo(42, 6), 8:9),
#'                      cf = list(BalancedTree(6), PectinateTree(6), 
#'                                CollapseNode(as.phylo(1337, 6), 9:10)))
#' 
#' @importFrom TreeTools NTip RenumberTips
#' @export
CompareQuartetsMulti <- function (x, cf) {
  
  input <- QuartetStates(x, asRaw = TRUE)
  if (inherits(cf, 'phylo')) {
    cf <- list(cf)
  }
  xLabels <- x$tip.label
  xSorted <- sort(xLabels)
  cfTips <- vapply(cf, NTip, 0L)
  if (!all(cfTips == length(xLabels))) {
    stop("All trees must contain the same number of leaves.")
  }
  if (!all(apply(sapply(cf, getElement, 'tip.label'), 2L, function (lab)
    identical(sort(lab), xSorted)))) {
    stop("All trees must contain the same tip labels.")
  }
  
  cf <- lapply(cf, RenumberTips, xLabels)
  
  nCf <- length(cf)
  comparison <- vapply(cf, QuartetStates, input, asRaw = TRUE)
  
  xResolved <- as.logical(input)
  cfResolved <- comparison != as.raw(0)
  cfAnyResolved <- as.logical(rowSums(cfResolved))
  cfAllResolved <- rowSums(cfResolved) == nCf
  
  equal <- comparison == input
  same <- rowSums(equal)
  anySame <- as.logical(same)
  allSame <- same == nCf
  different <- rowSums(!equal & cfResolved)
  
  # Return:
  c(
    N = (length(cf) + 1L) * length(input),
    Q = length(input),
    s_all = sum(xResolved & allSame), # Same in all cf
    s_any = sum(xResolved & anySame), # Same in at least one of cf
    d_all = sum(xResolved & !anySame & cfAllResolved), # Resolved differently in all cf
    d_any = sum(xResolved & different), # Resolved differently in at least one cf
    r1_all = sum(xResolved & !cfAnyResolved),
    r1_any = sum(xResolved & !cfAllResolved),
    r2_all = sum(!xResolved & cfAllResolved),
    r2_any = sum(!xResolved & cfAnyResolved),
    u_all = sum(!xResolved & allSame),
    u_any = sum(!xResolved & anySame),
    x_only = sum(xResolved & !anySame)
  )
}