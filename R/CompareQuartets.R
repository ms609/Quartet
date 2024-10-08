#' List all quartets
#'
#' Lists all choices of four taxa from a tree.
#'  
#' A more computationally efficient alternative to \code{\link[utils]{combn}}.
#'
#' @param nTips Integer, specifying the number of tips in a tree; or a tree,
#' whose tips will be counted.
#' 
#' @return `AllQuartets()` returns a matrix with four rows and 
#' \code{choose(n_tips, 4)} columns, with each column corresponding to a unique
#' selection of four different integers less than or equal to `nTips`.
#' 
#' @template MRS
#'
#' @family quartet counting functions
#' @seealso 
#' States of quartets in given trees: [`QuartetStates()`]
#' 
#' @examples
#' AllQuartets(5)
#'  
#' combn(5, 4) # Provides the same information, but for large 
#'             # values of n_tips is significantly slower.
#' 
#' @export
AllQuartets <- function (nTips) UseMethod("AllQuartets")

#' @rdname AllQuartets
#' @export
AllQuartets.numeric <- function (nTips) all_quartets(nTips)

#' @rdname AllQuartets
#' @importFrom TreeTools NTip
#' @export
AllQuartets.phylo <- function (nTips) AllQuartets(NTip(nTips))


#' Quartet State(s)
#' 
#' Report the status of the specified quartet(s) in given trees or lists of 
#' splits \insertCite{Estabrook1985}{Quartet}.
#' 
#' One of the three possible four-leaf trees will be consistent with any set of
#' splits generated from a fully resolved tree.  If the leaves are numbered 
#' 1 to 4, this tree can be identified by naming the leaf most closely related 
#' to leaf 4.
#' If a set of splits is generated from a tree that contains polytomies, 
#' it is possible that all three four-leaf trees are consistent with the set
#' of splits.
#'
#' @param tips A four-element array listing a quartet of leaves, either by their
#'             number (if class `numeric`) or their name (if class `character`).
#' @param splits An object, such as a tree of class `phylo`, that can be
#'  induced to a `Splits` object using \code{\link[TreeTools]{as.Splits}}.
#' @param bips Deprecated; included for compatibility with v1.0.2 and below.
#' @param asRaw Logical specifying whether return format should be `raw`,
#' which uses less memory and can be processed faster than `integer` type.
#' Default is currently set to `FALSE` for backwards compatibility; suggest
#' overriding to `TRUE`.
#'
#' @return `QuartetState()` returns `0` if the relationships of the four leaves
#' are not constrained by the provided splits, or the index of the closest
#' relative to `tips[4]`, otherwise.
#'
#' @template MRS
#' 
#' @family element-by-element comparisons
#' @seealso Compare quartet states between trees (slowly) using 
#' [`CompareQuartets()`] and [`CompareQuartetsMulti()`].
#' 
#' @examples
#' trees <- list(TreeTools::BalancedTree(6),
#'               TreeTools::PectinateTree(6))
#' 
#' trees[[3]] <- TreeTools::CollapseNode(trees[[2]], 9:10)
#' 
#' QuartetState(c(1, 3, 4, 6), trees[[2]])  
#' QuartetState(1:4, trees[[1]]) == QuartetState(1:4, trees[[2]])
#' QuartetState(c(1, 3, 4, 6), trees[[3]])  
#' 
#' QuartetStates(trees[[2]])
#' QuartetStates(trees[[3]])
#' 
#' CompareQuartets(QuartetStates(trees[[2]]), QuartetStates(trees[[3]]))
#' CompareQuartetsMulti(trees[[1]], trees[2:3])
#' 
#' @references \insertAllCited{}
#' 
#' @importFrom TreeTools Subsplit as.Splits
#' @export
QuartetState <- function (tips, bips, splits = bips, asRaw = FALSE) {
  if ((is.character(tips) && !all(tips %in% TipLabels(splits)))) {
    ret <- 0L
  } else if (is.numeric(tips) && max(tips) > NTip(splits)) {
    warning("`splits` contains ", NTip(splits), " leaves but `tips` includes \"",
            max(tips), "\"")
    ret <- 0L
  } else {
    statement <- Subsplit(as.Splits(splits), tips, keepAll = FALSE, 
                          unique = TRUE)[1]
    ret <- if (statement == 0L) {
      0L
    } else if (statement == 3L || statement == 12L) {
      3L
    } else if (statement == 5L || statement == 10L) {
      2L
    } else {
      1L
    }
  }
  
  # Return:
  if(asRaw) as.raw(ret) else ret
}

#' @rdname QuartetState
#' @importFrom TreeTools as.Splits NTip
#' @return `QuartetStates()` returns a raw vector listing the status of each 
#' quartet of leaves (in the order listed by [`AllQuartets()`]) in turn,
#' or if multiple trees are provided, a matrix in which each row corresponds
#' to such a vector.
#' @export
QuartetStates <- function (splits, asRaw = FALSE) UseMethod("QuartetStates")

#' @export
QuartetStates.phylo <- function (splits, asRaw = FALSE) {
  splits <- as.Splits(splits)
  # Treating most balanced splits first saves 20% of runtime compared to 
  # least balanced first.  BUT ordering by split balance takes 30% of runtime!
  ret <- quartet_states(splits)
  
  # Return:
  if (asRaw) {
    ret 
  } else {
    as.integer(ret)
  }
}

#' @rdname QuartetState
#' @export
QuartetStates.Splits <- QuartetStates.phylo

#' @rdname QuartetState
#' @export
QuartetStates.list <- function (splits, asRaw = FALSE) {
  splits <- as.Splits(splits)
  nTip <- NTip(splits)[1]
  
  nQuartets <- choose(nTip, 4)
  return(t(vapply(splits, QuartetStates, 
                  if (asRaw) raw(nQuartets) else integer(nQuartets),
                  asRaw = asRaw)))
}

#' @rdname QuartetState
#' @export
QuartetStates.multiPhylo <- QuartetStates.list


#' Compare quartet states by explicit enumeration
#' 
#' `CompareQuartets()` uses explicit enumeration to compare two lists of 
#' quartet states  \insertCite{Estabrook1985}{Quartet},
#' detailing how many are identical and how many are unresolved.
#' For most purposes, the faster function [`QuartetStatus()`] will be
#' preferable.
#' 
#' @param x,cf List of quartet states, perhaps generated by [`QuartetStates()`].
#'
#' @templateVar intro Returns an array of seven numeric elements, corresponding
#' to the quantities of Estabrook _et al_. (1985):
#' @template returnEstabrook
#' 
#' @template MRS
#'
#' @family element-by-element comparisons
#' @family quartet counting functions
#' 
#' @seealso
#' - [`QuartetStatus()`] generates the same output from a list of trees.
#'
#' @examples
#'   trees <- list(TreeTools::BalancedTree(6),
#'                 TreeTools::PectinateTree(6))
#'   quartets <- QuartetStates(trees)
#'   CompareQuartets(quartets[[1]], quartets[[2]])
#' 
#' @references \insertAllCited{}
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

#' Compare one tree's quartets against others'
#' 
#' `CompareQuartetsMulti()` counts how many quartets in one tree are resolved
#' in the same way or different ways in a forest of comparison trees.
#' 
#' `CompareQuartetsMulti()` explicitly evaluates each quartet in each tree.
#' As such its runtime will increase hyper-exponentially with the number of 
#' leaves in trees being compared.  30 leaves will take around 5 seconds; 
#' 40 closer to 20 s, and 50 around a minute.
#' 
#' @param x Object of class `phylo` representing the tree of interest.
#' @param cf Comparison tree of class `phylo`, or list thereof, each with the 
#' same leaves as `x`.
#' 
#' @return `CompareQuartetsMulti()` returns a named integer vector specifying 
#' the number of quartets whose resolution in `x` matches all or any of the
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
#' @family quartet counting functions
#' 
#' @examples 
#' library("TreeTools")
#' CompareQuartetsMulti(x  = CollapseNode(as.phylo(42, 6), 8:9),
#'                      cf = list(BalancedTree(6), PectinateTree(6), 
#'                                CollapseNode(as.phylo(1337, 6), 9:10)))
#' 
#' @importFrom TreeTools NTip RenumberTips
#' @export
CompareQuartetsMulti <- function (x, cf) {
  
  input <- QuartetStates(x, asRaw = TRUE)
  nIn <- length(input)
  if (inherits(cf, "phylo")) {
    cf <- list(cf)
  }
  xLabels <- x$tip.label
  xSorted <- sort(xLabels)
  cfTips <- vapply(cf, NTip, 0L)
  if (!all(cfTips == length(xLabels))) {
    stop("All trees must contain the same number of leaves.")
  }
  if (!all(apply(sapply(cf, getElement, "tip.label"), 2L, function (lab)
    identical(sort(lab), xSorted)))) {
    stop("All trees must contain the same tip labels.")
  }
  
  cf <- lapply(cf, RenumberTips, xLabels)
  
  nCf <- length(cf)
  comparison <- vapply(cf, QuartetStates, input, asRaw = TRUE)
  
  xResolved <- as.logical(input)
  cfResolved <- comparison != as.raw(0)
  cfResolvedSums <- .rowSums(cfResolved, nIn, nCf)
  cfAnyResolved <- as.logical(cfResolvedSums)
  cfAllResolved <- cfResolvedSums == nCf
  
  equal <- comparison == input
  same <- .rowSums(equal, nIn, nCf)
  anySame <- as.logical(same)
  allSame <- same == nCf
  different <- .rowSums(!equal & cfResolved, nIn, nCf)
  
  # Return:
  c(
    N = (length(cf) + 1L) * nIn,
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
