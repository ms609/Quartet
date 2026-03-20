#' Consensus tree minimizing quartet distance
#'
#' Construct a consensus tree that minimizes the sum of symmetric quartet
#' distances to a set of input trees, using a greedy add-and-prune heuristic.
#'
#' The majority-rule consensus minimizes the sum of Robinson-Foulds distances
#' to the input trees.  Analogously, `QuartetConsensus()` finds an approximate
#' median tree under the symmetric quartet distance
#' \insertCite{Takazawa2026}{Quartet}, which counts
#' both false-positive and false-negative resolved quartets equally.
#'
#' Because the quartet distance gives greater weight to deep branches (which
#' resolve more quartets), quartet consensus trees tend to be more resolved
#' than majority-rule trees, especially when phylogenetic signal is low.
#'
#' @param trees An object of class `multiPhylo`: the input trees.
#'   All trees must share the same tip labels.
#'   Trees may be non-binary (polytomies are handled correctly).
#' @param init Character string specifying the initial tree:
#'   - `"majority"` (default): start from the majority-rule consensus.
#'   - `"empty"`: start from a star tree (purely additive).
#'   - `"extended"`: start from the extended (greedy) majority-rule consensus.
#' @param greedy Character string specifying the greedy strategy:
#'   - `"best"` (default): evaluate all candidates and pick the single
#'     highest-benefit action at each step.
#'   - `"first"`: pick the first improving action encountered (faster but
#'     may give a slightly worse result).
#' @param neverDrop Controls rogue taxon dropping:
#'   - `TRUE` (default): never drop taxa; use the symmetric quartet distance
#'     objective (existing behaviour).
#'   - `FALSE`: any taxon may be dropped; switches to the
#'     `SimilarityToReference` (chance-corrected) objective to make split
#'     changes and taxon drops commensurable.
#'   - A character vector of tip labels: those tips are protected from
#'     dropping; all other tips are candidates.  Uses the S2R objective.
#'
#' @details
#' The algorithm pools all splits observed across input trees and maintains
#' a quartet profile: for each of the \eqn{\binom{n}{4}}{C(n,4)} quartets,
#' a count of how many input trees resolve it as each of the three possible
#' topologies.  Splits are greedily added to (or removed from) the consensus
#' when doing so reduces the total symmetric quartet distance to the input
#' trees.  Candidate splits must be compatible with all currently included
#' splits (four-gamete test).
#'
#' When `neverDrop` enables taxon dropping, the greedy loop also considers
#' removing rogue taxa.  At each step, the single best-improving action
#' (add split, remove split, or drop taxon) is taken.  The objective
#' switches from symmetric quartet distance to the mean chance-corrected
#' [`SimilarityToReference`], which is comparable across different numbers
#' of taxa: 0 for a random tree, 1 for perfect agreement.
#'
#' The function supports trees with up to 100 tips.  For larger trees,
#' the explicit quartet enumeration becomes prohibitively expensive.
#'
#' @return A tree of class `phylo`.  When taxon dropping is enabled,
#'   the tree may have fewer tips than the input trees.  Attributes
#'   `"dropped"` (character vector of dropped tip labels, in drop order)
#'   and `"drop_scores"` (S2R score after each drop) are attached.
#'
#' @references
#' \insertAllCited{}
#'
#' @examples
#' library(TreeTools)
#'
#' # Generate bootstrap-like trees
#' trees <- as.phylo(1:20, nTip = 8)
#'
#' # Quartet consensus
#' qc <- QuartetConsensus(trees)
#' plot(qc)
#'
#' # Compare resolution with majority-rule
#' mr <- Consensus(trees, p = 0.5)
#' cat("Majority-rule splits:", NSplits(mr), "\n")
#' cat("Quartet consensus splits:", NSplits(qc), "\n")
#'
#' @importFrom ape as.phylo
#' @importFrom TreeTools as.Splits TipLabels NSplits Consensus StarTree
#' @export
QuartetConsensus <- function(trees,
                             init = c("majority", "empty", "extended"),
                             greedy = c("best", "first"),
                             neverDrop = TRUE) {
  init <- match.arg(init)
  greedy <- match.arg(greedy)

  if (!inherits(trees, "multiPhylo")) {
    stop("`trees` must be an object of class 'multiPhylo'.")
  }
  nTree <- length(trees)
  if (nTree < 2L) stop("Need at least 2 trees.")

  tipLabels <- TipLabels(trees[[1]])
  nTip <- length(tipLabels)

  if (nTip < 4L) stop("Need at least 4 tips for quartet consensus.")
  if (nTip > 100L) {
    stop("QuartetConsensus supports at most 100 tips. ",
         "The explicit quartet enumeration is O(n^4).")
  }

  # Resolve neverDrop to an integer vector (1-based) or NULL
  if (isTRUE(neverDrop)) {
    neverDropR <- NULL
  } else if (isFALSE(neverDrop)) {
    neverDropR <- integer(0)
  } else {
    # Character vector of protected labels
    neverDrop <- as.character(neverDrop)
    bad <- setdiff(neverDrop, tipLabels)
    if (length(bad)) {
      stop("neverDrop labels not found in trees: ",
           paste(bad, collapse = ", "))
    }
    neverDropR <- match(neverDrop, tipLabels)
  }

  # Convert each tree to a raw split matrix
  splitsList <- lapply(trees, function(tr) {
    sp <- as.Splits(tr, tipLabels)
    unclass(sp)
  })

  res <- cpp_quartet_consensus(
    splitsList, nTip,
    init_majority = (init == "majority"),
    init_extended = (init == "extended"),
    greedy_best_flag = (greedy == "best"),
    never_drop_r = neverDropR
  )

  # Determine active tips
  activeTips <- res$active_tips
  activeTipLabels <- tipLabels[activeTips]

  included <- res$included
  if (!any(included)) {
    result <- StarTree(activeTipLabels)
  } else {
    rawSplits <- res$raw_splits[included, , drop = FALSE]

    if (length(activeTipLabels) < nTip) {
      # Filter split bitvectors to active tips only
      droppedIdx <- which(!activeTips)
      keepIdx <- which(activeTips)
      nActiveTip <- length(keepIdx)
      nBytesNew <- ceiling(nActiveTip / 8)

      # Rebuild split matrix for active tips
      newSplits <- matrix(raw(0), nrow = nrow(rawSplits), ncol = nBytesNew)
      for (i in seq_len(nrow(rawSplits))) {
        bits <- as.integer(rawSplits[i, ])
        # Extract bits for each original tip, keep only active ones
        activeBits <- vapply(keepIdx, function(tip) {
          byteIdx <- (tip - 1L) %/% 8L + 1L
          bitIdx <- (tip - 1L) %% 8L
          as.logical(bitwAnd(bits[byteIdx], bitwShiftL(1L, bitIdx)))
        }, logical(1))
        # Pack into new raw bytes
        for (j in seq_along(activeBits)) {
          if (activeBits[j]) {
            byteIdx <- (j - 1L) %/% 8L + 1L
            bitIdx <- (j - 1L) %% 8L
            newSplits[i, byteIdx] <- as.raw(
              bitwOr(as.integer(newSplits[i, byteIdx]),
                     bitwShiftL(1L, bitIdx))
            )
          }
        }
      }

      # Filter out trivial splits (< 2 on either side)
      nontrivial <- apply(newSplits, 1, function(row) {
        bits <- as.integer(row)
        popcount <- sum(vapply(bits, function(b) {
          s <- 0L
          while (b > 0L) { s <- s + bitwAnd(b, 1L); b <- bitwShiftR(b, 1L) }
          s
        }, integer(1)))
        popcount >= 2L && (nActiveTip - popcount) >= 2L
      })
      newSplits <- newSplits[nontrivial, , drop = FALSE]

      if (nrow(newSplits) == 0L) {
        result <- StarTree(activeTipLabels)
      } else {
        sp <- structure(newSplits, nTip = nActiveTip,
                        tip.label = activeTipLabels, class = "Splits")
        result <- as.phylo(sp)
      }
    } else {
      sp <- structure(rawSplits, nTip = nTip, tip.label = tipLabels,
                      class = "Splits")
      result <- as.phylo(sp)
    }
  }

  # Attach drop metadata
  if (!is.null(neverDropR)) {
    droppedTipIdx <- res$dropped_tips
    attr(result, "dropped") <- tipLabels[droppedTipIdx]
    attr(result, "drop_scores") <- res$drop_scores
  }

  result
}
