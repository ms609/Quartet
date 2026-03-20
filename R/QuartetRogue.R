#' Identify rogue taxa using quartet similarity
#'
#' Greedily drops taxa whose removal most improves the mean quartet similarity
#' across a set of trees.  Uses chance-corrected `SimilarityToReference`
#' (normalised to \[0, 1\]) as the objective, so that scores are comparable
#' before and after taxon removal.
#'
#' At each step the taxon whose removal yields the greatest improvement is
#' dropped.  A restoration pass then checks whether any earlier drop is now
#' unnecessary.  The result is a sequence of drops together with the running
#' score; the optimal reduced tree set is obtained by applying only the drops
#' up to the score maximum.
#'
#' @section Method:
#' The score for a tree set is the mean of
#' `SimilarityToReference(normalize = TRUE)` over all ordered pairs `(i, j)`
#' with tree `j` as reference.  This score is 0 for random or star trees,
#' 1 for perfect agreement, and comparable across different numbers of taxa.
#'
#' @param trees An object of class `multiPhylo`.  All trees must share the same
#'   tip labels.
#' @param neverDrop Character vector of tip labels (or integer indices) that
#'   should never be dropped.
#' @param maxDrop Maximum number of taxa to drop.
#'   `NULL` (default) means drop until no further improvement.
#' @param return Character: `"taxa"` returns a data frame of drop operations;
#'   `"tree"` returns a consensus tree with rogue taxa removed.
#' @param p Consensus threshold when `return = "tree"`.
#' @param verbose Logical; if `TRUE`, print progress messages.
#'
#' @return If `return = "taxa"`, a `data.frame` with columns:
#'   \describe{
#'     \item{num}{Sequential drop index (0 = baseline).}
#'     \item{taxon}{Label of the dropped taxon (`NA` for the baseline row).}
#'     \item{rawImprovement}{Score improvement from this drop (`NA` for
#'       baseline).}
#'     \item{score}{Mean normalised SimilarityToReference after all drops
#'       so far.}
#'   }
#'   If `return = "tree"`, a consensus tree of class `phylo` with rogue taxa
#'   removed.
#'
#' @examples
#' library("TreeTools")
#' trees <- AddTipEverywhere(BalancedTree(6), "rogue")[3:6]
#' QuartetRogue(trees)
#' @importFrom ape drop.tip consensus
#' @importFrom TreeTools TipLabels NTip Preorder
#' @template MRS
#' @family tree comparison
#' @export
QuartetRogue <- function(trees,
                         neverDrop = character(0),
                         maxDrop = NULL,
                         return = c("taxa", "tree"),
                         p = 0.5,
                         verbose = FALSE) {
  return <- match.arg(return)

  if (!inherits(trees, "multiPhylo")) {
    if (is.list(trees) && all(vapply(trees, inherits, FALSE, "phylo"))) {
      class(trees) <- "multiPhylo"
    } else {
      stop("`trees` must be a list of `phylo` objects.")
    }
  }

  k <- length(trees)
  if (k < 2L) stop("Need at least 2 trees.")

  allTips <- TipLabels(trees[[1]])
  nTip <- length(allTips)
  if (nTip < 5L) stop("Need at least 5 tips (can't drop below 4).")

  # Resolve neverDrop to character labels
  if (is.numeric(neverDrop)) {
    neverDrop <- allTips[neverDrop]
  }
  bad <- setdiff(neverDrop, allTips)
  if (length(bad)) {
    stop("neverDrop labels not found in trees: ",
         paste(bad, collapse = ", "))
  }

  maxPossible <- nTip - 4L - length(neverDrop)
  if (is.null(maxDrop)) {
    maxDrop <- maxPossible
  } else {
    maxDrop <- min(maxDrop, maxPossible)
  }
  if (maxDrop <= 0L) {
    baseline <- .QuartetRogueScore(trees)
    return(.QRResult(character(0), baseline, return, trees, p))
  }

  # Preorder trees once
  trees[] <- lapply(trees, Preorder)

  # Baseline score
  baseline <- .QuartetRogueScore(trees)

  # ------------------------------------------------------------------
  # Forward greedy pass
  # ------------------------------------------------------------------
  dropped <- character(0)
  scores <- baseline
  currentTrees <- trees
  currentScore <- baseline

  for (step in seq_len(maxDrop)) {
    currentTips <- TipLabels(currentTrees[[1]])
    candidates <- setdiff(currentTips, neverDrop)
    if (length(candidates) == 0L) break

    candidateScores <- vapply(candidates, function(tip) {
      pruned <- lapply(currentTrees, drop.tip, tip)
      class(pruned) <- "multiPhylo"
      .QuartetRogueScore(pruned)
    }, double(1))

    bestIdx <- which.max(candidateScores)
    bestScore <- candidateScores[bestIdx]
    if (bestScore <= currentScore) break

    bestTip <- candidates[bestIdx]
    if (verbose) {
      message("Step ", step, ": drop '", bestTip,
              "' (score ", round(currentScore, 4),
              " -> ", round(bestScore, 4), ")")
    }

    dropped <- c(dropped, bestTip)
    scores <- c(scores, bestScore)
    currentScore <- bestScore
    currentTrees <- lapply(currentTrees, drop.tip, bestTip)
    class(currentTrees) <- "multiPhylo"
  }

  # ------------------------------------------------------------------
  # Trim to best position
  # ------------------------------------------------------------------
  bestPos <- which.max(scores)
  dropped <- dropped[seq_len(bestPos - 1L)]

  # ------------------------------------------------------------------
  # Restoration pass
  # ------------------------------------------------------------------
  if (length(dropped) > 1L) {
    improved <- TRUE
    while (improved) {
      improved <- FALSE
      for (i in seq_along(dropped)) {
        tryDropped <- dropped[-i]
        tryTrees <- .DropTips(trees, tryDropped)
        tryScore <- .QuartetRogueScore(tryTrees)
        if (tryScore > currentScore) {
          if (verbose) {
            message("Restore '", dropped[i],
                    "' (score ", round(currentScore, 4),
                    " -> ", round(tryScore, 4), ")")
          }
          dropped <- tryDropped
          currentScore <- tryScore
          improved <- TRUE
          break
        }
      }
    }
  }

  # Recompute final score sequence
  scores <- .QuartetRogueScoreSequence(trees, dropped)

  # ------------------------------------------------------------------
  # Build return value
  # ------------------------------------------------------------------
  .QRResult(dropped, scores, return, trees, p)
}


# ---- Internal helpers -------------------------------------------------------

#' Mean normalised SimilarityToReference for a tree set
#'
#' For each tree as reference in turn, compute the normalised S2R of every
#' other tree, then average across all ordered pairs.
#'
#' @param trees `multiPhylo` object (all same tips).
#' @return Scalar: mean normalised S2R.
#' @keywords internal
#' @export
.QuartetRogueScore <- function(trees) {
  k <- length(trees)
  if (k < 2L) return(1)

  totalS2R <- 0
  nPairs <- 0L
  for (j in seq_len(k)) {
    status <- QuartetStatus(trees[-j], trees[[j]])
    s2r <- SimilarityToReference(status, similarity = TRUE, normalize = TRUE)
    # NaN arises when refBest = 1/3 (star reference); treat as 0
    s2r[!is.finite(s2r)] <- 0
    totalS2R <- totalS2R + sum(s2r)
    nPairs <- nPairs + length(s2r)
  }
  totalS2R / nPairs
}


#' Compute score sequence for a given drop order
#'
#' @param trees Original trees.
#' @param dropped Character vector of taxa in drop order.
#' @return Numeric vector of scores (length = `length(dropped) + 1`).
#' @keywords internal
#' @export
.QuartetRogueScoreSequence <- function(trees, dropped) {
  scores <- .QuartetRogueScore(trees)
  currentTrees <- trees
  for (tip in dropped) {
    currentTrees <- .DropTips(currentTrees, tip)
    scores <- c(scores, .QuartetRogueScore(currentTrees))
  }
  scores
}


#' Drop tips from a tree list
#' @keywords internal
.DropTips <- function(trees, tips) {
  if (length(tips) == 0L) return(trees)
  pruned <- lapply(trees, drop.tip, tips)
  class(pruned) <- "multiPhylo"
  pruned
}


#' Format QuartetRogue return value
#' @keywords internal
.QRResult <- function(dropped, scores, return, trees, p) {
  result <- data.frame(
    num = seq_along(scores) - 1L,
    taxon = c(NA_character_, if (length(dropped)) dropped),
    rawImprovement = c(NA_real_, diff(scores)),
    score = scores,
    stringsAsFactors = FALSE
  )

  if (return == "tree") {
    pruned <- .DropTips(trees, dropped)
    consensus(pruned, p = p)
  } else {
    result
  }
}
