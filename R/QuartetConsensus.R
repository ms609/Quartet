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
#' The function supports trees with up to 100 tips.  For larger trees,
#' the explicit quartet enumeration becomes prohibitively expensive.
#'
#' @return A tree of class `phylo`.
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
#' @importFrom TreeTools as.Splits TipLabels NSplits Consensus StarTree
#' @export
QuartetConsensus <- function(trees,
                             init = c("majority", "empty", "extended"),
                             greedy = c("best", "first")) {
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

  # Convert each tree to a raw split matrix
  splitsList <- lapply(trees, function(tr) {
    sp <- as.Splits(tr, tipLabels)
    unclass(sp)
  })

  res <- cpp_quartet_consensus(
    splitsList, nTip,
    init_majority = (init == "majority"),
    init_extended = (init == "extended"),
    greedy_best_flag = (greedy == "best")
  )

  included <- res$included
  if (!any(included)) {
    return(StarTree(tipLabels))
  }

  rawSplits <- res$raw_splits[included, , drop = FALSE]
  sp <- structure(rawSplits, nTip = nTip, tip.label = tipLabels,
                  class = "Splits")
  as.phylo(sp)
}
