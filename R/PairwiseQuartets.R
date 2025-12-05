#' Pairwise quartet distances
#' 
#' Computes the quartet distance between each pair of trees in a list.
#' 
#' @inheritParams QuartetStatus
#' @param Measure a function that calculates tree similarity or difference
#' from quartet statuses. Default is [`QuartetDivergence()`].
#' @return a matrix specifying the distance between each tree and
#' each other tree in the `trees`.
#' 
#' @template MRS
#' 
#' @examples
#' data("sq_trees")
#' # Calculate the status of each quartet relative to the first entry in 
#' # sq_trees
#' sq_status <- QuartetStatus(sq_trees)
#' 
#' # Calculate Estabrook et al's similarity measures:
#' SimilarityMetrics(sq_status)
#' 
#' # Compare trees that include a subset of the taxa 1..10
#' library("TreeTools", quietly = TRUE, warn.conflict = FALSE)
#' QuartetStatus(BalancedTree(1:5), BalancedTree(3:8), nTip = 10)
#' 
#' # If all taxa studied occur in `trees` or `cf`, set `nTip = TRUE`
#' QuartetStatus(BalancedTree(1:5), BalancedTree(3:10), nTip = TRUE)
#'  
#' @family element-by-element comparisons
#' @seealso 
#' - Use splits (groups/clades defined by nodes or edges of the tree) instead
#'   of quartets as the unit of comparison: [`SplitStatus()`].
#'  
#' - Generate distance metrics from quartet statuses: [`SimilarityMetrics()`].
#' 
#' @references \insertAllCited{}
#' @export
PairwiseQuartets <- function(trees, Measure = QuartetDivergence) {
  Measure(ManyToManyQuartetAgreement(trees))
}
