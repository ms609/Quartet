#' Tree Similarity Metrics
#' 
#' Functions to calculate tree similarity / difference metrics.
#' 
#' Estabrook _et al._ (1985, table 2) define four similarity metrics in terms of the 
#' total number of quartets (_N_, their _Q_), the number of quartets resolved in the same
#' manner in two trees (_s_), the number resolved differently in both trees 
#' (_d_), the number resolved in tree 1 or 2 but unresolved in the other tree
#' (_r1_, _r2_), and the number that are unresolved in both trees (_u_).
#' 
#' The similarity metrics are then given as below.  The dissimilarity metrics 
#' are their complement (i.e. 1 - _similarity_), and can be calculated 
#' algebraically using the identity _N_ = _s_ + _d_ + _r1_ + _r2_ + _u_.
#' 
#' Although defined using quartets, analagous values can be calculated using partitions
#' -- though for a number of reasons, quartets offer a more meaningful
#' measure of the amount of information shared by two trees.
#' 
#' * Do Not Conflict (DC): (_s_ + _r1_ + _r2_ + _u_) / _N_
#' 
#' * Explicitly Agree (EA): _s_ / _N_
#' 
#' * Strict Joint Assertions (SJA): _s_ / (_s_ + _d_)
#' 
#' * SemiStrict Joint Assertions (SSJA): (_s_ + _d_) / (_s_ + _d_ + _u_)
#' 
#' Steel & Penny (1993) propose a further metric, which they denote d<sub>Q</sub>,
#' which this package calculates using the function `SteelPenny`:
#' 
#' * Steel & Penny's Quartet Metric (dQ): (_s_ + _u_) / _N_
#' 
#' Another take on tree similarity is to consider the symmetric difference: that is,
#' the number of partitions or quartets present in one tree that do not appear in the
#' other, originally used to measure tree similarity by Robinson & Foulds (1981).
#' 
#' * Raw Symmetric Difference: 2 _d_ + _r1_ + _r2_
#' 
#' To contextualize the symmetric difference, it may be normalized against:
#'
#' The total number of resolved quartets or partitions present in both trees (Day 1986):
#' 
#' * Symmetric Difference (SD): (2 _d_ + _r1_ + _r2_) / (2 _d_ + 2 _s_ + _r1_ + _r2_)
#' 
#' The total distinctly resolved quartets or partitions (Day 1986):
#' 
#' * Marczewski-Steinhaus (MS): (2 _d_ + _r1_ + _r2_) / (2 _d_ + _s_ + _r1_ + _r2_)
#' 
#' The maximum number of quartets or partitions that could have been resolved, given the
#' number of tips [@Smith2019]:
#' 
#' * Symmetric Divergence: (_d_ + _d_ + _r1_ + _r2_) / 2 _Q_
#' 
#'
#' @param elementStatus Two-dimensional integer array, with rows corresponding to 
#'   counts of matching quartets or partitions for each tree, and columns named 
#'   according to the output of [QuartetStatus] or [SplitStatus].  
#' @param similarity Logical specifying whether to calculate the similarity
#'                   or dissimilarity.
#'
#' @return
#'   `SimilarityMetrics` returns a named two-dimensional array in which each row 
#'   corresponds to an input tree, and each column corresponds to one of the
#'   listed measures.
#'   
#'   `DoNotConflict` and others return a named vector describing the requested
#'   similarity (or difference) between the trees.
#'
#' @seealso 
#'   * [QuartetStatus]: Calculate status of each quartet: the raw material 
#'     from which the Estabrook _et al._ metrics are calculated.
#'     
#'   * [SplitStatus], [CompareSplits]: equivalent metrics for bipartition splits.
#'
#' @examples 
#'   data('sq_trees')
#'   
#'   sq_status <- QuartetStatus(sq_trees)
#'   SimilarityMetrics(sq_status)
#'   QuartetDivergence(sq_status, similarity=FALSE)
#'
#' @references 
#' \insertRef{Day1986}{Quartet}
#' 
#' \insertRef{Estabrook1985}{Quartet}
#' 
#' \insertRef{Marczewski1958}{Quartet}
#'
#' \insertRef{Robinson1981}{Quartet}
#'
#' \insertRef{Steel1993}{Quartet}
#' 
#' @template MRS
#' 
#' @name SimilarityMetrics
#' @export
SimilarityMetrics <- function (elementStatus, similarity=TRUE) {
  result <- data.frame(
    DoNotConflict = elementStatus[, 'd'] / elementStatus[, 'N'],
    ExplicitlyAgree = 1 - (elementStatus[, 's'] / elementStatus[, 'N']),
    StrictJointAssertions = elementStatus[, 'd'] / rowSums(elementStatus[, c('d', 's')]),
    SemiStrictJointAssertions = elementStatus[, 'd'] / rowSums(elementStatus[, c('d', 's', 'u')]),
    SymmetricDifference =  rowSums(elementStatus[, c('d', 'd', 'r1', 'r2')]) /
      rowSums(elementStatus[, c('d', 'd', 's', 's', 'r1', 'r2')]),
    MarczewskiSteinhaus = rowSums(elementStatus[, c('d', 'd', 'r1', 'r2')]) /
      rowSums(elementStatus[, c('d', 'd', 's', 'r1', 'r2')]),
    SteelPenny = rowSums(elementStatus[, c('d', 'r1', 'r2')]) / elementStatus[, 'N'],
    QuartetDivergence = rowSums(elementStatus[, c('d', 'd', 'r1', 'r2')]) / (2 * elementStatus[, 'N'])
  )
  if (similarity) 1 - result else result
}

#' Status vector to matrix
#' 
#' Converts a vector to a matrix that can be analysed by the [DoNotConflict]
#' function family.
#' 
#' @param statusVector A vector of six integers, in the sequence expected by
#'  `BLANK_QUARTET`.  If provided a matrix, the matrix will be returned 
#'  unaltered.
#' @return A matrix, containing columns named `N`, `s`, `d`, `r1`, `r2`, `u`, 
#' and a single named row.  The row name means that column names are dropped in
#' the output of `DoNotConflict` etc.
#' 
#' @author Martin R. Smith
#' @keywords internal
StatusToMatrix <- function (statusVector) {
  if (is.null(dim(statusVector))) {
    matrix(statusVector, 1, 6, dimnames = list('tree', c('N', 's', 'd', 'r1', 'r2', 'u')))
  } else {
    statusVector
  }
}

#' @rdname SimilarityMetrics
#' @export
DoNotConflict <- function (elementStatus, similarity=TRUE) {
  elementStatus <- StatusToMatrix(elementStatus)
  result <- elementStatus[, 'd'] / elementStatus[, 'N']
  if (similarity) 1 - result else result
}

#' @rdname SimilarityMetrics
#' @export
ExplicitlyAgree <- function (elementStatus, similarity=TRUE) {
  elementStatus <- StatusToMatrix(elementStatus)
  result <- elementStatus[, 's'] / elementStatus[, 'N']
  if (similarity) result else 1 - result
}

#' @rdname SimilarityMetrics
#' @export
StrictJointAssertions <- function (elementStatus, similarity=TRUE) {
  elementStatus <- StatusToMatrix(elementStatus)
  result <- elementStatus[, 'd'] / rowSums(elementStatus[, c('d', 's'), drop=FALSE])
  if (similarity) 1 - result else result
}

#' @rdname SimilarityMetrics
#' @export
SemiStrictJointAssertions <- function (elementStatus, similarity=TRUE) {
  elementStatus <- StatusToMatrix(elementStatus)
  result <- elementStatus[, 'd'] / rowSums(elementStatus[, c('d', 's', 'u'), drop=FALSE])
  if (similarity) 1 - result else result
}

#' @rdname SimilarityMetrics
#' @export
SymmetricDifference <- function (elementStatus, similarity=TRUE) {
  elementStatus <- StatusToMatrix(elementStatus)
  result <- rowSums(elementStatus[, c('d', 'd', 'r1', 'r2'), drop=FALSE]) /
    rowSums(elementStatus[, c('d', 'd', 's', 's', 'r1', 'r2'), drop=FALSE])
  if (similarity) 1 - result else result
}

#' @rdname SimilarityMetrics
#' @export
MarczewskiSteinhaus <- function (elementStatus, similarity=TRUE) {
  elementStatus <- StatusToMatrix(elementStatus)
  result <- rowSums(elementStatus[, c('d', 'd', 'r1', 'r2'), drop=FALSE]) /
    rowSums(elementStatus[, c('d', 'd', 's', 'r1', 'r2'), drop=FALSE])
  if (similarity) 1 - result else result
}

#' @rdname SimilarityMetrics
#' @export
SteelPenny <- function (elementStatus, similarity=TRUE) {
  elementStatus <- StatusToMatrix(elementStatus)
  # Defined in Steel & Penny, p. 133; "dq would be written as "D + R".
  # dq = D + R in Day's (1986) terminology, where D = d/N, R = (r1 + r2)/N, 
  # and N is the total number of quartets
  result <- rowSums(elementStatus[, c('d', 'r1', 'r2'), drop=FALSE]) / elementStatus[, 'N']
  if (similarity) 1 - result else result
}

#' @rdname SimilarityMetrics
#' @references \insertRef{Smith2019}{Quartet}
#' @export
QuartetDivergence <- function (elementStatus, similarity=TRUE) {
  elementStatus <- StatusToMatrix(elementStatus)
  result <- rowSums(elementStatus[, c('d', 'd', 'r1', 'r2'), drop=FALSE]) /
    ( 2 * elementStatus[, 'N'])
  if (similarity) 1 - result else result
}
