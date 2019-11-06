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
#' -- though for a number of reasons, quartets may offer a more meaningful
#' measure of the amount of information shared by two trees.
#' 
#' * Do Not Conflict (DC): (_s_ + _r1_ + _r2_ + _u_) / _N_
#' 
#' * Explicitly Agree (EA): _s_ / _N_
#' 
#' * Strict Joint Assertions (SJA): _s_ / (_s_ + _d_)
#' 
#' * SemiStrict Joint Assertions (SSJA): _s_ / (_s_ + _d_ + _u_)
#' 
#' (The numerator of the Semistrict Joint Assertions similarity metric is given in
#'  Estabrook _et al_. (1985)'s table 2 as _s_ + _d_, but this is understood, with
#'  reference to the text to be a typographic error.)
#' 
#' Steel & Penny (1993) propose a further metric, which they denote d<sub>Q</sub>,
#' which this package calculates using the function `SteelPenny`:
#' 
#' * Steel & Penny's Quartet Metric (dQ): (_s_ + _u_) / _N_
#' 
#' Another take on tree similarity is to consider the symmetric difference: that is,
#' the number of partitions or quartets present in one tree that do not appear in the
#' other, originally used to measure tree similarity by Robinson & Foulds (1981).
#' (Note that, given the familiarity of the Robinson Foulds distance metric, this
#' quantity is be default expressed as a difference rather than a similarity.)
#' 
#' * Robinson Foulds (RF): _d1_ + _d2_ + _r1_ + _r2_
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
#' number of tips (Smith 2019):
#' 
#' * Symmetric Divergence: (_d_ + _d_ + _r1_ + _r2_) / 2 _Q_
#' 
#'
#' @template elementStatusParam
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
#' \insertRef{Smith2019}{Quartet}
#'
#' \insertRef{Steel1993}{Quartet}
#' 
#' @template MRS
#' 
#' @name SimilarityMetrics
#' @export
SimilarityMetrics <- function (elementStatus, similarity = TRUE) {
  elementStatus <- StatusToMatrix(elementStatus)
  result <- data.frame(
    DoNotConflict = elementStatus[, '2d'] / elementStatus[, 'N'],
    ExplicitlyAgree = 1 - (2L * elementStatus[, 's']) / elementStatus[, 'N'],
    StrictJointAssertions = elementStatus[, '2d'] / rowSums(elementStatus[, c('2d', 's', 's')]),
    SemiStrictJointAssertions = SemiStrictJointAssertions(elementStatus, similarity = FALSE),
    SymmetricDifference =  rowSums(elementStatus[, c('2d', 'r1', 'r2')]) /
      rowSums(elementStatus[, c('2d', 's', 's', 'r1', 'r2')]),
    MarczewskiSteinhaus = rowSums(elementStatus[, c('2d', 'r1', 'r2')]) /
      rowSums(elementStatus[, c('2d', 's', 'r1', 'r2')]),
    SteelPenny = SteelPenny(elementStatus, similarity = FALSE),
    QuartetDivergence = rowSums(elementStatus[, c('2d', 'r1', 'r2')]) / elementStatus[, 'N']
  )
  if (similarity) 1 - result else result
}

#' Status vector to matrix
#' 
#' Converts a vector to a matrix that can be analysed by the [DoNotConflict]
#' function family.
#' 
#' @param statusVector Either (i) a named vector of integers, with 
#' names `N`, `s`, `r1`, `r2`, either `d` or `d1` and `d2`, and optionally `u`; or
#' (ii) a matrix whose named rows correspond to the same quantities.
#' @return A matrix, containing the input columns plus `2d`, representing 
#' either `2 * d` or `d1 + d2`, and row names.  
#' 
#' The row name means that column names are dropped in
#' the output of `DoNotConflict` etc.
#' 
#' @template MRS
#' @export
#' @keywords internal
StatusToMatrix <- function (statusVector) {
  if (is.null(dim(statusVector))) {
    statusVector <- matrix(statusVector, 1L, dimnames = list('tree', names(statusVector)))
  }
  if ('2d' %in% colnames(statusVector)) {
    # Repeat visitor; return unadulterated
    statusVector
  } else if ('d' %in% colnames(statusVector)) {
    statusVector <- cbind(statusVector, '2d' = 2L * statusVector[, 'd'])
  } else {
    statusVector <- cbind(statusVector,
                          '2d' = statusVector[, 'd1'] + statusVector[, 'd2'])
  }
}

#' @rdname SimilarityMetrics
#' @export
DoNotConflict <- function (elementStatus, similarity=TRUE) {
  elementStatus <- StatusToMatrix(elementStatus)
  result <- elementStatus[, '2d'] / elementStatus[, 'N']
  if (similarity) 1 - result else result
}

#' @rdname SimilarityMetrics
#' @export
ExplicitlyAgree <- function (elementStatus, similarity=TRUE) {
  elementStatus <- StatusToMatrix(elementStatus)
  result <- 2L * elementStatus[, 's'] / elementStatus[, 'N']
  if (similarity) result else 1 - result
}

#' @rdname SimilarityMetrics
#' @export
StrictJointAssertions <- function (elementStatus, similarity=TRUE) {
  elementStatus <- StatusToMatrix(elementStatus)
  result <- elementStatus[, '2d'] / rowSums(elementStatus[, c('2d', 's', 's'), drop=FALSE])
  if (similarity) 1 - result else result
}

#' @rdname SimilarityMetrics
#' @export
SemiStrictJointAssertions <- function (elementStatus, similarity=TRUE) {
  elementStatus <- StatusToMatrix(elementStatus)
  if (all(c('s', 'd', 'u') %in% colnames(elementStatus))) {
    numerator <- if (similarity) elementStatus[, 's'] else elementStatus[, 'd']
    # Return:
    numerator / rowSums(elementStatus[, c('s', 'd', 'u'), drop=FALSE])
  } else {
    NA
  }
}

#' @rdname SimilarityMetrics
#' @export
SymmetricDifference <- function (elementStatus, similarity=TRUE) {
  elementStatus <- StatusToMatrix(elementStatus)
  result <- rowSums(elementStatus[, c('2d', 'r1', 'r2'), drop=FALSE]) /
    rowSums(elementStatus[, c('2d', 's', 's', 'r1', 'r2'), drop=FALSE])
  if (similarity) 1 - result else result
}

#' @rdname SimilarityMetrics
#' @export
RobinsonFoulds <- function (elementStatus, similarity=FALSE) {
  elementStatus <- StatusToMatrix(elementStatus)
  rFDist <- rowSums(elementStatus[, c('2d', 'r1', 'r2'), drop=FALSE])
  if (similarity) elementStatus[, c('N')] - rFDist else rFDist
}

#' @rdname SimilarityMetrics
#' @export
MarczewskiSteinhaus <- function (elementStatus, similarity=TRUE) {
  elementStatus <- StatusToMatrix(elementStatus)
  result <- rowSums(elementStatus[, c('2d', 'r1', 'r2'), drop=FALSE]) /
    rowSums(elementStatus[, c('2d', 's', 'r1', 'r2'), drop=FALSE])
  if (similarity) 1 - result else result
}

#' @rdname SimilarityMetrics
#' @export
SteelPenny <- function (elementStatus, similarity=TRUE) {
  elementStatus <- StatusToMatrix(elementStatus)
  # Defined in Steel & Penny, p. 133; "dq would be written as "D + R".
  # dq = D + R in Day's (1986) terminology, where D = d/Q, R = (r1 + r2)/Q
  result <- rowSums(elementStatus[, c('2d', 'r1', 'r1', 'r2', 'r2'), drop=FALSE]) / 
    elementStatus[, 'N']
  if (similarity) 1 - result else result
}

#' @rdname SimilarityMetrics
#' @references \insertRef{Smith2019}{Quartet}
#' @export
QuartetDivergence <- function (elementStatus, similarity=TRUE) {
  elementStatus <- StatusToMatrix(elementStatus)
  result <- rowSums(elementStatus[, c('2d', 'r1', 'r2'), drop=FALSE]) / 
    elementStatus[, 'N']
  if (similarity) 1 - result else result
}
