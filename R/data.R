#' Eighteen example trees
#' 
#' A list of class \code{\link[ape:multiphylo]{multiPhylo}} containing phylogenetic trees:
#' \describe{
#'    \item{`ref_tree`}{A reference tree, bearing tips labelled 1 to 11.}
#'    \item{`move_one_near`}{Tip 1 has been moved a short distance.}
#'    \item{`move_one_mid`}{Tip 1 has been moved further.}
#'    \item{`move_one_far`}{Tip 1 has been moved further still.}
#'    \item{`move_two_near`}{Tips 10 & 11 have been moved a short distance.}
#'    \item{`move_two_mid`}{Tips 10 & 11 have been moved further.}
#'    \item{`move_two_far`}{Tips 10 & 11 have been moved further still.}
#'    \item{`collapse_one`}{One node has been collapsed into a polytomy.}
#'    \item{`collapse_some`}{Several nodes have been collapsed.}
#'    \item{`m1mid_col1`}{Tree `move_one_mid` with one node collapsed.}
#'    \item{`m1mid_colsome`}{Tree `move_one_mid` with several nodes collapsed.}
#'    \item{`m2mid_col1`}{Tree `move_two_mid` with one node collapsed.}
#'    \item{`m2mid_colsome`}{Tree `move_two_mid` with several nodes collapsed.}
#'    \item{`opposite_tree`}{A tree that shares fewer quartets with `ref_tree` than expected by chance.}
#'    \item{`caterpillar`}{A pectinate "caterpillar" tree.}
#'    \item{`top_and_tail`}{Tree `caterpillar`, with its outermost taxa swapped 
#'    such that it shares no partitions with `caterpillar`.}
#'    \item{`anti_pectinate`}{A random tree that shares no partitions with 
#'    `caterpillar`.}
#'    \item{`random_tree`}{A random tree.}
#' }
#'
#' @keywords datasets
"sq_trees"
