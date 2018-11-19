#' Fifteen trees
#' 
#' A list of class \code{multiPhylo} containing phylogenetic trees:
#' \describe{
#'   \item{ref_tree}{A reference tree, bearing tips labelled 1 to 11.}
#'    \item{move_one_near}{Tip 1 has been moved a short distance.}
#'    \item{move_one_mid}{Tip 1 has been moved further.}
#'    \item{move_one_far}{Tip 1 has been moved further still.}
#'    \item{move_two_near}{Tips 10 & 11 have been moved a short distance.}
#'    \item{move_two_mid}{Tips 10 & 11 have been moved further.}
#'    \item{move_two_far}{Tips 10 & 11 have been moved further still.}
#'    \item{collapse_one}{One node has been collapsed into a polytomy.}
#'    \item{collapse_some}{Several nodes have been collapsed.}
#'    \item{m1mid_col1}{Tree `move_one_mid` with one node collapsed.}
#'    \item{m1mid_colsome}{Tree `move_one_mid` with several nodes collapsed.}
#'    \item{m2mid_col1}{Tree `move_two_mid` with one node collapsed.}
#'    \item{m2mid_colsome}{Tree `move_two_mid` with several nodes collapsed.}
#'    \item{opposite_tree}{A tree that is more different from `ref_tree` than expected by chance.}
#'    \item{random_tree}{A random tree.}
#' }
#'
#' @keywords datasets
"sq_trees"
#' O'Reilly et al. reference tree
#'
#' The tree topology used to generate the matrices in O'Reilly _et al._ 2016
#'
#' @format A single phylogenetic tree saved as an object of class \code{phylo}
#'
#' @references 
#' \insertRef{OReilly2016}{Quartet}
#' 
#' @examples 
#'   data(or_refTree)
#'   plot(or_refTree)
#' 
#' @source \insertRef{OReilly2016}{Quartet}
"or_refTree"

#' Partition and Quartet similarity counts for the O'Reilly _et al._ datasets
#'
"orQuartets"

#' Partition similarity counts for the O'Reilly _et al._ datasets
#' 
"orPartitions"
