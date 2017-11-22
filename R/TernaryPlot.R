#' Quartet plot
#' 
#' Plot how similar trees are to a reference tree.
#' 
#' @template treesParam
#' 
#' @references {
#'   #TODO Provide ggtern citation.
#' }
#' 
#' @author Martin R. Smith
#' 
#' @importFrom ggtern ggtern
#' @importFrom ggplot2 geom_line
#' @export
QuartetPlot <- function (trees) {
  ref_tree <- trees[[1]]
  status <- MatchingQuartets(trees)
  status <- rbind(status, status[1] - colSums(status))
  ccu <- c('Consistent', 'Unresolved', 'Contradicted')
  rownames(status) <- ccu
  ggtern(mapping=aes(Contradicted, Consistent, Unresolved)) +
    geom_path
}


GREY <- rgb(0.8, 0.8, 0.6, 0.7)

#' @describeIn TernaryGrid Plot KL divergence lines
#' @export
#' @keywords internal
KLDivergenceLine <- function (i) geom_line(data=data.frame(Correct    = c(10-i, 10-(2*i)), 
                                                           Incorrect  = c(i, 0),
                                                           Unresolved = c(0, i*2)),
                                           color=rgb(1, 1, 1, 1))

#' Ternary Grid
#'
#' @importFrom ggtern ggtern
#' @importFrom ggplot2 geom_line geom_path theme_rotate
#' @export
#' @keywords internal
TernaryGrid <- ggtern(mapping=aes(Contradicted, Consistent, Unresolved)) +
  geom_path (data=data.frame(Consistent    = as.integer(sapply(10:0, function(x) c(x, 0))),
                             Contradicted  = as.integer(sapply(0:10, function(x) c(x, 0))), 
                             Unresolved    = rep(0:1, 11)
  ), color=GREY, linetype=3) +
  sapply(1:10, KLDivergenceLine) +
  geom_path (data=data.frame(Consistent    = c(1, 0, 0, 1), 
                             Contradicted  = c(0, 1, 0, 0), 
                             Unresolved    = c(0, 0, 1, 0)), color=GREY) +
  theme_rotate(degrees=30)
