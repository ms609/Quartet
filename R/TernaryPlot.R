#' Quartet Points
#' 
#' Generate points to add to a ternary plot
#' 
#' @template treesParam
#' 
#' @examples {
#'   TernaryGrid +
#'   geom_point(data=QuartetPoints(trees), color=rgb(0.3, 0.2, 0.5), shape=3, size=3) +
#'   ggtitle(paste0("Quartets:"))
#' 
#' 
#'   TernaryGrid +
#'   geom_point(data=SplitsPoints(trees), color=rgb(0.3, 0.2, 0.5), shape=3, size=3) +
#'   ggtitle(paste0("Partitions:"))
#' }
#' 
#' @references {
#'   #TODO Provide ggtern citation.
#' }
#' 
#' @author Martin R. Smith
#' 
#' @importFrom ggtern ggtern
#' @importFrom ggplot2 geom_line ggtitle
#' @export
QuartetPoints <- function (trees) {
  status <- MatchingQuartets(trees)
  status <- rbind(status, status[1] - colSums(status))
  
  # Return: 
  data.frame(Consistent   = status[1, ], 
             Contradicted = status[3, ],
             Unresolved   = status[2, ])
}

#' @describeIn QuartetPoints Uses partition distance instead of quartet metric.
SplitsPoints <- function (trees) {
  status <- MatchingSplits(trees)
  status <- rbind(status, status[1] - colSums(status))
  
  # This probably assumes that ref is bifurcating.  #TODO Add resilience
  # Return: 
  data.frame(Consistent   = status['ref', ] - status['ref_not_cf', ], 
             Contradicted = status['cf_not_ref', ],
             Unresolved   = status['ref_not_cf', ] - status['cf_not_ref', ])
}

GREY <- rgb(0.8, 0.8, 0.6, 0.7)

#' @describeIn TernaryGrid Plot KL divergence lines
#' @export
#' @keywords internal
KLDivergenceLine <- function (i) geom_line(data=data.frame(Consistent    = c(10-i, 10-(2*i)), 
                                                           Contradicted  = c(i, 0),
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





TernaryEdges <- function (x) data.frame(Correct    = x['edges.agree', ],
                                        Incorrect  = x['edges.conflict', ],
                                        Unresolved = gen.nedge - x['edges.present', ])
TernaryQuart <- function (x) data.frame(Correct    = x['quartet.agree', ],
                                        Incorrect  = x['quartet.conflict', ],
                                        Unresolved = x['quartet.missing', ])

