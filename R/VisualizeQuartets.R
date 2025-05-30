#' Visualize quartet difference on trees, by split
#' 
#' @param tree1,tree2 Trees of class `phylo`, with identical leaf labels.
#' @param setPar Logical specifying whether graphical parameters should be set
#'  to display trees side by side.
#' @param style Character string specifying split labels with an unambiguous 
#' abbreviation of:
#'  - `label`: Label stating proportion of resolved quartets in agreement,
#'  coloured accordingly;
#'  - `pie`: Pie chart showing proportion of quartets in agreement, sized
#'     according to number of quartets influenced by each split;
#'  - `bar`: Bar showing proportion of quartets in agreement, labelled;
#'  - `size`: Circle coloured according to proportion of quartets in agreement,
#'  with area corresponding to number of quartet statements associated with
#'  split.
#' @param precision Integer specifying number of significant figures to display
#' when reporting matching scores.
#' @param spectrum 101-element vector specifying a range of colours by which
#' to colour matches.
#' @param legend Logical specifying whether to display simple legend.
#' @param scale Numeric, enlargement factor for split labels.
#' @param Plot Function to use to plot trees.
#' @param \dots Additional parameters to send to `Plot()`.
#' 
#' @returns `VisualizeQuartets()` invisibly returns a list with two elements, 
#' named `tree1` and `tree2`, containing a matrix.
#' Each row corresponds to a split within that tree; columns correspond to:
#' \describe{
#'   \item{node}{The internal numbering of the node corresponding to each split,
#'   as displayed by `ape::nodelabels()`}
#'   \item{N, Q, s, d, r1, r2, u}{The status of each quartet relative to that
#'    split, as documented in [`QuartetStatus()`]}
#'   \item{res}{The number of quartets resolved by that split, i.e. `s` + `d`}
#'   \item{same}{The proportion of quartets resolved by that node that are 
#'   resolved in the same manner in the other tree; i.e. `s / s + d`}
#' }
#' 
#' @examples
#' library("TreeTools", quietly = TRUE)
#' # Simple plot
#' VisualizeQuartets(BalancedTree(10), CollapseNode(PectinateTree(10), 19),
#'                   style = "label")
#'
#' # Plot with custom graphical parameters
#' origPar <- par(mfrow = c(2, 2))
#' VisualizeQuartets(BalancedTree(10), CollapseNode(PectinateTree(10), 19),
#'                   setPar = FALSE)
#' VisualizeQuartets(BalancedTree(10), CollapseNode(PectinateTree(10), 19),
#'                   style = "bar", legend = FALSE, setPar = FALSE)
#' 
#' # Circle size denotes similarity
#' par(mfrow = c(2, 1), mar = rep(0.1, 4))
#' vq <- VisualizeQuartets(
#'   tree1 = BalancedTree(20),
#'   tree2 = CollapseNode(PectinateTree(20), 29:33),
#'   style = "size", scale = 2,
#'   setPar = FALSE # necessary for node labels to align
#' )
#' # Manually add custom node labels
#' percentSame <- paste(round(vq[["tree2"]][, "same"] * 100, 1), "%")
#' nodelabels(percentSame, vq[["tree2"]][, "node"],
#'            frame = "n", bg = NA, # No frame or background
#             cex = 0.8, # character expansion / font size
#'            adj = 0.5 # align label
#'            )
#'            
#' # restore original graphical parameters
#' par(origPar)
#' @template MRS
#' @importFrom ape plot.phylo
#' @importFrom PlotTools SpectrumLegend
#' @importFrom viridisLite viridis
#' @export
VisualizeQuartets <- function (tree1, tree2, style = "pie",
                               setPar = TRUE,
                               precision = 3L,
                               Plot = plot.phylo,
                               scale = 1L,
                               spectrum = viridisLite::viridis(101),
                               legend = TRUE,
                               ...) {
  if (setPar) {
    origPar <- par(mfrow = c(1, 2), mar = rep(0.1, 4), cex = 0.8)
    on.exit(par(origPar))
  }
  
  Plot(tree1, ...)
  vq1 <- .VQPanel(tree1, tree2, style = style, scale = scale,
                  precision = precision, spectrum = spectrum)
  if (isTRUE(legend)) {
    Legend2 <- function() {
      legend("topleft", c("Quartets match", "Quartets differ"), bty = "n",
             pch = 15, col = spectrum[c(101, 1)])
    }
    Legend5 <- function() {
      PlotTools::SpectrumLegend(
        "topleft",
        palette = spectrum,
        legend = c("100% quartets match", "75%", "50%", "25%", "100% differ"),
        bty = "n")
    }
    switch(pmatch(style, c("label", "bar", "pie", "size")),
           Legend5(), Legend2(), Legend2(), Legend5())
  }
  Plot(tree2, ...)
  vq2 <- .VQPanel(tree2, tree1, style = style, scale = scale,
                  precision = precision, spectrum = spectrum)
  
  # Return:
  invisible(list(tree1 = vq1, tree2 = vq2))
}

#' @importFrom TreeTools as.Splits CollapseNode
#' @importFrom ape nodelabels
.VQPanel <- function (tr1, tr2, style, scale, precision, spectrum) {
  
  splits <- as.integer(names(as.Splits(tr1)))
  allNodes <- NTip(tr1) + seq_len(tr1$Nnode)[-1]
  oneNode <- lapply(splits, function (keep) {
    CollapseNode(tr1, allNodes[allNodes != keep])
  })
  qs <- QuartetStatus(oneNode, tr2)
  resolved <- qs[, "s"] + qs[, "d"]
  sames <- qs[, "s"] / (qs[, "s"] + qs[, "d"])
  
  switch(pmatch(tolower(style), c("label", "bar", "pie", "size")),
         ## Coloured according to % same
         nodelabels(signif(sames, precision), splits, cex = scale,
                    bg = spectrum[1 + ceiling(100 * sames)]),
         ## Thermo bars
         nodelabels(signif(sames, precision), splits, cex = scale,
                    frame = "n", adj = -0.1,
                    thermo = sames, piecol = spectrum[c(101, 1)]),
         ## Pie charts
         nodelabels("", splits, frame = "n",
                    cex = sqrt(resolved / qs[1, "N"]) * 5L * scale,
                    pie = sames, piecol = spectrum[c(101, 1)]),
         ## Size according to total number of quartet statements
         nodelabels("", splits, pch = 16, frame = "none",
                    col = spectrum[1 + ceiling(100 * sames)],
                    cex = sqrt(resolved / qs[1, "N"]) * 10L * scale)
  )
  
  # Return:
  invisible(cbind(node = splits, qs, res = resolved, same = sames))
}
