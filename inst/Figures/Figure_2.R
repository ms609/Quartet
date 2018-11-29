OUTPUT = 'pdf'
source('inst/Figures/style.R')

AddArrows <- function (quality) {
  arrows(sqrt(3/4) * 0.5, 0.5, sqrt(3/4) * 0.8, 0.5, length=0.08)
  text  (sqrt(3/4) * 0.65, 0.5, pos=3, 'Decreasing resolution', cex=FONT_SIZE)
  arrows(sqrt(3/4) * 0.98, 0.40, sqrt(3/4) * 0.98, 0.20, length=0.08)
  text  (sqrt(3/4) * 1.01, 0.30, pos=3, quality, cex=FONT_SIZE, srt=270)
}
################################################################################

TernaryQuarts <- function(zoom=1, padding=0.1) {
  clInitializeTernaryQuarts(zoom=zoom, padding=padding)
  clPlotAverageQuartets(clBootGcQuartets, pch=PCH, col=COL)
  clPlotTheseAverageQuartets(clMkvQuartets, col=clColours['mk'], pch=PCH_MK)
}

TernarySplits <- function(plotChars = TRUE) {
  clInitializeTernarySplits()
  clPlotAverageSplits(clBootGcPartitions, col=COL, pch=if(plotChars) PCH else NA)
  clPlotTheseAverageSplits(clMkvPartitions, col=clColours['mk'], pch=PCH_MK)
}

AddLegend <- function(pos='bottomright')
  legend(pos, cex=FONT_SIZE, bty='n',
         lty=1,
         pch=PCH[c('mk', 'eq', 'kX', 'k5', 'k3', 'k1', 'kC')], pt.cex=1.1,
         col=COL[c('mk', 'eq', 'kX', 'k5', 'k3', 'k1', 'kC')],
         legend=c('Markov', 'Equal weights', paste0('Implied, k=', c(10, 5, '2, 3', 1, '2..10')))
  )


#dev.new()

Write(filename="Figure_2", width=FIG_WIDTH, height=FIG_WIDTH,
      family=FONT_FAMILY, pointsize=FONT_PT)
par(mfrow=c(2, 2), mai=rep(0, 4), family='serif', ps=FONT_PT)

TernaryQuarts()

AddArrows('Increasing quartet dissimilarity')
rect(xleft=-0.01, ybottom=0.19, xright=0.278, ytop=0.52, border='#00000088', lty='dashed')
text(x=0.30, y=0.54, labels='Panel (b)', cex=FONT_SIZE, pos=2)
rightPoint <- TernaryCoords(1, 0, 0)
otherYs <- vapply(2 * seq_len(GRID_LINES - 1L),
                  function (p) TernaryCoords(p, GRID_LINES - p, 0),
                  double(2))[2, ]
lapply(otherYs, function (y) lines(c(0, rightPoint[1]), c(y, rightPoint[2]),
                                   lty='dashed', col='#00000022'))

legend('bottomright', bty='n', cex=FONT_SIZE,
       lty=c('dotted', 'dashed', 'dotted'),
       lwd=c(1, 1, 2), col=c('grey', '#00000044', cbPalette8[8]),
       legend=c('Equal divergence', 'Equal accuracy', 'Similarity of random tree'))

Panel('a')
##########

par(mai=c(0, 0.15, 0, 0.15))
TernaryQuarts(zoom=3.5, padding=0.01)
text(-0.015, 0.345, 'Identical quartets', cex=FONT_SIZE, srt=90)
text(0.126, 0.45, 'Unresolved quartets', cex=FONT_SIZE, srt=330)
lapply(otherYs, function (y) lines(c(0, rightPoint[1]), c(y, rightPoint[2]),
                                   lty='dashed', col='#00000022'))

AddLegend('topright')
Panel('b')
##########

TernarySplits()
AddArrows("Increasing symmetric difference")
#AddLegend()
Panel('c')
##########


TernarySplits(FALSE)
clPlotBestAverageSplits(clBootGcPartitions, pch=PCH)
rightPoint <- TernaryCoords(0, 0, 1)
otherYs <- vapply(2*(1:8), function (p) TernaryCoords(p, 19-p, 0), double(2))[2, ]
lapply(otherYs, function (y) lines(c(0, rightPoint[1]), c(y, rightPoint[2]),
                                   lty='dashed', col='#00000022'))

equal_point <- CLAverageSplits('equal')[, 1]
equal_differents <- equal_point[2]
COL_LINES = cbPalette8[7]
TernaryLines(list(c(0, equal_differents, 20-equal_differents),
                  c(20-equal_differents, equal_differents, 0)), 
             col=COL_LINES, lty='longdash', lwd=1.5)
equal_coords <- TernaryCoords(equal_point)
lines(c(0, sqrt(3/4) * (0.5 + equal_coords[2])), rep(equal_coords[2], 2), 
      col=COL_LINES, lty='dotted', lwd=1.75)
lines(rep(equal_coords[1], 2), c(-1, +1) * 0.5 * (1 - equal_coords[1] / sqrt(3/4)),
      col=COL_LINES, lty='dotdash', lwd=1.5)

lines(c(sqrt(0.75), 0), c(0, equal_coords[2] / (1 - (equal_coords[1] / sqrt(3/4)) ) ),
      col=COL_LINES, lty='dashed', lwd=1.5)

arrow_tips <- matrix(c(TernaryCoords(3, 19-6, 3), TernaryCoords(6.5, 19-13, 6.5), TernaryCoords(3, 6.5, 19-(6.5+3))), 2, 3)
arrows(arrow_tips[1, 1], arrow_tips[2, 1], arrow_tips[1, 2], arrow_tips[2, 2], length=0.08, col='#666666')
arrows(arrow_tips[1, 1], arrow_tips[2, 1], arrow_tips[1, 3], arrow_tips[2, 3], length=0.08, col='#666666')
text(mean(arrow_tips[1, 1:2]) + 0.01, mean(arrow_tips[2, 1:2]), 
     "Increasing quality\n(Congreve & Lamsdell)",
     cex=FONT_SIZE, srt=58, pos=1, col='#666666')
text(mean(arrow_tips[1, c(1, 3)]) - 0.02, mean(arrow_tips[2, c(1, 3)]),
     "Increasing quality\n(Divergence)", 
     cex=FONT_SIZE, srt=90, pos=3, col='#666666')
#AddLegend()
legend('bottomright', bty='n', cex=FONT_SIZE, lwd=1.2, col=COL_LINES, 
       lty=c('dotted', 'dotdash', 'dashed', 'longdash'), 
       legend=c('Equally informative', 'Equal resolution', 'Equal accuracy', 'Equal incorrect nodes'))
Panel('d')
##########




dev.off()
