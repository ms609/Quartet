OUTPUT <- 'pdf'
OUTPUT <- 'svg'
source('Figures/style.R')

NodeSupportData <- function(Plot, brem, bootf, bootg, jackf, jackg, mkv) {
  Plot(pch=PCH['brem' ], brem [['eq']], col=clColours['eq'])
  Plot(pch=PCH['bootF'], bootf[['eq']], col=clColours['eq'])
  Plot(pch=PCH['bootG'], bootg[['eq']], col=clColours['eq'])
  Plot(pch=PCH['jackF'], jackf[['eq']], col=clColours['eq'])
  Plot(pch=PCH['jackG'], jackg[['eq']], col=clColours['eq'])
  
  Plot(pch=PCH['brem' ], brem [['k3']], col=clColours['k3'])
  Plot(pch=PCH['bootF'], bootf[['k3']], col=clColours['k3'])
  Plot(pch=PCH['bootG'], bootg[['k3']], col=clColours['k3'])
  Plot(pch=PCH['jackF'], jackf[['k3']], col=clColours['k3'])
  Plot(pch=PCH['jackG'], jackg[['k3']], col=clColours['k3'])
  
  Plot(pch=PCH['markov'], mkv, col=clColours['mk'])
}

QuartetNodeSupportData <- function() {
  NodeSupportData(clPlotTheseAverageQuartets, clBremQuartets,
                  clBootFreqQuartets, clBootGcQuartets, clJackFreqQuartets,
                  clJackGcQuartets, clMkvQuartets)
}

SplitNodeSupportData <- function() {
  NodeSupportData(clPlotTheseAverageSplits, clBremPartitions,
                  clBootFreqPartitions, clBootGcPartitions, clJackFreqPartitions,
                  clJackGcPartitions, clMkvPartitions)
}

################################################################################
# Figure 1: Comparing node support measures
################################################################################

#dev.new()
Write(filename="Figure_1", width=FIG_WIDTH, height=FIG_WIDTH * 1.5,
      family=FONT_FAMILY, pointsize=FONT_PT)
par(mfrow=c(3, 2), mai=rep(0.1, 4), family='serif', ps=FONT_PT)

COL_SD_LINES <- '#bbbbbb'
clInitializeTernaryQuarts()
HorizontalGrid(10, grid.col=COL_SD_LINES, grid.lty='solid')
SymmetricDifferenceLines(seq(0.1, 0.9, by=0.1),
                         col=COL_SD_LINES, lty='dotted', lwd=1)

#arrow_tips <- matrix(c(TernaryCoords(3, 19-6, 3), TernaryCoords(6.5, 19-13, 6.5), TernaryCoords(3, 6.5, 19-(6.5+3))), 2, 3)
#arrows(arrow_tips[1, 1], arrow_tips[2, 1], arrow_tips[1, 2], arrow_tips[2, 2], length=0.08, col='#666666')
#arrows(arrow_tips[1, 1], arrow_tips[2, 1], arrow_tips[1, 3], arrow_tips[2, 3], length=0.08, col='#666666')
#text(mean(arrow_tips[1, 1:2]) + 0.01, mean(arrow_tips[2, 1:2]), 
#     "Increasing quality\n(Congreve & Lamsdell)",
#     cex=FONT_SIZE, srt=58, pos=1, col='#666666')
#text(mean(arrow_tips[1, c(1, 3)]) - 0.02, mean(arrow_tips[2, c(1, 3)]),
#     "Increasing quality\n(Divergence)", 
#     cex=FONT_SIZE, srt=90, pos=3, col='#666666')

legend('bottomright', bty='n', cex=FONT_SIZE, lwd=c(1, 1, 2), 
       col=c(COL_SD_LINES, COL_SD_LINES, cbPalette8[8]), 
       lty=c('dotted', 'solid', 'dotted'), 
       legend=c('Equal SD/TIP', 'Equal SD/MaxI', 'Similarity of random tree'))
Panel('a')
##########


clInitializeTernarySplits()

equal_point <- SplitsToPoints(t(as.matrix(rowMeans(clBootGcPartitions[['eq']][1, , ]))))
equal_differents <- equal_point['d', ]
equal_coords <- TernaryCoords(equal_point)
COL_LINES <- cbPalette8[7]


rightPoint <- TernaryCoords(1, 0, 0)
rightX <- rightPoint[1]
otherYs <- vapply(2 * (1:9), function (p) TernaryCoords(p, 19-p, 0), double(2))[2, ]
lapply(otherYs, function (y) lines(c(0, rightX, 0), c(y, rightPoint[2], -y),
                                   lty='dashed', col='#00000022'))

TernaryLines(list(c(0, equal_differents, 19-equal_differents),
                  c(19-equal_differents, equal_differents, 0)), 
             col=COL_LINES, lty='longdash', lwd=1.5) # Equal differents

lines(c(0, 2 * (0.5 - equal_coords[2]) * rightX), rep(equal_coords[2], 2), 
      col=COL_LINES, lty='solid', lwd=1.75) # Equally different

#SymmetricDifferenceLines(sum(equal_point[c('d', 'd', 'r2'), ]) /
#                           sum(equal_point[c('s', 's', 'd', 'd', 'r2'), ]),
#                         col=COL_LINES, lty='dotted', lwd=1.75)

lines(rep(equal_coords[1], 2), c(-1, +1) * 0.5 * (1 - (equal_coords[1] / rightX)),
      col=COL_LINES, lty='dotdash', lwd=1.5) # Equal resolution

lines(c(sqrt(0.75), 0), c(0, equal_coords[2] / (1 - (equal_coords[1] / rightX) ) ),
      col=COL_LINES, lty='dashed', lwd=1.5) # Equal proportion

arrow_tips <- matrix(c(TernaryCoords(3, 19-6, 3), TernaryCoords(6.5, 19-13, 6.5), TernaryCoords(3, 6.5, 19-(6.5+3))), 2, 3)
arrows(arrow_tips[1, 1], arrow_tips[2, 1], arrow_tips[1, 2], arrow_tips[2, 2], length=0.08, col='#666666')
arrows(arrow_tips[1, 1], arrow_tips[2, 1], arrow_tips[1, 3], arrow_tips[2, 3], length=0.08, col='#666666')
text(mean(arrow_tips[1, 1:2]) + 0.01, mean(arrow_tips[2, 1:2]), 
     "Increasing quality\n(Congreve & Lamsdell)",
     cex=FONT_SIZE, srt=58, pos=1, col='#666666')
text(mean(arrow_tips[1, c(1, 3)]) - 0.02, mean(arrow_tips[2, c(1, 3)]),
     "Increasing quality\n(SD/MaxI)", 
     cex=FONT_SIZE, srt=90, pos=3, col='#666666')
#AddLegend()
legend('bottomright', bty='n', cex=FONT_SIZE, lwd=1.2, col=COL_LINES, 
       lty=c('solid', 'dotdash', 'dashed', 'longdash'), 
       legend=c('Equal SD/MaxI', 'Equal resolution', 'Equal proportion of partitions correct', 'Equal number of incorrect partitions'))
Panel('b')
##########



##########
##########



bLim <- c(0, 0.18, 0.372, 0.405)

clInitializeTernaryQuarts()
QuartetNodeSupportData()
AddArrows('Less informative (SD/MaxI)')
rect(xleft=bLim[1], ybottom=bLim[3], xright=bLim[2], ytop=bLim[4], border='#00000088', lty='dashed')
text(x=bLim[1] - 0.01, y=bLim[3] - 0.03, labels="See right", cex=FONT_SIZE, pos=4)
rightPoint <- TernaryCoords(1, 0, 0)
otherYs <- vapply(2 * seq_len(GRID_LINES - 1L),
                  function (p) TernaryCoords(p, GRID_LINES - p, 0),
                  double(2))[2, ]
lapply(otherYs, function (y) lines(c(0, rightPoint[1]), c(y, rightPoint[2]),
                                   lty='dashed', col='#00000022'))

legend('bottomright', bty='n', cex=FONT_SIZE,
       lty=0,
       pch=PCH[c('mk', 'brem', 'bootF', 'bootG', 'jackF', 'jackG')], pt.cex=1.2,
       col=COL['black'],
       legend=c('Posterior probability', 
                'Bremer', 'BootFreq', 'Boot GC', 'Jack Freq', 'Jack GC')
)
Panel('c')
##########

clInitializeTernaryQuarts(padding=0.001, xLim=bLim[1:2], yLim=bLim[3:4])
QuartetNodeSupportData()

TernaryText(apply(clBootGcQuartets[['k3']][, c('r2', 'd', 's'), ], 2, rowMeans),
            labels=SUBOPTIMAL[['gc']], col=clColours['k3'], pos=3)
TernaryText(apply(clBremQuartets[['k3']][, c('r2', 'd', 's'), ], 2, rowMeans),
            labels=SUBOPTIMAL[['brem']], col=clColours['k3'], pos=1)
text(x=bLim[1], y=bLim[3], "(Vertically exaggerated)", cex=FONT_SIZE, pos=4)
Panel("c'")
##########

clInitializeTernarySplits()
SplitNodeSupportData()
legend('topright', bty='n', cex=FONT_SIZE, lty=1, lwd=2,
       col=COL[c('k3', 'mk', 'eq')],
       legend=c('Implied weights (k = 3)', 'Bayesian (Mk)', 'Equal weights')
)
dLim <- c(0, 0.5, 0.15, 0.25)
rect(xleft=dLim[1], ybottom=dLim[3], xright=dLim[2], ytop=dLim[4], border='#00000088', lty='dashed')
text(x=dLim[2] + 0.02, y=dLim[3] - 0.04, labels="See right", cex=FONT_SIZE, pos=2)

Panel('d')
##########

clInitializeTernarySplits(xlim=dLim[1:2], ylim=dLim[3:4], padding=0.001)
SplitNodeSupportData()

TernaryText(SplitsToPoints(apply(clBootGcPartitions[['k3']][, , ], 2, rowMeans)),
            labels=paste0(SUBOPTIMAL[['gc']]), col=clColours['k3'], pos=3, offset=0.5)

TernaryText(SplitsToPoints(apply(clBremPartitions[['k3']][, , ], 2, rowMeans)),
            labels=SUBOPTIMAL[['brem']], col=clColours['k3'], pos=4)

TernaryText(SplitsToPoints(apply(clBootGcPartitions[['eq']][, , ], 2, rowMeans)),
            labels=SUBOPTIMAL[['gc']], col=clColours['eq'], pos=2)

text(x=dLim[1], y=dLim[3], "(Vertically exaggerated)", cex=FONT_SIZE, pos=4)
Panel("d'")
##########

dev.off()
