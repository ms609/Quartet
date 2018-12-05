OUTPUT <- 'pdf'
source('inst/Figures/style.R')

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
Write(filename="Figure_1", width=FIG_WIDTH, height=FIG_WIDTH,
      family=FONT_FAMILY, pointsize=FONT_PT)
par(mfrow=c(2, 2), mai=rep(0.1, 4), family='serif', ps=FONT_PT)

bLim <- c(0, 0.18, 0.372, 0.405)

clInitializeTernaryQuarts()
QuartetNodeSupportData()
AddArrows('Increasing quartet dissimilarity')
rect(xleft=bLim[1], ybottom=bLim[3], xright=bLim[2], ytop=bLim[4], border='#00000088', lty='dashed')
text(x=bLim[1] - 0.01, y=bLim[3] - 0.03, labels='Panel (b)', cex=FONT_SIZE, pos=4)
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
Panel('a')
##########

clInitializeTernaryQuarts(padding=0.001, xLim=bLim[1:2], yLim=bLim[3:4])
QuartetNodeSupportData()
text(x=bLim[1], y=bLim[3], "(Vertically exaggerated)", cex=FONT_SIZE, pos=4)
Panel('b')
##########

clInitializeTernarySplits()
SplitNodeSupportData()
legend('topright', bty='n', cex=FONT_SIZE, lty=1, lwd=2,
       col=COL[c('k3', 'mk', 'eq')],
       legend=c('Implied weights (k = 3)', 'Bayesian (Mk)', 'Equal weights')
)
dLim <- c(0, 0.5, 0.15, 0.25)
rect(xleft=dLim[1], ybottom=dLim[3], xright=dLim[2], ytop=dLim[4], border='#00000088', lty='dashed')
text(x=dLim[2] + 0.02, y=dLim[3] - 0.04, labels='Panel (d)', cex=FONT_SIZE, pos=2)

Panel('c')
##########

clInitializeTernarySplits(xlim=dLim[1:2], ylim=dLim[3:4], padding=0.001)
SplitNodeSupportData()
text(x=dLim[1], y=dLim[3], "(Vertically exaggerated)", cex=FONT_SIZE, pos=4)
Panel('d')
##########

dev.off()
