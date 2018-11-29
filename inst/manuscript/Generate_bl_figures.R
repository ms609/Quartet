require('ape')
require('Quartet')
library('Ternary')
library('CongreveLamsdell2016')
data('clBremPartitions', 'clBremQuartets', 'clMkvPartitions', 'clMkvQuartets',
     'clBootFreqPartitions', 'clBootFreqQuartets', 'clBootGcPartitions', 'clBootGcQuartets',
     'clJackFreqPartitions', 'clJackFreqQuartets', 'clJackGcPartitions', 'clJackGcQuartets',
     'clCI')
library('OReillyEtAl2016')
data('orQuartets')
data('orPartitions')

source('style.R')

# Use Inkscape to generate EPS from SVG.  R creates bitmap EPS due to semitrans.
Write <- function(filename, ...) svg(filename=paste0("inst/figures/", filename, ".svg"), ...)
Write <- function(filename, ...) cairo_pdf(filename=paste0("inst/figures/", filename, ".pdf"), ...)

AddArrows <- function (quality) {
  arrows(sqrt(3/4) * 0.5, 0.5, sqrt(3/4) * 0.8, 0.5, length=0.08)
  text  (sqrt(3/4) * 0.65, 0.5, pos=3, 'Decreasing resolution', cex=FONT_SIZE)
  arrows(sqrt(3/4) * 0.98, 0.40, sqrt(3/4) * 0.98, 0.20, length=0.08)
  text  (sqrt(3/4) * 1.01, 0.30, pos=3, quality, cex=FONT_SIZE, srt=270)
}

AddLegend2 <- function(pos='bottomright')
  legend(pos, cex=FONT_SIZE, bty='n',
         lty=1,
         pch=PCH[c('mk', 'eq', 'none', 'k5', 'none', 'k1', 'kC')], pt.cex=1.1,
         col=COL[c('mk', 'eq', 'kX', 'k5', 'k3', 'k1', 'kC')],
         legend=c('Markov', 'Equal weights', paste0('Implied, k=', c(10, 5, '2, 3', 1, '2..10')))
  )

AverageQuarts <- function (item) apply(clQuartets[[item]][c('r2', 'd', 's'), , ], 2, rowMeans, na.rm=TRUE)
ORAverageQuarts <- function (nchar, item) apply(orQuartets[[as.character(nchar)]][[item]][c('r2', 'd', 's'), , ], 2, rowMeans, na.rm=TRUE)

################################################################################
# Figure 0: Comparing node support measures
################################################################################


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

#dev.new()
Write(filename="Figure_0", width=FIG_WIDTH, height=FIG_WIDTH,
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

clInitializeTernaryQuarts(zoom=bZoom, padding=0.001, xLim=bLim[1:2], yLim=bLim[3:4])
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

################################################################################
################################################################################
################################################################################

TernaryQuarts2 <- function(zoom=1, padding=0.1) {
  clInitializeTernaryQuarts(zoom=zoom, padding=padding)
  clPlotAverageQuartets(clBootGcQuartets, pch=PCH, col=COL)
  clPlotTheseAverageQuartets(clMkvQuartets, col=clColours['mk'], pch=PCH_MK)
}

TernarySplits2 <- function(plotChars = TRUE) {
  clInitializeTernarySplits()
  clPlotAverageSplits(clBootGcPartitions, col=COL, pch=if(plotChars) PCH else NA)
  clPlotTheseAverageSplits(clMkvPartitions, col=clColours['mk'], pch=PCH_MK)
}

#dev.new()

Write(filename="Figure_1", width=FIG_WIDTH, height=FIG_WIDTH,
      family=FONT_FAMILY, pointsize=FONT_PT)
par(mfrow=c(2, 2), mai=rep(0, 4), family='serif', ps=FONT_PT)

TernaryQuarts2()

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

TernarySplits2()
AddArrows("Increasing symmetric difference")
#AddLegend()
Panel('c')
##########


TernarySplits2(FALSE)
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




##################################################################################
#                                                                                #
#                                 FIGURE 2 (née 3)                               #
#                                                                                #
##################################################################################

totalQuarts <- orQuartets[['350']][['equal']][1, 1, 1] # = 37 * 73 * 9 * 50
AXIS_LABELS <- c(0, paste0(round(1:GRID_LINES * totalQuarts / GRID_LINES / 1000), 'k'))

Fig2Ternary <- function(title.text, ORFunc) {
  TernaryPlot(NULL, NULL, NULL, #'Unresolved', 'Different', 'Same', 
            alab="Unresolved quartets",
            blab="Different quartets",
            clab="Identical quartets",
            lab.cex=FONT_SIZE, lab.offset=0.14,
            col=BG_COL, point='right',
            grid.lines = GRID_LINES, grid.lty='solid', grid.col=GRID_COL,
            grid.minor.lines = 0,
            axis.col=rgb(0.6, 0.6, 0.6), #axis.labels.col = 'black',
            padding=0.1, axis.labels = AXIS_LABELS)
  HorizontalGrid(GRID_LINES)
  title(main=title.text, cex.main=FONT_SIZE)
  lapply(orAnalyses, function (analysis) {
    TernaryLines(ORFunc(analysis), col=COL[analysis], pch=PCH['dot'],
                 lty=LTY[analysis], lwd=LWD[analysis])
    TernaryPoints(ORFunc(analysis)[, 1], col=COL[analysis], 
                  pch=PCH[analysis], cex=1.1)
  })
}
Fig2Zoom <- function (zoom, ORFunc) {
  TernaryPlot(col=BG_COL, point='right',
              grid.lines = GRID_LINES, grid.lty='solid', grid.col=GRID_COL,
              grid.minor.lines = 0,
              axis.col=rgb(0.6, 0.6, 0.6),
              padding=0.01, axis.labels = AXIS_LABELS,
              ticks.length = 0.025 / zoom,
              xlim = c(0, 1 / zoom),
              ylim = c(0.5-(1/zoom), 0.5))
  HorizontalGrid(GRID_LINES)
  lapply(orAnalyses, function (analysis) {
    TernaryLines(ORFunc(analysis), col=COL[analysis],  pch=PCH['dot'],
                 lty=LTY[analysis], lwd=LWD[analysis])
    TernaryPoints(ORFunc(analysis)[, 1], col=COL[analysis],
                  pch=PCH[analysis], cex=1.1)
  })
}

InsetBox <- function (ybottom, xright, text) {
  rect(xleft=-0.01, ybottom=ybottom, xright=xright, ytop=0.52,
       border='#00000088', lty='dashed')
  text(x=xright + 0.01, y=0.49, labels=text, cex=FONT_SIZE, col='#00000088', pos=2)
}

Write(filename="Figure_2", width=FIG_WIDTH, height=FIG_WIDTH*2/3,
      family=FONT_FAMILY, pointsize=FONT_PT)

par(mar=rep(0, 4), mfrow=c(2, 3), mai=rep(0, 4), ps=FONT_PT)
Fig2Ternary("\n100 characters", ORQ100)
InsetBox(0.026, 0.41, '(below)')
Panel('a')
Fig2Ternary("\n350 characters", ORQ350)
InsetBox(0.026, 0.41, '(below)')
Panel('b')
Fig2Ternary("\n1000 characters", ORQ1000)
InsetBox(0.33, 0.151, '')
Panel('c')

analyses <- orAnalyses[c(7, 8, 1:6)]
legend('bottomright', bty='n', lty=1, pch=PCH[analyses], col=COL[analyses],
       pt.cex=1.1,
       legend=c('Markov', 'Equal weights', paste0('Implied, k=', 
                                                  rev(c(2, 3, 5, 10, 20, 200))))
  )


par(mai=c(0, 0.15, 0, 0.15))

Fig2Zoom(2.25, ORQ100)
TernaryText(ORAverageQuarts(100, 3)[, 1], col=COL_5, pos=1, label='k = 2', cex=FONT_SIZE)
TernaryText(ORAverageQuarts(100, 4)[, 1], col=COL_5, pos=1, label='k = 3', cex=FONT_SIZE)
TernaryText(ORAverageQuarts(100, 5)[, 1], col=COL_5, pos=1, label='k = 5, 200', cex=FONT_SIZE)
TernaryText(ORAverageQuarts(100, 6)[, 1], col=COL_5, pos=3, label='k = 10, 20', cex=FONT_SIZE)
#TernaryText(ORAverageQuarts(100, 7)[, 1], col=COL_2, pos=1, label='k = 20', cex=0.8)
#TernaryText(ORAverageQuarts(100, 8)[, 1], col=COL_2, pos=1, label='k = 200', cex=0.8)
Fig2Zoom(2.25, ORQ350)
Fig2Zoom(6.5, ORQ1000)

dev.off()

