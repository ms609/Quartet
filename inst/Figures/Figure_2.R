OUTPUT <- 'pdf'
OUTPUT <- 'svg'
source('inst/Figures/style.R')
load_all('../OReillyEtAl2016') # TODO REMOVE THIS LINE
orQuartets <- OReillyEtAl2016::orQuartets
orPartitions <- OReillyEtAl2016::orPartitions
orAnalyses <- names(orPartitions[[1]])

ORAverageSplits <- function (nchar, item) {
  AverageSplits(orPartitions[[as.character(nchar)]][[item]][, , ])
}

ORAverageQuarts <- function (nchar, item) {
  apply(orQuartets[[as.character(nchar)]][[item]][, c('r2', 'd', 's'), ],
        2, rowMeans, na.rm=TRUE)
}

################################################################################

TernaryQuarts <- function(zoom=1, padding=0.1) {
  clInitializeTernaryQuarts(zoom=zoom, padding=padding)
  clPlotAverageQuartets(clBootGcQuartets, pch=PCH, col=COL)
  clPlotTheseAverageQuartets(clMkvQuartets, col=clColours['mk'], pch=PCH['mk'])
}

TernarySplits <- function(plotChars = TRUE) {
  clInitializeTernarySplits()
  clPlotAverageSplits(clBootGcPartitions, col=COL, pch=if(plotChars) PCH else NA)
  clPlotTheseAverageSplits(clMkvPartitions, col=clColours['mk'], pch=PCH['mk'])
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
par(mfrow=c(3, 3), mai=rep(0, 4), family='serif', ps=FONT_PT)

TernaryQuarts()

AddArrows('Less informative (SD/MaxI)')
rect(xleft=-0.01, ybottom=0.19, xright=0.278, ytop=0.52, border='#00000088', lty='dashed')
text(x=0.30, y=0.54, labels='(right)', cex=FONT_SIZE, pos=2)
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

Panel("a")
##########

par(mai=c(0, 0.15, 0, 0.15))
TernaryQuarts(zoom=3.5, padding=0.01)
text(-0.015, 0.345, 'Identical quartets', cex=FONT_SIZE, srt=90)
text(0.126, 0.45, 'Unresolved quartets', cex=FONT_SIZE, srt=330)
lapply(otherYs, function (y) lines(c(0, rightPoint[1]), c(y, rightPoint[2]),
                                   lty='dashed', col='#00000022'))

AddLegend('topright')
Panel("a'")
##########

TernarySplits()
AddArrows("Less informative (SD/MaxI)")
#AddLegend()
Panel("b")
##########




##################################################################################
#                                                                                #
#                                   O'Reilly                                     #
#                                                                                #
##################################################################################

totalQuarts <- orQuartets[['350']][['equal']][1, 1, 1] # = 37 * 73 * 9 * 50
AXIS_LABELS <- c(0, paste0(round(1:GRID_LINES * totalQuarts / GRID_LINES / 1000), 'k'))

ORQ100 <- function (x) ORAverageQuarts(100, x)
ORQ350 <- function (x) ORAverageQuarts(350, x)
ORQ1000 <- function (x) ORAverageQuarts(1000, x)

ORS100 <- function (x) ORAverageSplits(100, x)
ORS350 <- function (x) ORAverageSplits(350, x)
ORS1000 <- function (x) ORAverageSplits(1000, x)

FigTernary <- function(titleText, ORFunc) {
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
  title(main=titleText, cex.main=FONT_SIZE)
  lapply(orAnalyses, function (analysis) {
    TernaryLines(ORFunc(analysis), col=COL[analysis], pch=PCH['dot'],
                 lty=LTY[analysis], lwd=LWD[analysis])
    TernaryPoints(ORFunc(analysis)[1, ], col=COL[analysis], 
                  pch=PCH[analysis], cex=1.1)
  })
  
  # Return:
  invisible()
}
FigZoom <- function (zoom, ORFunc) {
  TernaryPlot(col=BG_COL, point='right',
              grid.lines = GRID_LINES, grid.lty='solid', grid.col=GRID_COL,
              grid.minor.lines = 0,
              axis.col=rgb(0.6, 0.6, 0.6),
              padding=0.1/zoom, axis.labels = AXIS_LABELS,
              ticks.length = 0.025 / zoom,
              xlim = c(0, 1 / zoom),
              ylim = c(0.5-(1/zoom), 0.5))
  HorizontalGrid(GRID_LINES)
  lapply(orAnalyses, function (analysis) {
    TernaryLines(ORFunc(analysis), col=COL[analysis],  pch=PCH['dot'],
                 lty=LTY[analysis], lwd=LWD[analysis])
    TernaryPoints(ORFunc(analysis)[1, ], col=COL[analysis],
                  pch=PCH[analysis], cex=1.1)
  })
  
  # Return:
  invisible()
}

InsetBox <- function (zoom, text) {
  rect(xleft=-0.01, ybottom=0.5-(1/zoom), xright=1 / zoom, ytop=0.52,
       border='#00000088', lty='dashed')
  text(x=(1 / zoom) + 0.01, y=0.49, labels=text, cex=FONT_SIZE, col='#00000088', pos=2)
}

#Write(filename="Figure_3", width=FIG_WIDTH, height=FIG_WIDTH*2/3,
#      family=FONT_FAMILY, pointsize=FONT_PT)
#
#par(mar=rep(0, 4), mfrow=c(2, 3), mai=rep(0, 4), ps=FONT_PT)
FigTernary("\n100 characters", ORQ100)
InsetBox(2.25, '(below)')
Panel('c')
FigTernary("\n350 characters", ORQ350)
#InsetBox(6.5, '(below)')
Panel('d')
FigTernary("\n1000 characters", ORQ1000)
InsetBox(6.5, '')
Panel('e')

analyses <- orAnalyses[c(7, 8, 1:6)]
legend('bottomright', bty='n', lty=1, pch=PCH[analyses], col=COL[analyses],
       pt.cex=1.1,
       legend=c('Markov', 'Equal weights', paste0('Implied, k=', 
                                                  rev(c(2, 3, 5, 10, 20, 200))))
)


par(mai=c(0, 0.15, 0, 0.15))

FigZoom(2.25, ORQ100)
Panel("c'")
TernaryText(ORAverageQuarts(100, 3)[1, ], col=COL['k5'], pos=1, label='k = 2', cex=FONT_SIZE)
TernaryText(ORAverageQuarts(100, 4)[1, ], col=COL['k5'], pos=1, label='k = 3', cex=FONT_SIZE)
TernaryText(ORAverageQuarts(100, 5)[1, ], col=COL['k5'], pos=1, label='k = 5, 200', cex=FONT_SIZE)
TernaryText(ORAverageQuarts(100, 6)[1, ], col=COL['k5'], pos=3, label='k = 10, 20', cex=FONT_SIZE)
#TernaryText(ORAverageQuarts(100, 7)[1, ], col=COL_2, pos=1, label='k = 20', cex=0.8)
#TernaryText(ORAverageQuarts(100, 8)[, 1], col=COL_2, pos=1, label='k = 200', cex=0.8)
FigZoom(6.5, ORQ350)
Panel("d'")
FigZoom(6.5, ORQ1000)
Panel("e'")

dev.off()
