OUTPUT <- PDF
source('inst/Figures/style.R')
library('OReillyEtAl2016')
data('orQuartets')
data('orPartitions')

AverageQuarts <- function (item) apply(clQuartets[[item]][c('r2', 'd', 's'), , ], 2, rowMeans, na.rm=TRUE)
ORAverageQuarts <- function (nchar, item) apply(orQuartets[[as.character(nchar)]][[item]][c('r2', 'd', 's'), , ], 2, rowMeans, na.rm=TRUE)
##################################################################################
#                                                                                #
#                                   FIGURE 3                                     #
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

