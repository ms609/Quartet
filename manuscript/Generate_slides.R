
COL_MK <- cbPalette8[4]
COL_EQ <- cbPalette8[2]
COL_1  <- cbPalette15[6]
COL_2  <- cbPalette15[7]
COL_3  <- cbPalette15[8]
COL_5  <- cbPalette15[4]
COL10  <- cbPalette15[5]
COL_C  <- cbPalette15[12]


COL <- c(
  markov =     cbPalette8[4],
  equal  =     cbPalette8[2],
  implied1   = cbPalette15[11],
  implied2   = cbPalette15[10],
  implied3   = cbPalette15[9],
  implied5   = cbPalette15[8],
  implied10  = cbPalette15[7],
  implied20  = cbPalette15[6],
  implied200 = cbPalette15[5],
  impliedC =    cbPalette15[12]
)
LTR <- c(
  markov =     'M',
  equal  =     '=',
  implied1   = '',
  implied2   = '',
  implied3   = '',
  implied5   = 5,
  implied10  = '',
  implied20  = '',
  implied200 = '',
  impliedC =  'I'
)


InitialPlot <- function() {
  TernaryPlot( 'Unresolved', 'Different', 'Same', lab.cex=0.8,
               grid.lty='solid', grid.col=GRID_COL, grid.lines=19,
               col=BG_COL, point='right',
               axis.col=rgb(0.6, 0.6, 0.6),
               padding=0.1, axis.labels = 0:19)
  title(main="\nPartitions", cex.main=0.8)
  
  HorizontalGrid(grid.col='#888888', grid.lines=19)
  partition_distances <- SplitPoints(sq_trees)
  invisible()
}
CLBest <- function (analysis) rowMeans(PointsFromItem(clPartitions[[analysis]][ , 1, ]))
CLBestQ <- function (analysis) rowMeans(clQuartets[[analysis]][c('r2', 'd', 's') , 1, ])

par(mfrow=c(1, 1), mai=rep(0, 4))
InitialPlot()
AddToTernary(text, CLBest('equal'   ),  col=COL_EQ, '=', cex=1.5)
# Save here
AddToTernary(text, CLBest('implied1'),  col=COL_1,   1,  cex=1.5)
AddToTernary(text, CLBest('implied2'),  col=COL_2,   2,  cex=1.5)
AddToTernary(text, CLBest('implied3'),  col=COL_3,   3,  cex=1.5)
AddToTernary(text, CLBest('implied5'),  col=COL_5,   5,  cex=1.5)
AddToTernary(text, CLBest('implied10'), col=COL10,   10, cex=1.5)
AddToTernary(text, CLBest('impliedC'),  col=COL_C , 'I', cex=1.5)
AddToTernary(text, CLBest('markov'  ),  col=COL_MK, 'M', cex=1.5)

JoinTheDots(CLAverageSplits('equal'   ), col=COL_EQ, pch=PCH_XX, cex=1.1)
JoinTheDots(CLAverageSplits('implied1'), col=COL_1,  pch=PCH_XX, cex=1.1)
JoinTheDots(CLAverageSplits('implied2'), col=COL_2,  pch=PCH_XX, cex=1.1)
JoinTheDots(CLAverageSplits('implied3'), col=COL_3,  pch=PCH_XX, cex=1.1)
JoinTheDots(CLAverageSplits('implied5'), col=COL_5,  pch=PCH_XX, cex=1.1)
JoinTheDots(CLAverageSplits('implied10'), col=COL10, pch=PCH_XX, cex=1.1)
JoinTheDots(CLAverageSplits('impliedC'), col=COL_C,  pch=PCH_XX, cex=1.1)
JoinTheDots(CLAverageSplits('markov'  ), col=COL_MK, pch=PCH_XX, cex=1.1)

dev.off()
#################################################
# QUARTETS ######################################
#################################################

dev.new()
par(mfrow=c(1, 1), mai=rep(0, 4))

TernaryQuarts(AverageQuarts)
AddArrows('Increasing Divergence')
rightPoint <- TernaryCoords(0, 0, 1)
otherYs <- vapply(2*(1:8), function (p) TernaryCoords(p, 19-p, 0), double(2))[2, ]
lapply(otherYs, function (y) lines(c(0, rightPoint[1]), c(y, rightPoint[2]),
                                   lty='dashed', col='#00000022'))


par(mai=c(0, 0.15, 0, 0.15))
TernaryQuarts(Func=AverageQuarts, zoom=3.5, padding=0.01)
otherYs <- vapply(14:19, function (p) TernaryCoords(p, 19-p, 0), double(2))[2, ]
lapply(otherYs, function (y) lines(c(0, rightPoint[1]), c(y, rightPoint[2]),
                                   lty='dashed', col='#00000022'))
TernaryText(CLBestQ('implied1'), '1', col=COL_1, cex=2)
TernaryText(CLBestQ('implied10'), '10', col=COL10, cex=2)
TernaryText(CLBestQ('implied5'), '5', col=COL_5, cex=2)
TernaryText(CLBestQ('markov'), 'M', col=COL_MK, cex=2)
TernaryText(CLBestQ('impliedC'), 'I', col=COL_C, cex=2)
TernaryText(CLBestQ('equal'), '=', col=COL_EQ, cex=2)

AddLegend('topright')
#dev.off()


################################################################################
orAnalyses <- c('implied200', 'implied20', 'implied10', 'implied5', 'implied3',
                'implied2','markov', 'equal')

ORQ100 <- function (x) ORAverageQuarts(100, x)
ORQ350 <- function (x) ORAverageQuarts(350, x)
ORQ1000 <- function (x) ORAverageQuarts(1000, x)

ORS100 <- function (x) ORAverageSplits(100, x)
ORS350 <- function (x) ORAverageSplits(350, x)
ORS1000 <- function (x) ORAverageSplits(1000, x)

orNPartitions <- choose(74, 4)
ORQlines <- 12L
ORQlabs <- paste0(round(seq(0, orNPartitions, length.out=ORQlines + 1L) / 1000), 'k')

ORSlines <- 12L
ORSlabs <- seq(0, 72, length.out=13)

################################################################################
# Quartets: 
################################################################################

dev.new()

##################### 100
LTR['implied2'] <- '2'
par(mar=rep(0, 4), mfrow=c(1, 2), mai=rep(0, 4))
TernaryPlot('Unresolved', 'Different', 'Same', lab.cex=0.8,
            col=BG_COL, point='right',
            grid.lines = ORQlines, grid.lty='solid', grid.col=GRID_COL,
            axis.col=rgb(0.6, 0.6, 0.6),
            padding=0.1, axis.labels = ORQlabs)
title(main="\nQuartets (100 characters)", cex.main=0.8)

HorizontalGrid(24)

xx <- lapply(orAnalyses, function (analysis) {
  TernaryLines(ORQ100(analysis), col=COL[analysis], pch=PCH['dot'],
               lty=LTY[analysis], lwd=LWD[analysis])
  TernaryText(ORQ100(analysis)[, 1], LTR[analysis], col=COL[analysis], cex=1.7)
})

par(mai=c(0, 0.15, 0, 0.15))
zoom=2.25
TernaryPlot('Unresolved', 'Different', 'Same', lab.cex=0.8,
            col=BG_COL, point='right',
            grid.lines = ORQlines, grid.lty='solid', grid.col=GRID_COL,
            axis.col=rgb(0.6, 0.6, 0.6),
            padding=0.01, axis.labels = ORQlabs,
            xlim = c(0, sqrt(3/4)/zoom),
            ylim = c(0.5-(1/zoom), 0.5))
HorizontalGrid(24)
xx <- lapply(orAnalyses, function (analysis) {
  TernaryLines(ORQ100(analysis), col=COL[analysis],  pch=PCH['dot'],
               lty=LTY[analysis], lwd=LWD[analysis])
  TernaryText(ORQ100(analysis)[, 1], LTR[analysis], col=COL[analysis], cex=2.2)
})

#################### 350

LTR['implied2'] <- ''
par(mar=rep(0, 4), mfrow=c(1,2), mai=rep(0, 4))
TernaryPlot('Unresolved', 'Different', 'Same', lab.cex=0.8,
            col=BG_COL, point='right',
            grid.lines = ORQlines, grid.lty='solid', grid.col=GRID_COL,
            axis.col=rgb(0.6, 0.6, 0.6),
            padding=0.1, axis.labels = ORQlabs)
title(main="\nQuartets (350 characters)", cex.main=0.8)

HorizontalGrid(24)

xx <- lapply(orAnalyses, function (analysis) {
  TernaryLines(ORQ350(analysis), col=COL[analysis], pch=PCH['dot'],
               lty=LTY[analysis], lwd=LWD[analysis])
  TernaryText(ORQ350(analysis)[, 1], LTR[analysis], col=COL[analysis], cex=1.7)
})

par(mai=c(0, 0.15, 0, 0.15))
zoom=3.5
TernaryPlot('Unresolved', 'Different', 'Same', lab.cex=0.8,
            col=BG_COL, point='right',
            grid.lines = ORQlines, grid.lty='solid', grid.col=GRID_COL,
            axis.col=rgb(0.6, 0.6, 0.6),
            padding=0.01, axis.labels = ORQlabs,
            xlim = c(0, sqrt(3/4)/zoom),
            ylim = c(0.5-(1/zoom), 0.5))
HorizontalGrid(24)
xx <- lapply(orAnalyses, function (analysis) {
  TernaryLines(ORQ350(analysis), col=COL[analysis],  pch=PCH['dot'],
               lty=LTY[analysis], lwd=LWD[analysis])
  TernaryText(ORQ350(analysis)[, 1], LTR[analysis], col=COL[analysis], cex=2.2)
})

##################### 1000
par(mar=rep(0, 4), mfrow=c(1, 2), mai=rep(0, 4))
TernaryPlot('Unresolved', 'Different', 'Same', lab.cex=0.8,
            col=BG_COL, point='right',
            grid.lines = ORQlines, grid.lty='solid', grid.col=GRID_COL,
            axis.col=rgb(0.6, 0.6, 0.6),
            padding=0.1, axis.labels = ORQlabs)
title(main="\nQuartets (1000 characters)", cex.main=0.8)

HorizontalGrid(24)

xx <- lapply(orAnalyses, function (analysis) {
  TernaryLines(ORQ1000(analysis), col=COL[analysis], pch=PCH['dot'],
               lty=LTY[analysis], lwd=LWD[analysis])
  TernaryText(ORQ1000(analysis)[, 1], LTR[analysis], col=COL[analysis], cex=1.7)
})


par(mai=c(0, 0.15, 0, 0.15))
zoom=6.5
TernaryPlot('Unresolved', 'Different', 'Same', lab.cex=0.8,
            col=BG_COL, point='right',
            grid.lines = ORQlines, grid.lty='solid', grid.col=GRID_COL,
            axis.col=rgb(0.6, 0.6, 0.6),
            padding=0.01, axis.labels = ORQlabs,
            xlim = c(0, sqrt(3/4)/zoom),
            ylim = c(0.5-(1/zoom), 0.5))
HorizontalGrid(24)
xx <- lapply(orAnalyses, function (analysis) {
  TernaryLines(ORQ1000(analysis), col=COL[analysis],  pch=PCH['dot'],
               lty=LTY[analysis], lwd=LWD[analysis])
  TernaryText(ORQ1000(analysis)[, 1], LTR[analysis], col=COL[analysis], cex=2.2)
})

################################################################################
# Partitions:
################################################################################

orAnalyses <- c('implied200', 'implied20', 'implied10', 'implied5', 'implied3',
                'implied2','markov', 'equal', 'impliedC')
dev.new()
par(mar=rep(0, 4), mfrow=c(1, 1), mai=rep(0, 4))

##################### 100
LTR['implied2'] <- '2'
TernaryPlot('Unresolved', 'Different', 'Same', lab.cex=0.8,
            col=BG_COL, point='right',
            grid.lines = ORSlines, grid.lty='solid', grid.col=GRID_COL,
            axis.col=rgb(0.6, 0.6, 0.6),
            padding=0.1, axis.labels = ORSlabs)
title(main="\nPartitions", cex.main=0.8)
HorizontalGrid(24)

##################### 100
xx <- lapply(orAnalyses, function (analysis) {
  TernaryLines(ORS100(analysis), col=COL[analysis], pch=PCH['dot'],
               lty=LTY[analysis], lwd=LWD[analysis])
  TernaryText(ORS100(analysis)[, 1], LTR[analysis], col=COL[analysis], cex=1.7)
})

#################### 350
xx <- lapply(orAnalyses, function (analysis) {
  TernaryLines(ORS350(analysis), col=COL[analysis], pch=PCH['dot'],
               lty=LTY[analysis], lwd=LWD[analysis])
  TernaryText(ORS350(analysis)[, 1], LTR[analysis], col=COL[analysis], cex=1.7)
})

##################### 1000
xx <- lapply(orAnalyses, function (analysis) {
  TernaryLines(ORS1000(analysis), col=COL[analysis], pch=PCH['dot'],
               lty=LTY[analysis], lwd=LWD[analysis])
  TernaryText(ORS1000(analysis)[, 1], LTR[analysis], col=COL[analysis], cex=1.7)
})
