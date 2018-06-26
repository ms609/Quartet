
COL_MK <- cbPalette8[4]
COL_EQ <- cbPalette8[2]
COL_1  <- cbPalette15[6]
COL_2  <- cbPalette15[7]
COL_3  <- cbPalette15[8]
COL_5  <- cbPalette15[4]
COL10  <- cbPalette15[5]
COL_C  <- cbPalette15[12]

InitialPlot <- function() {
  TernaryPlot( 'Unresolved', 'Different', 'Same', lab.cex=0.8,
               grid.lty='solid', grid.col=GRID_COL, grid.lines=19,
               col=BG_COL, point='right',
               axis.col=rgb(0.6, 0.6, 0.6),
               padding=0.1, axis.labels = 0:19)
  title(main="\nPartitions", cex.main=0.8)
  
  HorizontalGrid(grid.col='#888888', grid.lines=19)
  partition_distances <- SplitsPoints(sq_trees)
  invisible()
}
CLBest <- function (analysis) rowMeans(PointsFromItem(clPartitions[[analysis]][ , 1, ]))
CLBestQ <- function (analysis) rowMeans(clQuartets[[analysis]][c('r2', 'd', 's') , 1, ])

#setEPS()
#postscript("c:/Box/Presentations/images/PrecisionAccuracy/CL_best.eps")
#win.metafile("c:/Box/Presentations/images/PrecisionAccuracy/CL_best.wmf")
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

JoinTheDots(CLAverageSplits('implied1'), col=COL_1,  pch=PCH_XX, cex=1.1)
JoinTheDots(CLAverageSplits('equal'   ), col=COL_EQ, pch=PCH_XX, cex=1.1)
JoinTheDots(CLAverageSplits('implied2'), col=COL_2,  pch=PCH_XX, cex=1.1)
JoinTheDots(CLAverageSplits('implied10'), col=COL10, pch=PCH_XX, cex=1.1)
JoinTheDots(CLAverageSplits('implied3'), col=COL_3,  pch=PCH_XX, cex=1.1)
JoinTheDots(CLAverageSplits('implied5'), col=COL_5,  pch=PCH_XX, cex=1.1)
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
TernaryText(CLBestQ('equal'), '=', col=COL_EQ, cex=2)

AddLegend('topright')
#dev.off()
