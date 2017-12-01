require('ape')
require('SlowQuartet')
data('sq_trees')
n_tip <- 11
ref_tree <- sq_trees[[1]]
tip_colours <- Ternary::cbPalette15[-c(4, 7)] # Rm Tritanopia duplicates of 13 and 3
palette4 <- Ternary::cbPalette8[1:4]

rf_max <- (n_tip - 3) * 2

colplot <- function (tr, title=NULL, bold=NULL, ...) {
  tr$edge.length <- rep(1, dim(tr$edge)[1])
  font <- rep(1, length(tr$tip.label))
  if (!is.null(bold)) font[tr$tip.label %in% bold] <- 4
  plot(tr, tip.col=tip_colours[as.integer(tr$tip.label)], main=title, cex.main=0.8, font=font, y.lim=c(-3.5, n_tip), ...)
}

rfplot <- function (tr, title=NULL, highlight=NULL, ref=ref_tree, ...) {
  tree_dist <- phangorn::treedist(tr, ref)
  tree_pair <- lapply(list(tr, ref), ape::root, outgroup='1', resolve.root=FALSE)
  class(tree_pair) <- 'multiPhylo'
  topo_dist <- as.matrix(ape::dist.topo(tree_pair, method='PH85'))[2]
  
  colplot(tr, title, highlight, cex=0.8, ...)
  text_x <- par('usr')[2] * 0.5
  
  text(text_x, -0.5, 'Quartet:', cex=0.8, pos=2)
  text(text_x, -1.5, "RF:", cex=0.8, pos=2)
  text(text_x, -2.5, "Path:", cex=0.8, pos=2)
  text(text_x, -3.5, "SPR:", cex=0.8, pos=2)
  text(text_x, -0.5, paste0(MatchingQuartets(list(tr, ref))['d', 2], '/', choose(11,4)), cex=0.8, pos=4)
  text(text_x, -1.5, paste0(topo_dist, '/', rf_max), cex=0.8, pos=4)
  text(text_x, -2.5, paste0(signif(tree_dist[2], 3)), cex=0.8, pos=4)
  text(text_x, -3.5, paste0(phangorn::sprdist(tr, ref)[1]), cex=0.8, pos=4)
}

polyplot <- function (tr, title, highlight, ...) {
  tree_pair <- lapply(list(ref_tree, tr), ape::unroot)
  class(tree_pair) <- 'multiPhylo'
  partitions <- MatchingSplits(tree_pair)[, 2]
  
  colplot(tr, title, highlight, ...)
  x_mid <- par('usr')[2] * 0.62
  
  text(x_mid, -0.5, cex=0.8, pos=2, "Quartets contradicted")
  text(x_mid, -1.5, cex=0.8, pos=2, "Quartets unresolved")
  text(x_mid, -2.5, cex=0.8, pos=2, "Partitions contradicted")
  text(x_mid, -3.5, cex=0.8, pos=2, "Partitions unresolved")
  
  text(x_mid, -0.5, cex=0.8, pos=4, 
       paste0(MatchingQuartets(tree_pair)['d', 2], '/', choose(11,4)))
  text(x_mid, -1.5, cex=0.8, pos=4, 
       paste0(sum(MatchingQuartets(tree_pair)[c('r1','r2','u'), 2]), '/', choose(11,4)))
  # The below assumes that ref is bifurcated, so if a partition's in cf not ref it must contradict.
  text(x_mid, -2.5, cex=0.8, pos=4, paste0(partitions['cf_not_ref'], '/', rf_max / 2))
  text(x_mid, -3.5, cex=0.8, pos=4, paste0(partitions['ref_not_cf'] - partitions['cf_not_ref'], '/', rf_max / 2)) 
}
par(mfrow=c(1, 2), mar=rep(0.3, 4))
collapse_perfect <- c('ref_tree', 'collapse_one', 'collapse_some')
collapse_move1   <- c('move_one_mid', 'm1mid_col1', 'm1mid_colsome')
collapse_move2   <- c('move_two_mid', 'm2mid_col1', 'm2mid_colsome')


par(mfrow=c(1, 3), mar=rep(0.3, 4))
data(clPartitions)
data(clQuartets)

PCH_MK <- 1   # circle
PCH_EQ <- 61  #'='
PCH_XX <- 183 #'.'
PCH_IW <- 2   #triup
PCH_IC <- 17  #triupfilled

COL_MK <- paste0(cbPalette8[4],   '99')
COL_EQ <- paste0(cbPalette8[2],   '99')
COL_1  <- paste0(cbPalette15[6],  '99')
COL_2  <- paste0(cbPalette15[7],  '99')
COL_3  <- paste0(cbPalette15[8],  '99')
COL_5  <- paste0(cbPalette15[4],  '99')
COL10  <- paste0(cbPalette15[5],  '99')
COL_C  <- paste0(cbPalette15[12], '99')

GRID_COL <- rgb(0.92, 0.92, 0.92)
BG_COL   <- rgb(0.985, 0.985, 0.992)

Quartet2Ternary <- function (item) clQuartets[[item]][c('s', 'd', 'r2'), , TREE]
  
Split2Ternary <- function (item) {
  itemData <- clPartitions[[item]][, , TREE]
  rbind(itemData['cf_and_ref', ],
        itemData['cf_not_ref', ],
        itemData['ref', ] - itemData['cf', ])
}

TernaryQuarts<-function(Func=Quartet2Ternary, zoom=1, padding=0.1) {
  
  xLim <- c(0, sqrt(3/4)/zoom)
  yLim <- c(0.5-(1/zoom), 0.5)
  lab <- if (zoom == 1) c('  Same', ' Different', '\nUnresolved') else rep('', 3)
  
  TernaryPlot(lab[1], lab[2], lab[3], lab.cex=0.8, lab.font=2,
              col=BG_COL,
              grid.lty='solid', grid.col=GRID_COL, grid.lines=19,
              axis.labels = 
                if(zoom==1) round(seq(0, choose(22, 4), length.out=20), 0)
                else FALSE,
              axis.col=rgb(0.6, 0.6, 0.6),
              padding=padding, xlim=xLim, ylim=yLim)
  HorizontalGrid(19)
  AddToTernary(lines, list(c(1/3, 2/3, 0), c(0, 0, 1)), lty='dotted', col=cbPalette8[8], lwd=2)
  
  JoinTheDots(Func('implied10'), col=COL10, pch=PCH_XX, cex=1.1)
  JoinTheDots(Func('implied5'), col=COL_5,  pch=PCH_IW, cex=1.1)
  JoinTheDots(Func('implied3'), col=COL_3,  pch=PCH_XX, cex=1.1)
  JoinTheDots(Func('implied2'), col=COL_2,  pch=PCH_XX, cex=1.1)
  JoinTheDots(Func('implied1'), col=COL_1,  pch=PCH_IW, cex=1.1)
  JoinTheDots(Func('impliedC'), col=COL_C,  pch=PCH_IC, cex=1.1)
  JoinTheDots(Func('equal'   ), col=COL_EQ, pch=PCH_EQ, cex=1.1)
  JoinTheDots(Func('markov'  ), col=COL_MK, pch=PCH_MK, cex=1.1)
}


AddArrows <- function (quality) {
  arrows(sqrt(3/4) * 0.5, 0.5, sqrt(3/4) * 0.8, 0.5, length=0.08)
  text  (sqrt(3/4) * 0.65, 0.5, pos=3, 'Decreasing resolution', cex=0.8)
  arrows(sqrt(3/4) * 0.98, 0.40, sqrt(3/4) * 0.98, 0.20, length=0.08)
  text  (sqrt(3/4) * 1.01, 0.30, pos=3, quality, cex=0.8, srt=270)
}

AddLegend <- function(pos='bottomright')
  legend(pos, cex=0.8, bty='n',
         lty=1,
         pch=c(PCH_MK, PCH_EQ, PCH_XX, PCH_IW, PCH_XX, PCH_XX, PCH_IW, PCH_IC), pt.cex=1.1,
         col=c(COL_MK, COL_EQ, COL10, COL_5, COL_3, COL_2, COL_1, COL_C),
         legend=c('Markov', 'Equal weights', paste0('Implied, k=', c(10, 5, 3, 2, 1, '2..10')))
  )

AverageSplits <- function (item) {
  itemData <- apply(clPartitions[[item]][, , ], 2, rowMeans)
  rbind(itemData['cf_and_ref', ],
        itemData['cf_not_ref', ],
        itemData['ref', ] - itemData['cf', ])
}





COL_WIDTH = 7.6/2.54


################################################################################

# Figures should be drawn to publication quality and to fit into a single column
# width (7 cm) wherever possible. Please ensure that axes, tick marks, symbols 
# and labels are large enough to allow reduction to a final size of c. 8 point.
pdf(file="inst/Figure_1.pdf", width=COL_WIDTH, paper='a4', title="Smith Figure 1",  pointsize=8)
par(mfrow=c(2, 1), mai=rep(0, 4))
AverageQuarts <- function (item) apply(clQuartets[[item]][c('s', 'd', 'r2'), , ], 2, rowMeans)
TernaryQuarts(AverageQuarts)
AddArrows('Increasing Divergence')
par(mai=c(0, 0.15, 0, 0.15))
TernaryQuarts(Func=AverageQuarts, zoom=3.5, padding=0.01)
AddLegend('topright')
dev.off()



################################################################################

################################################################################


pdf(file="inst/Figure_2.pdf", width=COL_WIDTH, paper='a4', title="Smith Figure 2", pointsize=8)
par(mfrow=c(1,1), mai=rep(0, 4))


TernaryPlot('Same', 'Different', 'Unresolved', lab.cex=0.8,
            grid.lty='solid', grid.col=GRID_COL, grid.lines=19,
            col=BG_COL, 
            axis.col=rgb(0.6, 0.6, 0.6),
            padding=0.1, axis.labels = 0:19)
title(main="\nPartitions", cex.main=0.8)

HorizontalGrid(grid.col='#888888', grid.lines=19)
partition_distances <- SplitsPoints(sq_trees)

JoinTheDots(AverageSplits('implied10'), col=COL10, pch=PCH_XX, cex=1.1)
JoinTheDots(AverageSplits('implied5'), col=COL_5, pch=PCH_IW, cex=1.1)
JoinTheDots(AverageSplits('implied3'), col=COL_3, pch=PCH_XX, cex=1.1)
JoinTheDots(AverageSplits('implied2'), col=COL_2, pch=PCH_XX, cex=1.1)
JoinTheDots(AverageSplits('implied1'), col=COL_1, pch=PCH_IW, cex=1.1)
JoinTheDots(AverageSplits('markov'  ), col=COL_MK, pch=PCH_MK, cex=1.1)
JoinTheDots(AverageSplits('impliedC'), col=COL_C, pch=PCH_IC, cex=1.1)
JoinTheDots(AverageSplits('equal'   ), col=COL_EQ, pch=PCH_EQ, cex=1.1)

AddArrows("Increasing RF distance")
AddLegend()
dev.off()


################################################################################

################################################################################

pdf(file="inst/Figure_3.pdf", width=COL_WIDTH, paper='default', title="Smith Figure 3",  pointsize=8)
par(mar=rep(0, 4), mfrow=c(1,1), mai=rep(0, 4))
TernaryPlot('Same', 'Different', 'Unresolved', lab.cex=0.8,
            col=BG_COL,
            grid.lines = 19, grid.lty='solid', grid.col=GRID_COL,
            axis.col=rgb(0.6, 0.6, 0.6),
            padding=0.1, axis.labels = 0:19)
title(main="\nPartitions", cex.main=0.8)

HorizontalGrid()
partition_distances <- SplitsPoints(sq_trees)

TernaryLines(AverageSplits('implied10'), col=COL10, pch=PCH_XX)
TernaryLines(AverageSplits('implied5' ), col=COL_5,  pch=PCH_XX)
TernaryLines(AverageSplits('implied3' ), col=COL_3,  pch=PCH_XX)
TernaryLines(AverageSplits('implied2' ), col=COL_2,  pch=PCH_XX)
TernaryLines(AverageSplits('implied1' ), col=COL_1,  pch=PCH_XX)
TernaryLines(AverageSplits('impliedC' ), col=COL_C,  pch=PCH_XX)
TernaryLines(AverageSplits('markov'   ), col=COL_MK, pch=PCH_XX)
TernaryLines(AverageSplits('equal'    ), col=COL_EQ, pch=PCH_XX)

PCH_EQ = 0

TernaryPoints(AverageSplits('implied10')[, 1], col=COL10,  pch=PCH_IW, cex=1.1)
TernaryPoints(AverageSplits('implied5' )[, 1], col=COL_5,  pch=PCH_IW, cex=1.1)
TernaryPoints(AverageSplits('implied3' )[, 1], col=COL_3,  pch=PCH_IW, cex=1.1)
TernaryPoints(AverageSplits('implied2' )[, 1], col=COL_2,  pch=PCH_IW, cex=1.1)
TernaryPoints(AverageSplits('implied1' )[, 1], col=COL_1,  pch=PCH_IW, cex=1.1)
TernaryPoints(AverageSplits('markov'   )[, 1], col=COL_MK, pch=PCH_MK, cex=1.1)
TernaryPoints(AverageSplits('impliedC' )[, 1], col=COL_C,  pch=PCH_IC, cex=1.1)
TernaryPoints(AverageSplits('equal'    )[, 1], col=COL_EQ, pch=PCH_EQ, cex=1.1)

equal_point <- AverageSplits('equal'    )[, 1]
equal_differents <- equal_point[2]
TernaryLines(list(c(0, equal_differents, 20-equal_differents),
                  c(20-equal_differents, equal_differents, 0)), 
             col=COL_EQ, lty='dashed', lwd=1.5)
equal_coords <- TernaryCoords(equal_point)
lines(c(0, sqrt(3/4) * (0.5 + equal_coords[2])), rep(equal_coords[2], 2), 
      col=COL_EQ, lty='dotted', lwd=1.75)
lines(rep(equal_coords[1], 2), c(-1, +1) * 0.5 * (1 - equal_coords[1] / sqrt(3/4)),
      col=COL_EQ, lty='dotdash', lwd=1.5)

arrow_tips <- matrix(c(TernaryCoords(3, 19-6, 3), TernaryCoords(7, 19-14, 7), TernaryCoords(19-(7+3), 7, 3)), 2, 3)
arrows(arrow_tips[1, 1], arrow_tips[2, 1], arrow_tips[1, 2], arrow_tips[2, 2], length=0.08, col='#666666')
arrows(arrow_tips[1, 1], arrow_tips[2, 1], arrow_tips[1, 3], arrow_tips[2, 3], length=0.08, col='#666666')
text(mean(arrow_tips[1, 1:2]) + 0.01, mean(arrow_tips[2, 1:2]), "Increasing quality\n(Congreve & Lamsdell)", cex=0.8, srt=58, pos=1, col='#666666')
text(mean(arrow_tips[1, c(1, 3)]) - 0.02, mean(arrow_tips[2, c(1, 3)]), "Increasing quality\n(Quartet divergence)", cex=0.8, srt=90, pos=3, col='#666666')
AddLegend()
legend('bottom', bty='n', cex=0.8, lwd=1.2, col=COL_EQ, 
       lty=c('dotted', 'dotdash', 'dashed'), legend=c('Equal quality', 'Equal precision', 'Equal incorrect nodes'))
dev.off()
