require('ape')
require('SlowQuartet')
library('Ternary')
data('sq_trees')
data('clQuartets')
data('orQuartets')
data('clPartitions')
data('orPartitions')
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

PCH <- c(
  markov = 1,
  equal  = 61, #'='
  dot = 183, #'.'
  implied1   = 2,   #triup
  implied2   = 2,   #triup
  implied3   = 2,   #triup
  implied5   = 2,   #triup
  implied10  = 2,   #triup
  implied20  = 2,   #triup
  implied200 = 2,   #triup
  impliedC = 17  #triupfilled
)

LTY <- c(
  markov = 2,
  equal  = 1, 
  implied1   = 1,   #triup
  implied2   = 1,   #triup
  implied3   = 1,   #triup
  implied5   = 1,   #triup
  implied10  = 1,   #triup
  implied20  = 1,   #triup
  implied200 = 1,   #triup
  impliedC = 1  #triupfilled
)
LWD <- LTY

COL <- c(
  markov     = paste0(cbPalette8[4], '99'),
  equal      = paste0(cbPalette8[8], '99'),
  implied1   = paste0(cbPalette8[6], '42'),
  implied2   = paste0(cbPalette8[6], '42'),
  implied3   = paste0(cbPalette8[6], '42'),
  implied5   = paste0(cbPalette8[6], '42'),
  implied10  = paste0(cbPalette8[6], '99'),
  implied20  = paste0(cbPalette8[6], '42'),
  implied200 = paste0(cbPalette8[6], '42'),
  impliedC   = paste0(cbPalette8[2], '99')
)

PCH_MK <- PCH['markov']
PCH_EQ <- PCH['equal']
PCH_XX <- PCH['dot']
PCH_IW <- 3 #[plus] ##2   #triup
PCH_IC <- PCH['impliedC']

COL_MK <- COL['markov']
COL_EQ <- COL['equal']
COL_1  <- paste0(cbPalette8[6], '42')
COL_2  <- paste0(cbPalette8[6], '42')
COL_3  <- paste0(cbPalette8[6], '42')
COL_5  <- paste0(cbPalette8[6], '99')
COL10  <- paste0(cbPalette8[6], '42')
COL_C  <- COL['impliedC']

GRID_COL <- rgb(0.92, 0.92, 0.92)
BG_COL   <- rgb(0.985, 0.985, 0.992)

COL_WIDTH <- 5.6
PAGE_WIDTH <- 11

Quartet2Ternary <- function (item) clQuartets[[item]][c('r2', 'd', 's'), , TREE]
  
Split2Ternary <- function (item) {
  itemData <- clPartitions[[item]][, , TREE]
  rbind(itemData['ref', ] - itemData['cf', ],
        itemData['cf_not_ref', ],
        itemData['cf_and_ref', ])
}

TernaryQuarts <- function(Func=Quartet2Ternary, zoom=1, padding=0.1) {
  xLim <- c(0, 1 / zoom) - 0.003
  yLim <- c(0.5-(1/zoom), 0.5)
  tip <- if (FALSE && zoom == 1) c('\n\nUnresolved', "\n\nDifferent", '      Same') else rep('', 3)
  lab <- if (zoom == 1) c("Unresolved quartets", 
                          "Different quartets",
                          "Identical quartets") else rep('', 3)
  
  TernaryPlot(atip=tip[1], btip=tip[2], ctip=tip[3],
              alab=lab[1], blab=lab[2], clab=lab[3], 
              lab.cex=0.8, lab.font=2, lab.offset=0.13,
              point='right', isometric = TRUE,
              col=BG_COL,
              grid.lty='solid', grid.col=GRID_COL, grid.lines=19,
              grid.minor.lines = 0,
              axis.labels = 
                if(zoom==1) round(seq(0, choose(22, 4), length.out=20), 0)
                else if (zoom == 3.5) round(seq(0, choose(22, 4), length.out=20), 0)
                else FALSE,
              axis.col=rgb(0.6, 0.6, 0.6),
              ticks.length = if (zoom == 1) 0.025 else 0.009,
              #axis.labels.col = if (zoom == 1) GRID_COL else rgb(0.6, 0.6, 0.6),
              padding=padding, xlim=xLim, ylim=yLim)
  HorizontalGrid(19)
  AddToTernary(lines, list(c(0, 2/3, 1/3), c(1, 0, 0)), lty='dotted', col=cbPalette8[8], lwd=2)
  
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

AddLegend2 <- function(analyses, pos='bottomright')
  legend(pos, cex=0.8, bty='n',
         lty=1,
         pch=PCH[analyses], pt.cex=1.1,
         col=COL[analyses],
         legend=c('Markov', 'Equal weights', paste0('Implied, k=', rev(c(2, 3, 5, 10, 20, 200))))
  )

Panel <- function (panel) legend('topleft', paste0('(', panel, ')'), bty='n', text.font=2)

PointsFromItem <- function(itemData) {
  rbind(itemData['ref', ] - itemData['cf', ],
        itemData['cf_not_ref', ],
        itemData['cf_and_ref', ])
}

AverageSplits <- function (dataset) {
  PointsFromItem(apply(dataset[, , ], 2, rowMeans, na.rm=TRUE))
}

CLAverageSplits <- function (item) {
  AverageSplits(clPartitions[[item]][, , ])
}

ORAverageSplits <- function (nchar, item) {
  AverageSplits(orPartitions[[as.character(nchar)]][[item]][, , ])
}


AverageQuarts <- function (item) apply(clQuartets[[item]][c('r2', 'd', 's'), , ], 2, rowMeans, na.rm=TRUE)
ORAverageQuarts <- function (nchar, item) apply(orQuartets[[as.character(nchar)]][[item]][c('r2', 'd', 's'), , ], 2, rowMeans, na.rm=TRUE)





################################################################################

# Figures should be drawn to publication quality and to fit into a single column
# width (7 cm) wherever possible. Please ensure that axes, tick marks, symbols 
# and labels are large enough to allow reduction to a final size of c. 8 point.
dev.new()
#png(filename="Figure_1.png", units='in', res=72, width=COL_WIDTH, height=COL_WIDTH * 2, family='Gill Sans MT')
#pdf(file="inst/Figure_1.pdf", width=COL_WIDTH, paper='a4', title="Smith Figure 1",  pointsize=8)
cairo_pdf(filename="inst/Figure_1.pdf", width=PAGE_WIDTH, height=COL_WIDTH * 2, family='Gill sans')
#svg(filename="inst/Figure_1.svg",  width=PAGE_WIDTH, height=COL_WIDTH * 2, family='Gill sans')
par(mfrow=c(2, 2), mai=rep(0, 4))

TernaryQuarts(AverageQuarts)
#title(main="\nQuartets", cex.main=0.8)
AddArrows('Increasing quartet dissimilarity')
rect(xleft=-0.01, ybottom=0.19, xright=0.278, ytop=0.52, border='#00000088', lty='dashed')
text(x=0.30, y=0.54, labels='Panel (b)', cex=0.8, pos=2)
rightPoint <- TernaryCoords(1, 0, 0)
otherYs <- vapply(2*(1:18), function (p) TernaryCoords(p, 19 - p, 0), double(2))[2, ]
lapply(otherYs, function (y) lines(c(0, rightPoint[1]), c(y, rightPoint[2]),
                                   lty='dashed', col='#00000022'))

legend('bottomright', bty='n', cex=0.8,
       lty=c('dotted', 'dashed', 'dotted'),
       lwd=c(1, 1, 2), col=c('grey', '#00000044', cbPalette8[8]),
       legend=c('Equal divergence', 'Equal accuracy', 'Similarity of random tree'))

Panel('a')

par(mai=c(0, 0.15, 0, 0.15))
TernaryQuarts(Func=AverageQuarts, zoom=3.5, padding=0.01)
text(-0.007, 0.35, 'Identical quartets', cex=0.8, srt=90)
text(0.18, 0.432, 'Unresolved quartets', cex=0.8, srt=330)
otherYs <- vapply(2 * (14:18), function (p) TernaryCoords(p, 19 - p, 0), double(2))[2, ]
lapply(otherYs, function (y) lines(c(0, rightPoint[1]), c(y, rightPoint[2]),
                                   lty='dashed', col='#00000022'))

Panel('b')
AddLegend('topright')
# Caption: Quartet distances for Congreve & Lamsdell


################################################################################

################################################################################


#dev.new()
#png(filename="Figure_2.png", units='in', res=72, width=COL_WIDTH, height=COL_WIDTH)
#pdf(file="inst/Figure_2.pdf", width=COL_WIDTH, paper='a4', title="Smith Figure 2", pointsize=8)
#cairo_pdf(filename="inst/Figure_2.pdf", width=COL_WIDTH, height=COL_WIDTH, family='Gill sans')
#par(mfrow=c(1, 1), mai=rep(0, 4))

TernaryPlot(NULL, NULL, NULL, #'Unresolved', 'Different', 'Same', 
            alab="Unresolved partitions",
            blab="Different partitions",
            clab="Identical partitions",
            lab.cex=0.8, lab.offset=0.12,
            grid.lty='solid', grid.col=GRID_COL, grid.lines=19,
            grid.minor.lines = 0,
            col=BG_COL, point='right',
            axis.col=rgb(0.6, 0.6, 0.6),
            padding=0.1, axis.labels = 0:19)
#title(main="\nPartitions", cex.main=0.8)

HorizontalGrid(grid.col='#888888', grid.lines=19)
partition_distances <- SplitsPoints(sq_trees)

JoinTheDots(CLAverageSplits('implied10'), col=COL10, pch=PCH_XX, cex=1.1)
JoinTheDots(CLAverageSplits('implied5'), col=COL_5, pch=PCH_IW, cex=1.1)
JoinTheDots(CLAverageSplits('implied3'), col=COL_3, pch=PCH_XX, cex=1.1)
JoinTheDots(CLAverageSplits('implied2'), col=COL_2, pch=PCH_XX, cex=1.1)
JoinTheDots(CLAverageSplits('implied1'), col=COL_1, pch=PCH_IW, cex=1.1)
JoinTheDots(CLAverageSplits('markov'  ), col=COL_MK, pch=PCH_MK, cex=1.1)
JoinTheDots(CLAverageSplits('impliedC'), col=COL_C, pch=PCH_IC, cex=1.1)
JoinTheDots(CLAverageSplits('equal'   ), col=COL_EQ, pch=PCH_EQ, cex=1.1)

AddArrows("Increasing symmetric difference")
#AddLegend()
Panel('c')

#dev.off()


################################################################################

################################################################################

#dev.new()
#png(file="Figure_4.png", units='in', res=72, width=COL_WIDTH, height=COL_WIDTH)#), pointsize=8)
#pdf(file="inst/Figure_4.pdf", width=COL_WIDTH, paper='default', title="Smith Figure 4",  pointsize=8)
#cairo_pdf(file="inst/Figure_4.pdf", width=COL_WIDTH, height=COL_WIDTH)#), pointsize=8)
#par(mar=rep(0, 4), mfrow=c(1,1), mai=rep(0, 4))
TernaryPlot(NULL, NULL, NULL, #'Unresolved', 'Different', 'Same', 
            alab="Unresolved partitions",
            blab="Different partitions",
            clab="Identical partitions",
            #alab=expression("Unresolved partitions" %->% ''),
            #blab=expression("" %<-% "Different partitions"),
            #clab=expression("Identical partitions" %->% ""),
            lab.cex=0.8, lab.offset=0.12,
            col=BG_COL, point='right',
            grid.lines = 19, grid.lty='solid', grid.col=GRID_COL,
            grid.minor.lines = 0,
            axis.col=rgb(0.6, 0.6, 0.6),
            padding=0.1, axis.labels = 0:19)

HorizontalGrid(19)
partition_distances <- SplitsPoints(sq_trees)

TernaryLines(CLAverageSplits('implied10'), col=COL10,  pch=PCH_XX)
TernaryLines(CLAverageSplits('implied5' ), col=COL_5,  pch=PCH_XX)
TernaryLines(CLAverageSplits('implied3' ), col=COL_3,  pch=PCH_XX)
TernaryLines(CLAverageSplits('implied2' ), col=COL_2,  pch=PCH_XX)
TernaryLines(CLAverageSplits('implied1' ), col=COL_1,  pch=PCH_XX)
TernaryLines(CLAverageSplits('impliedC' ), col=COL_C,  pch=PCH_XX)
TernaryLines(CLAverageSplits('markov'   ), col=COL_MK, pch=PCH_XX)
TernaryLines(CLAverageSplits('equal'    ), col=COL_EQ, pch=PCH_XX)

TernaryPoints(CLAverageSplits('implied10')[, 1], col=COL10,  pch=PCH_IW, cex=1.1)
TernaryPoints(CLAverageSplits('implied5' )[, 1], col=COL_5,  pch=PCH_IW, cex=1.1)
TernaryPoints(CLAverageSplits('implied3' )[, 1], col=COL_3,  pch=PCH_IW, cex=1.1)
TernaryPoints(CLAverageSplits('implied2' )[, 1], col=COL_2,  pch=PCH_IW, cex=1.1)
TernaryPoints(CLAverageSplits('implied1' )[, 1], col=COL_1,  pch=PCH_IW, cex=1.1)
TernaryPoints(CLAverageSplits('markov'   )[, 1], col=COL_MK, pch=PCH_MK, cex=1.1)
TernaryPoints(CLAverageSplits('impliedC' )[, 1], col=COL_C,  pch=PCH_IC, cex=1.1)
TernaryPoints(CLAverageSplits('equal'    )[, 1], col=COL_EQ, pch=0, cex=1.1)


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
text(mean(arrow_tips[1, 1:2]) + 0.01, mean(arrow_tips[2, 1:2]), "Increasing quality\n(Congreve & Lamsdell)", cex=0.8, srt=58, pos=1, col='#666666')
text(mean(arrow_tips[1, c(1, 3)]) - 0.02, mean(arrow_tips[2, c(1, 3)]), "Increasing quality\n(Divergence)", cex=0.8, srt=90, pos=3, col='#666666')
#AddLegend()
legend('bottomright', bty='n', cex=0.8, lwd=1.2, col=COL_EQ, 
       lty=c('dotted', 'dotdash', 'dashed', 'longdash'), 
       legend=c('Equally informative', 'Equal precision', 'Equal accuracy', 'Equal incorrect nodes'))
Panel('d')

dev.off()




##################################################################################
#                                                                                #
#                                 FIGURE 2 (née 3)                               #
#                                                                                #
##################################################################################

totalQuarts <- orQuartets[['350']][['equal']][1, 1, 1] # = 37 * 73 * 9 * 50
GRID_LINES <- 15
AXIS_LABELS <- c(0, paste0(round(1:GRID_LINES * totalQuarts / GRID_LINES / 1000), 'k'))

Fig2Ternary <- function(title.text, ORFunc) {
  TernaryPlot(NULL, NULL, NULL, #'Unresolved', 'Different', 'Same', 
            alab="Unresolved quartets",
            blab="Different quartets",
            clab="Identical quartets",
            lab.cex=0.8, lab.offset=0.14,
            col=BG_COL, point='right',
            grid.lines = GRID_LINES, grid.lty='solid', grid.col=GRID_COL,
            grid.minor.lines = 0,
            axis.col=rgb(0.6, 0.6, 0.6), axis.labels.col = 'black',
            padding=0.1, axis.labels = AXIS_LABELS)
  HorizontalGrid(GRID_LINES)
  title(main=title.text, cex.main=0.8)
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
              xlim = c(0, sqrt(3/4)/zoom),
              ylim = c(0.5-(1/zoom), 0.5))
  HorizontalGrid(GRID_LINES)
  lapply(orAnalyses, function (analysis) {
    TernaryLines(ORFunc(analysis), col=COL[analysis],  pch=PCH['dot'],
                 lty=LTY[analysis], lwd=LWD[analysis])
    TernaryPoints(ORFunc(analysis)[, 1], col=COL[analysis],
                  pch=PCH[analysis], cex=1.1)
  })
}

#dev.new()
#png(file="Figure_2.png", units='in', res=72, width=COL_WIDTH, height=COL_WIDTH)#), pointsize=8)
cairo_pdf(filename="inst/Figure_2.pdf", width=PAGE_WIDTH, height=7.2, family='Gill sans')#), pointsize=8)
#svg(filename="inst/Figure_2.svg", width=PAGE_WIDTH, height=7.2, family='Gill sans')#), pointsize=8)
# Use Inkscape to generate EPS from SVG.  R creates bitmap EPS due to semitrans.


par(mar=rep(0, 4), mfrow=c(2, 3), mai=rep(0, 4))
Fig2Ternary("\n100 characters", ORQ100)
Fig2Ternary("\n350 characters", ORQ350)
Fig2Ternary("\n1000 characters", ORQ1000)
AddLegend2(orAnalyses[c(7, 8, 1:6)])

par(mai=c(0, 0.15, 0, 0.15))

Fig2Zoom(2.25, ORQ100)
Fig2Zoom(2.25, ORQ350)
Fig2Zoom(6.5, ORQ1000)

dev.off()


################################################################################
#                                                                              #
#                                    Old code                                  #
#                                                                              #
################################################################################


################################################################################
# 72 partition; 74 tips.
################################################################################
orAnalyses <- c('implied200', 'implied20', 'implied10', 'implied5', 'implied3',
                'implied2','markov', 'equal')

orNPartitions <- choose(74, 4)
ORQlines <- 12L
ORQlabs <- paste0(round(seq(0, orNPartitions, length.out=ORQlines + 1L) / 1000), 'k')

orLines <- 12L
orLabs <- seq(0, 72, length.out=13)


ORQ100 <- function (x) ORAverageQuarts(100, x)
ORQ350 <- function (x) ORAverageQuarts(350, x)
ORQ1000 <- function (x) ORAverageQuarts(1000, x)

ORS100 <- function (x) ORAverageSplits(100, x)
ORS350 <- function (x) ORAverageSplits(350, x)
ORS1000 <- function (x) ORAverageSplits(1000, x)


dev.new()
########## 350
par(mar=rep(0, 4), mfrow=c(1,1), mai=rep(0, 4))
TernaryPlot('Unresolved', 'Different', 'Same', lab.cex=0.8,
            col=BG_COL, point='right',
            grid.lines = 37, grid.lty='solid', grid.col=GRID_COL,
            axis.col=rgb(0.6, 0.6, 0.6),
            padding=0.1, axis.labels = seq(0, 74, by=2))
title(main="\nPartitions", cex.main=0.8)

HorizontalGrid(37)
partition_distances <- SplitsPoints(sq_trees)

lapply(orAnalyses, function (analysis) {
  TernaryLines(ORS350(analysis), col=COL[analysis],  pch=PCH['dot'])
  TernaryPoints(ORS350(analysis)[, 1], col=COL[analysis], 
                pch=PCH[analysis], cex=1.1)
})

########## 100
TernaryPlot('Unresolved', 'Different', 'Same', lab.cex=0.8,
            col=BG_COL, point='right',
            grid.lines = 37, grid.lty='solid', grid.col=GRID_COL,
            axis.col=rgb(0.6, 0.6, 0.6),
            padding=0.1, axis.labels = seq(0, 74, by=2))
title(main="\nPartitions", cex.main=0.8)

HorizontalGrid(37)
partition_distances <- SplitsPoints(sq_trees)

lapply(orAnalyses, function (analysis) {
  TernaryLines(ORS100(analysis), col=COL[analysis],  pch=PCH['dot'])
  TernaryPoints(ORS100(analysis)[, 1], col=COL[analysis], 
                pch=PCH[analysis], cex=1.1)
})

################################################################################
# Initial results from NCHAR = 350: [1_1 to 10_1]
# Partition metrics slightly favour MrBayes; 
# Quartet metrics quite strongly favour implied weighting.
################################################################################
# Further results from NCHAR = 350: [1_1 to 100_1]
# Partition metrics slightly favour MrBayes; 
# Quartet metrics marginally favour MrBayes.
################################################################################
# Further results from NCHAR = 100: [1_1 to 100_1]
# Partition metrics strongly favour MrBayes; 
# Quartet metrics marginally favour MrBayes.
################################################################################
dev.new()
par(mar=rep(0, 4), mfrow=c(2,1), mai=rep(0, 4))
TernaryPlot('Unresolved', 'Different', 'Same', lab.cex=0.8,
            col=BG_COL, point='right',
            grid.lines = 19, grid.lty='solid', grid.col=GRID_COL,
            axis.col=rgb(0.6, 0.6, 0.6),
            padding=0.1, axis.labels = 0:19)
title(main="\nPartitions", cex.main=0.8)

HorizontalGrid(19)
partition_distances <- SplitsPoints(sq_trees)

lapply(orAnalyses, function (analysis) {
  TernaryLines(ORQ350(analysis), col=COL[analysis], pch=PCH['dot'],
               lty=LTY[analysis], lwd=LWD[analysis])
  TernaryPoints(ORQ350(analysis)[, 1], col=COL[analysis], 
                pch=PCH[analysis], cex=1.1)
})


par(mai=c(0, 0.15, 0, 0.15))
zoom=3.5
TernaryPlot('Unresolved', 'Different', 'Same', lab.cex=0.8,
            col=BG_COL, point='right',
            grid.lines = 19, grid.lty='solid', grid.col=GRID_COL,
            axis.col=rgb(0.6, 0.6, 0.6),
            padding=0.01, axis.labels = 0:19,
            xlim = c(0, sqrt(3/4)/zoom),
            ylim = c(0.5-(1/zoom), 0.5))
lapply(orAnalyses, function (analysis) {
  TernaryLines(ORQ350(analysis), col=COL[analysis],  pch=PCH['dot'],
               lty=LTY[analysis], lwd=LWD[analysis])
  TernaryPoints(ORQ350(analysis)[, 1], col=COL[analysis], 
                pch=PCH[analysis], cex=1.1)
})

################################################################################
# Quartets: 
# TODO fix axis.labels (0:19 probably wrong)
################################################################################

dev.new()
par(mar=rep(0, 4), mfrow=c(2,1), mai=rep(0, 4))
TernaryPlot('Unresolved', 'Different', 'Same', lab.cex=0.8,
            col=BG_COL, point='right',
            grid.lines = 19, grid.lty='solid', grid.col=GRID_COL,
            axis.col=rgb(0.6, 0.6, 0.6),
            padding=0.1, axis.labels = 0:19)
title(main="\nQuartets (350 characters)", cex.main=0.8)

HorizontalGrid(19)

lapply(orAnalyses, function (analysis) {
  TernaryLines(ORQ350(analysis), col=COL[analysis], pch=PCH['dot'],
               lty=LTY[analysis], lwd=LWD[analysis])
  TernaryPoints(ORQ350(analysis)[, 1], col=COL[analysis], 
                pch=PCH[analysis], cex=1.1)
})

par(mai=c(0, 0.15, 0, 0.15))
zoom=3.5
TernaryPlot('Unresolved', 'Different', 'Same', lab.cex=0.8,
            col=BG_COL, point='right',
            grid.lines = 19, grid.lty='solid', grid.col=GRID_COL,
            axis.col=rgb(0.6, 0.6, 0.6),
            padding=0.01, axis.labels = 0:19,
            xlim = c(0, sqrt(3/4)/zoom),
            ylim = c(0.5-(1/zoom), 0.5))
lapply(orAnalyses, function (analysis) {
  TernaryLines(ORQ350(analysis), col=COL[analysis],  pch=PCH['dot'],
               lty=LTY[analysis], lwd=LWD[analysis])
  TernaryPoints(ORQ350(analysis)[, 1], col=COL[analysis], 
                pch=PCH[analysis], cex=1.1)
})