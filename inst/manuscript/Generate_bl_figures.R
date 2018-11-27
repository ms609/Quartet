require('ape')
require('Quartet')
library('Ternary')
library('CongreveLamsdell2016')
data('clBremPartitions', 'clBremQuartets', 'clMkvPartitions', 'clMkvQuartets',
     'clBootPartitions', 'clBootQuartets', 'clJackPartitions', 'clJackQuartets',
     'clCI')
library('OReillyEtAl2016')
data('orQuartets')
data('orPartitions')

# Use Inkscape to generate EPS from SVG.  R creates bitmap EPS due to semitrans.
Write <- function(filename, ...) cairo_pdf(filename=paste0("inst/", filename, ".pdf"), ...)
Write <- function(filename, ...) svg(filename=paste0("inst/", filename, ".svg"), ...)

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
  text(text_x, -0.5, paste0(QuartetStatus(list(tr, ref))['d', 2], '/', choose(11,4)), cex=0.8, pos=4)
  text(text_x, -1.5, paste0(topo_dist, '/', rf_max), cex=0.8, pos=4)
  text(text_x, -2.5, paste0(signif(tree_dist[2], 3)), cex=0.8, pos=4)
  text(text_x, -3.5, paste0(phangorn::sprdist(tr, ref)[1]), cex=0.8, pos=4)
}

polyplot <- function (tr, title, highlight, ...) {
  tree_pair <- lapply(list(ref_tree, tr), ape::unroot)
  class(tree_pair) <- 'multiPhylo'
  partitions <- SplitStatus(tree_pair)[, 2]
  
  colplot(tr, title, highlight, ...)
  x_mid <- par('usr')[2] * 0.62
  
  text(x_mid, -0.5, cex=0.8, pos=2, "Quartets contradicted")
  text(x_mid, -1.5, cex=0.8, pos=2, "Quartets unresolved")
  text(x_mid, -2.5, cex=0.8, pos=2, "Partitions contradicted")
  text(x_mid, -3.5, cex=0.8, pos=2, "Partitions unresolved")
  
  text(x_mid, -0.5, cex=0.8, pos=4, 
       paste0(QuartetStatus(tree_pair)['d', 2], '/', choose(11,4)))
  text(x_mid, -1.5, cex=0.8, pos=4, 
       paste0(sum(QuartetStatus(tree_pair)[c('r1','r2','u'), 2]), '/', choose(11,4)))
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
  implied1   = 183, #2,   #triup
  implied2   = 3, #2,   #triup
  implied3   = 3, #2,   #triup
  implied5   = 183, #2,   #triup
  implied10  = 3, #2,   #triup
  implied20  = 183, #2,   #triup
  implied200 = 183, #2,   #triup
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
GRID_LINES <- 10
BG_COL   <- rgb(0.985, 0.985, 0.992)

COL_WIDTH  <- 3.36 # inches; measured from BL
PAGE_WIDTH <- 8.26 # inches; measured from BL
FIG_WIDTH  <- 7.1 # inches; measured from BL
FONT_SIZE  <- 1 # cex
FONT_PT    <- 9 # 9pt measured from BL
FONT_FAMILY <- "serif" # BL

Quartet2Ternary <- function (item) clQuartets[[item]][c('r2', 'd', 's'), , TREE]
  
Split2Ternary <- function (item) {
  itemData <- clPartitions[[item]][, , TREE]
  rbind(itemData['ref', ] - itemData['cf', ],
        itemData['cf_not_ref', ],
        itemData['cf_and_ref', ])
}

TernaryQuarts <- function(zoom=1, padding=0.1) {
  xLim <- c(0, 1 / zoom) - 0.01
  yLim <- c(0.5-(1/zoom), 0.5)
  lab <- if (zoom == 1) c("Unresolved quartets", 
                          "Different quartets",
                          "Identical quartets") else rep('', 3)
  
  TernaryPlot(atip=NULL, btip=NULL, ctip=NULL,
              alab=lab[1], blab=lab[2], clab=lab[3], 
              lab.cex=FONT_SIZE, lab.offset=0.13,
              point='right', isometric = TRUE,
              col=BG_COL,
              grid.lty='solid', grid.col=GRID_COL, grid.lines=GRID_LINES,
              grid.minor.lines = 0,
              axis.labels = 
                if(zoom==1) round(seq(0, choose(22, 4), length.out=20), 0)
                else if (zoom == 3.5) round(seq(0, choose(22, 4), length.out=20), 0)
                else FALSE,
              axis.col=rgb(0.6, 0.6, 0.6),
              ticks.length = if (zoom == 1) 0.025 else 0.009,
              #axis.labels.col = if (zoom == 1) GRID_COL else rgb(0.6, 0.6, 0.6),
              padding=padding, xlim=xLim, ylim=yLim)
  HorizontalGrid(GRID_LINES)
  AddToTernary(lines, list(c(0, 2/3, 1/3), c(1, 0, 0)), lty='dotted', 
               col=cbPalette8[8], lwd=2)
  clPlotAverageQuartets(clBremQuartets, COL=clColours)
  clPlotAverageQuartets(clBootQuartets, pch=3)
  clPlotAverageQuartets(clJackQuartets, pch=4)
  JoinTheDots(clMkvQuartets[[TREE]][, c('r2', 'd', 's')], col=clColours['mk'], 
              cex=1.1, pch=PCH_MK)
}

AddArrows <- function (quality) {
  arrows(sqrt(3/4) * 0.5, 0.5, sqrt(3/4) * 0.8, 0.5, length=0.08)
  text  (sqrt(3/4) * 0.65, 0.5, pos=3, 'Decreasing resolution', cex=FONT_SIZE)
  arrows(sqrt(3/4) * 0.98, 0.40, sqrt(3/4) * 0.98, 0.20, length=0.08)
  text  (sqrt(3/4) * 1.01, 0.30, pos=3, quality, cex=FONT_SIZE, srt=270)
}

AddLegend <- function(pos='bottomright')
  legend(pos, cex=FONT_SIZE, bty='n',
         lty=1,
         pch=c(PCH_MK, PCH_EQ, PCH_XX, PCH_IW, PCH_XX, PCH_XX, PCH_IW, PCH_IC), pt.cex=1.1,
         col=c(COL_MK, COL_EQ, COL10, COL_5, COL_3, COL_2, COL_1, COL_C),
         legend=c('Markov', 'Equal weights', paste0('Implied, k=', c(10, 5, 3, 2, 1, '2..10')))
  )

Panel <- function (panel) legend('topleft', paste0('(', panel, ')'), bty='n', 
                                 cex=FONT_SIZE, text.font=3, inset=c(-0.056, 0))

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

#dev.new()

Write(filename="Figure_1", width=FIG_WIDTH, height=FIG_WIDTH,
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

par(mai=c(0, 0.15, 0, 0.15))
TernaryQuarts(zoom=3.5, padding=0.01)
text(-0.015, 0.345, 'Identical quartets', cex=FONT_SIZE, srt=90)
text(0.126, 0.45, 'Unresolved quartets', cex=FONT_SIZE, srt=330)
lapply(otherYs, function (y) lines(c(0, rightPoint[1]), c(y, rightPoint[2]),
                                   lty='dashed', col='#00000022'))

Panel('b')
AddLegend('topright')


################################################################################

################################################################################

TernaryPlot(NULL, NULL, NULL, #'Unresolved', 'Different', 'Same', 
            alab="Unresolved partitions",
            blab="Different partitions",
            clab="Identical partitions",
            lab.cex=FONT_SIZE, lab.offset=0.12,
            grid.lty='solid', grid.col=GRID_COL, grid.lines=19,
            grid.minor.lines = 0,
            col=BG_COL, point='right',
            axis.col=rgb(0.6, 0.6, 0.6),
            padding=0.1, axis.labels = 0:19)
#title(main="\nPartitions", cex.main=0.8)

HorizontalGrid(grid.col='#888888', grid.lines=19)

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

################################################################################

################################################################################
TernaryPlot(NULL, NULL, NULL, #'Unresolved', 'Different', 'Same', 
            alab="Unresolved partitions",
            blab="Different partitions",
            clab="Identical partitions",
            #alab=expression("Unresolved partitions" %->% ''),
            #blab=expression("" %<-% "Different partitions"),
            #clab=expression("Identical partitions" %->% ""),
            lab.cex=FONT_SIZE, lab.offset=0.12,
            col=BG_COL, point='right',
            grid.lines = 19, grid.lty='solid', grid.col=GRID_COL,
            grid.minor.lines = 0,
            axis.col=rgb(0.6, 0.6, 0.6),
            padding=0.1, axis.labels = 0:19)

HorizontalGrid(19)

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

