require('ape')
require('Quartet')
library('Ternary')
library('CongreveLamsdell2016')
data('clBremPartitions', 'clBremQuartets', 'clMkvPartitions', 'clMkvQuartets',
     'clBootFreqPartitions', 'clBootFreqQuartets', 'clBootGcPartitions', 'clBootGcQuartets',
     'clJackFreqPartitions', 'clJackFreqQuartets', 'clJackGcPartitions', 'clJackGcQuartets',
     'clCI', 'cbPalette8')


PCH <- c(
  brem  = 2,
  bootG = 0,
  bootF = 5,
  jackG = 3,
  jackF = 4,
  none  = NA,
  
  mk     = 1,
  markov = 1,
  equal  = 61, #'='
  eq     = 61,
  dot    = 183, #'.'
  
  k1         = 4,
  implied1   = 4,
  k2         = 183, 
  implied2   = 183,
  k3         = 183, 
  implied3   = 183,
  k5         = 3,
  implied5   = 3,
  kX         = 183,
  implied10  = 183,
  implied20  = 183,
  implied200 = 183,
  kC = 17,
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
  black      = paste0(cbPalette8[1], '99'),
  markov     = paste0(cbPalette8[4], '99'),
  mk         = paste0(cbPalette8[4], '99'),
  
  equal      = paste0(cbPalette8[8], '99'),
  eq        = paste0(cbPalette8[8], '99'),
  implied1   = paste0(cbPalette8[6], '42'),
  k1         = paste0(cbPalette8[6], '42'),
  implied2   = paste0(cbPalette8[6], '42'),
  k2         = paste0(cbPalette8[6], '42'),
  implied3   = paste0(cbPalette8[6], '42'),
  k3         = paste0(cbPalette8[6], '42'),
  implied5   = paste0(cbPalette8[6], '42'),
  k5         = paste0(cbPalette8[6], '42'),
  implied10  = paste0(cbPalette8[6], '99'),
  kX         = paste0(cbPalette8[6], '99'),
  implied20  = paste0(cbPalette8[6], '42'),
  implied200 = paste0(cbPalette8[6], '42'),
  kC         = paste0(cbPalette8[2], '99'),
  impliedC   = paste0(cbPalette8[2], '99')
)

SUBOPTIMAL <- list(
  freq = seq(0, 100, length.out=51L),
  gc = seq(-100, 100, length.out=41L),
  brem = c('0', '0.0025', '0.0035', '0.0048', '0.0065', '0.0089', '0.012', '0.017',
           '0.023', '0.031', '0.043', '0.059', '0.081', '0.11', '0.15', '0.21',
           '0.28', '0.39', '0.53', '0.73', '1')
)

GRID_COL <- rgb(0.92, 0.92, 0.92)
GRID_LINES <- 10
BG_COL   <- rgb(0.985, 0.985, 0.992)

COL_WIDTH  <- 3.36 # inches; measured from BL
PAGE_WIDTH <- 8.26 # inches; measured from BL
FIG_WIDTH  <- 7.1 # inches; measured from BL
FONT_SIZE  <- 1 # cex
FONT_PT    <- 9 # 9pt measured from BL
FONT_FAMILY <- "serif" # BL

Panel <- function (panel) legend('topleft', paste0('(', panel, ')'), bty='n', 
                                 cex=FONT_SIZE, text.font=3, inset=c(0, 0))

AddArrows <- function (quality) {
  arrows(sqrt(3/4) * 0.5, 0.5, sqrt(3/4) * 0.8, 0.5, length=0.08)
  text  (sqrt(3/4) * 0.65, 0.5, pos=3, 'Decreasing resolution', cex=FONT_SIZE)
  arrows(sqrt(3/4) * 0.98, 0.40, sqrt(3/4) * 0.98, 0.20, length=0.08)
  text  (sqrt(3/4) * 1.01, 0.30, pos=3, quality, cex=FONT_SIZE, srt=270)
}

# Use Inkscape to generate EPS from SVG.  R creates bitmap EPS due to semitrans.
Write <- if (OUTPUT == 'pdf') {
  function(filename, ...) cairo_pdf(filename=paste0("inst/figures/", filename, ".pdf"), ...)
} else {
  function(filename, ...) svg(filename=paste0("inst/figures/", filename, ".svg"), ...)
} 
