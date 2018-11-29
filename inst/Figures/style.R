require('ape')
require('Quartet')
library('Ternary')
library('CongreveLamsdell2016')
data('clBremPartitions', 'clBremQuartets', 'clMkvPartitions', 'clMkvQuartets',
     'clBootFreqPartitions', 'clBootFreqQuartets', 'clBootGcPartitions', 'clBootGcQuartets',
     'clJackFreqPartitions', 'clJackFreqQuartets', 'clJackGcPartitions', 'clJackGcQuartets',
     'clCI')

PCH <- c(
  brem = 2,
  bootG = 0,
  bootF = 5,
  jackG = 3,
  jackF = 4,
  none = NA,
  
  mk = 1,
  markov = 1,
  equal  = 61, #'='
  eq = 61,
  dot = 183, #'.'
  k1 = 183,
  implied1   = 183, #2,   #triup
  k2 = 3, 
  implied2   = 3, #2,   #triup
  k3 = 3, 
  implied3   = 3, #2,   #triup
  k5 = 183,
  implied5   = 183, #2,   #triup
  kX = 3,
  implied10  = 3, #2,   #triup
  implied20  = 183, #2,   #triup
  implied200 = 183, #2,   #triup
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
                                 cex=FONT_SIZE, text.font=3, inset=c(-0.056, 0))

# Use Inkscape to generate EPS from SVG.  R creates bitmap EPS due to semitrans.
Write <- if (OUTPUT == 'pdf') {
  function(filename, ...) cairo_pdf(filename=paste0("inst/figures/", filename, ".pdf"), ...)
} else {
  function(filename, ...) svg(filename=paste0("inst/figures/", filename, ".svg"), ...)
} 
