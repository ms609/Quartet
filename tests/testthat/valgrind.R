library(testthat)
library(pkgload)
load_all()
getwd()
dir()
if (dir.exists('testthat')) setwd('testthat/tests')
testFiles <- list.files(pattern = 'test\\-.*\\.R', full.names= TRUE)
# No doppleganger testing in interactive mode
lapply(testFiles[-c(3, 6)], source)
