library(testthat)
library(pkgload)
load_all()
getwd()
dir()
setwd('testthat')
testFiles <- list.files(pattern = 'test\\-.*\\.R', full.names= TRUE)
# No doppleganger testing in interactive mode
lapply(testFiles[-3], source)
