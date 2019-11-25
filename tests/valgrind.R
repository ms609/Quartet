library(testthat)
library(pkgload)
load_all()
getwd()
dir()
setwd('tests/testthat')
lapply(list.files('testthat', pattern = 'test\\-.*\\.R', full.names= TRUE),
       source)
