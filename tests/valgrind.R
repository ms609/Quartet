library(testthat)
library(pkgload)
load_all()
setwd('./testthat')
lapply(list.files('testthat', pattern = 'test\\-.*\\.R', full.names= TRUE),
       source)
