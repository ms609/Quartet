library(testthat)
library(Quartet)
setwd('../testthat')
lapply(list.files('testthat', pattern = 'test\\-.*\\.R', full.names= TRUE),
       source)
