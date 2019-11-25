library(testthat)
library(Quartet)
lapply(list.files('testthat', pattern = 'test\\-.*\\.R', full.names= TRUE),
       source)
