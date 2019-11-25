library(testthat)
pkgload::load_all()
lapply(list.files('testthat', pattern = 'test\\-.*\\.R', full.names= TRUE),
       source)
