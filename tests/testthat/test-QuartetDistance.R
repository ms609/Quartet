context("QuartetDistance.R")

data('sq_trees')

TreePath <- function (fileName) {
  paste0(system.file(package='Quartet'), '/trees/', fileName, '.new')
}
quartets <- ape::read.tree(TreePath('all_quartets'))

test_that("Distances are calculated from strings", {
  set.seed(0)
  
  trees <- structure(lapply(rep(15, 5), ape::rtree, br=NULL), class='multiPhylo')
  strs <- write.tree(trees)
  
  fileName <- TQFile(trees)
  on.exit(file.remove(fileName))
  
  expect_equal(AllPairsQuartetDistance(fileName),
               TQDist(trees))
  expect_equal(AllPairsQuartetAgreement(fileName),
               TQAE(trees))
  expect_equal(ManyToManyQuartetAgreement(trees)[1, , ],
               SingleTreeQuartetAgreement(trees, trees[[1]])[, 3:7])
})

test_that("Splits are compared", {
  trees <- TreeTools::UnshiftTree(
    ape::drop.tip(sq_trees$move_one_near, 10),
    ape::drop.tip(sq_trees$ref_tree, 11))
  expect_equal(c(N=252L, Q=126L, s=120L, d=6L, r1=0L, r2=0L, u=0L), 
               SharedQuartetStatus(trees)[2, ])
})

test_that("QuartetStates works", {
  expect_equal(2L, QuartetStates(quartets[[1]]))
  expect_equal(3L, QuartetStates(as.Splits(quartets[[2]], letters[1:4])))
  expect_equal(c(2L, 3L, 4L, 0L), QuartetStates(quartets)[, 1])
})

test_that("QuartetState works", {
  expect_equal(2L, QuartetState(letters[1:4], as.Splits(quartets[[1]])))
  expect_equal(3L, QuartetState(letters[1:4], as.Splits(quartets[[2]])))
  expect_equal(4L, QuartetState(letters[1:4], as.Splits(quartets[[3]])))
  expect_equal(0L, QuartetState(letters[1:4], as.Splits(quartets[[4]])))
})

test_that("PlotQuartet works", {
  library('vdiffr')
  expect_doppelganger('PlotQuartet', function() {
    data('sq_trees')
    
    par(mfrow=c(3, 2), mar=rep(0.5, 4), cex=1.1)
    PlotQuartet(sq_trees[c(1, 9, 13:16)], c(2, 5, 3, 8), overwritePar = FALSE)
  })
})
