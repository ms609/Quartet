context("PartitionDistance.R")

data('sq_trees')
quartets <- read.tree('../trees/all_quartets.new')

test_that("SplitStatus works", {
  expect_equal(c(15, 6), dim(SplitStatus(sq_trees)))
  expect_equal(SplitStatus(sq_trees, sq_trees$ref_tree), SplitStatus(sq_trees))
})

test_that("Splits are compared", {
  expect_equal(c(6, 6, 5, 1, 1, 2) + BLANK_SPLIT, 
               SharedSplitStatus(UnshiftTree(
                 ape::drop.tip(sq_trees$move_one_near, 10),
                 ape::drop.tip(sq_trees$ref_tree, 11)))[2, ])
})

test_that("UniqueSplits works", {
  set.seed(1)
  splits6 <- Tree2Splits(ape::rtree(6, br=NULL))
  expect_equal(rep(FALSE, 3), as.logical(UniqueSplits(splits6)['t4', ]))
  expect_equal(!splits6, UniqueSplits(cbind(!splits6, splits6), TRUE))
  
  splits9 <- Tree2Splits(ape::rtree(9, br=NULL))
  splits9Fewer <- splits9[-4:-6 ,]
  
  expect_error(CompareSplits(matrix(FALSE, 2, 2), matrix(TRUE, 3, 3)))
  expect_error(CompareSplits(splits9, splits9Fewer))
  expect_equal(c(one=3,two=3,both=3,one_not_two=0,two_not_one=0,RF_dist=0), CompareSplits(splits9Fewer, splits9))
  
})

test_that("Large splits don't cause memory issues", {
  splits5000 <- cbind(c(rep(TRUE, 2), rep(FALSE, 4998)),
                      c(rep(FALSE, 2), rep(TRUE, 4998)),
                      c(rep(TRUE, 2500), rep(FALSE, 2500)),
                      c(rep(TRUE, 2500), rep(FALSE, 2500)),
                      c(rep(TRUE, 2500), rep(FALSE, 2500)),
                      c(rep(FALSE, 2500), rep(TRUE, 2500))
                      )
  expect_equal(c(5000, 2), dim(UniqueSplits(splits5000)))
})
