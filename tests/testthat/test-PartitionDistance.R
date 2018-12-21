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

test_that("CompareSplits works", {
  set.seed(1)
  splits6 <- Tree2Splits(ape::rtree(6, br=NULL)) # No longer needed but
  # preserves random seed!
  splits9 <- Tree2Splits(ape::rtree(9, br=NULL))
  splits9Fewer <- splits9[-4:-6 ,]
  
  expect_error(CompareSplits(matrix(FALSE, 2, 2), matrix(TRUE, 3, 3)))
  expect_error(CompareSplits(splits9, splits9Fewer))
  expect_equal(c(one=3, two=3, both=3, one_not_two=0, two_not_one=0, RF_dist=0),
               CompareSplits(splits9Fewer, splits9))
  
  splitsA <- Tree2Splits(ape::read.tree(text="((((a, b, c, c2), g), h), (d, (e, f)));"))
  splitsB <- Tree2Splits(ape::read.tree(text="(((((a, b), (c, c2)), h), g), (d, e, f));"))
  
  expect_equal(c(N=9L, s=2L, d=1L, r1=1L, r2=2L, RF=5L), CompareSplits(splitsA, splitsB))
  
  splitsC <- Tree2Splits(ape::read.tree(text="(((a, d), e), (f, (b, c)));"))
  splitsD <- Tree2Splits(ape::read.tree(text="((a, b, c), (d, (e, f)));"))
  
  expect_equal(c(N=5L, s=0L, d1=3L, d2=2L, r1=0L, r2=0L, RF=5L), CompareSplits(splitsC, splitsD))
  
  
})
