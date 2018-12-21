context("PartitionDistance.R")

data('sq_trees')

test_that("SplitStatus works", {
  expect_equal(c(15, 8), dim(SplitStatus(sq_trees)))
  expect_equal(SplitStatus(sq_trees, sq_trees$ref_tree), SplitStatus(sq_trees))
})

test_that("Splits are compared", {
  expect_equal(c(N=12L, P1=6L, P2=6L, s=5L, d1=1L, d2=1L, r1=0L, r2=0L),
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
  expect_equal(c(N = 6, P1=3L, P2=3L, s=3, d1=0, d2=0, r1=0, r2=0),
               CompareSplits(splits9Fewer, splits9))
  
  splitsA <- Tree2Splits(ape::read.tree(text="((((a, b, c, c2), g), h), (d, (e, f)));"))
  splitsB <- Tree2Splits(ape::read.tree(text="(((((a, b), (c, c2)), h), g), (d, e, f));"))
  
  expect_equal(c(N=9L, P1 = 4L, P2 = 5L, s=2L, d1=1L, d2=1L, r1=1L, r2=2L), CompareSplits(splitsA, splitsB))
  expect_equal(c(tree = 5L), RobinsonFoulds(CompareSplits(splitsA, splitsB), similarity = FALSE))
  
  splitsC <- Tree2Splits(ape::read.tree(text="(((a, d), e), (b, (f, c)));"))
  splitsD <- Tree2Splits(ape::read.tree(text="((a, b, c), (d, (e, f)));"))
  
  expect_equal(c(N=5L, P1=3L, P2 = 2L, s=0L, d1=3L, d2=2L, r1=0L, r2=0L), CompareSplits(splitsC, splitsD))
  expect_equal(c(tree = 5L), RobinsonFoulds(CompareSplits(splitsC, splitsD), similarity = FALSE))

  
})
