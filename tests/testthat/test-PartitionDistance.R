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
