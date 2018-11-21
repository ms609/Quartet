context("QuartetDistance.R")

data('sq_trees')
quartets <- read.tree('../trees/all_quartets.new')

test_that("Splits are compared", {
  expect_equal(c(126, 120, 6, 0, 0, 0)  + BLANK_QUARTET, 
               SharedQuartetStatus(UnshiftTree(
                 ape::drop.tip(sq_trees$move_one_near, 10),
                 ape::drop.tip(sq_trees$ref_tree, 11)))[2, ])
})

test_that("QuartetStates works", {
  expect_equal(list(2L), QuartetStates(quartets[[1]]))
  expect_equal(list(3L), QuartetStates(Tree2Splits(quartets[[2]])))
  expect_equal(list(2L, 3L, 4L, 0L), QuartetStates(quartets))
})

test_that("QuartetState works", {
  expect_equal(2L, QuartetState(letters[1:4], Tree2Splits(quartets[[1]])))
  expect_equal(3L, QuartetState(letters[1:4], Tree2Splits(quartets[[2]])))
  expect_equal(4L, QuartetState(letters[1:4], Tree2Splits(quartets[[3]])))
  expect_equal(0L, QuartetState(letters[1:4], Tree2Splits(quartets[[4]])))
})

test_that("CompareQuartets works", {
  expect_equal(BLANK_QUARTET + c(8, 1, 2,    1, 1, 3),
               CompareQuartets(c(   2, 2, 4, 2, 0, 0, 0, 0),
                               c(   2, 3, 3, 0, 2, 0, 0, 0)))
})
