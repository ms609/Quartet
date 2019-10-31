context("QuartetDistance.R")

data('sq_trees')
quartets <- read.tree('../trees/all_quartets.new')

test_that("Splits are compared", {
  expect_equal(c(N=252L, Q=126L, s=120L, d=6L, r1=0L, r2=0L, u=0L), 
               SharedQuartetStatus(TreeTools::UnshiftTree(
                 ape::drop.tip(sq_trees$move_one_near, 10),
                 ape::drop.tip(sq_trees$ref_tree, 11)))[2, ])
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

test_that("CompareQuartets works", {
  expect_equal(c(N=16L, Q=8L, s=1, d=2, r1=1, r2=1, u=3),
               CompareQuartets(c(2, 2, 4, 2, 0, 0, 0, 0),
                               c(2, 3, 3, 0, 2, 0, 0, 0)))
})
