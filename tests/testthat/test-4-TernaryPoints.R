context("Plotting functions")

data('sq_trees')

test_that("Quartets are counted correctly", {
  expect_equal(c(15, 3), dim(QuartetPoints(sq_trees)))
  expect_equal(c(15, 3), dim(SplitPoints(sq_trees)))
})
