context("Plotting functions")

data('sq_trees')

test_that("Splits are compared", {
  expect_equal(c(15, 3), dim(QuartetPoints(sq_trees)))
  expect_identical(330, unique(rowSums(QuartetPoints(sq_trees))))
  expect_equal(c(15, 3), dim(BipartitionPoints(sq_trees)))
  expect_identical(8, unique(rowSums(BipartitionPoints(sq_trees))))
  expect_equal(c(0,0,8, 0,1,7, 0,3,5, 0,4,4, 0,1,7, 0,2,6, 0,3,5,
                 1,0,7, 5,0,3, 1,3,4, 3,3,2, 1,2,5, 5,0,3, 0,8,0, 0,8,0), as.integer(unlist(t(SplitPoints(sq_trees)))))
  expect_equal(c(15, 3), dim(BipartitionPoints(sq_trees, cf=sq_trees$collapse_some)))
})
