context("Plotting functions")

data('sq_trees')

test_that("Splits comparisons are correct", {
  
  
  expect_equal(c(length(sq_trees), 3), dim(BipartitionPoints(sq_trees)))
  expect_identical(8, unique(rowSums(BipartitionPoints(sq_trees))))
  expect_equal(c(length(sq_trees), 3), dim(BipartitionPoints(sq_trees, cf=sq_trees$collapse_some)))
  expect_equal(c(0,0,8, 0,1,7, 0,3,5, 0,4,4, 0,1,7, 0,2,6, 0,3,5,
                 1,0,7, 5,0,3, 1,3,4, 3,3,2, 1,2,5, 5,0,3, 0,8,0, 
                 0,4,4, rep(c(0,8,0), 3)),
               as.integer(unlist(t(SplitPoints(sq_trees)))))
 
  splitsVsOne <- SplitPoints(sq_trees, cf=sq_trees$collapse_one)
  expect_equal(rownames(splitsVsOne), names(sq_trees))
  # Numbers lazily copied from output, rather than calculated;
  # independent verification could be valuable.
  expect_equal(c(rep(0, 8),4,0,2,0,4,rep(0, 5)), splitsVsOne$Unresolved)
  expect_equal(c(0,1,3,4,1,2,3,0,0,3,3,2,0,7,3,7,7,7), splitsVsOne$Contradicted)
  expect_equal(c(7,6,4,3,6,5,4,7,3,4,2,5,3,0,4,0,0,0), splitsVsOne$Consistent)
  expect_equal(as.integer(data.frame(Unresolved=0, Contradicted=0, Consistent=3)),
               as.integer(SplitPoints(sq_trees$collapse_one, cf=sq_trees$collapse_some)))
}
)

test_that("Quartet comparisons are correct", {
  expect_equal(c(length(sq_trees), 3), dim(QuartetPoints(sq_trees)))
  expect_identical(330, unique(rowSums(QuartetPoints(sq_trees))))
  # Numbers lazily copied from output, rather than calculated;
  # independent verification could be valuable.
  cfCollapseOne <- QuartetPoints(sq_trees, cf=sq_trees$collapse_one)
  expect_equal(c(rep(0, 8), 115, 0, 57, 0, 197, rep(0, 5)), cfCollapseOne$Unresolved)
  expect_equal(c(0, 8, 52, 76, 24, 78, 92, 0, 0, 52, 52, 78, 0, 237,
                 84, 206, 240, 219), cfCollapseOne$Contradicted)
})

test_that("Line ends are correct", {
  expect_equal(c(r2a = 0, da = 0.5, sa = 0.5, r2b = 2/3, db = 0, sb = 1/3), 
               SymmetricDifferenceLineEnds(0.5)[1, ])
  expect_equal(c(r2a = 0, da = 0.5, sa = 0.5, r2b = 2/3, db = 0, sb = 1/3), 
               SymmetricDifferenceLineEnds(1:3/4)[2, ])
  expect_equal(2L, unique(rowSums(SymmetricDifferenceLineEnds(1:10/10))))
  expect_error(unique(rowSums(SymmetricDifferenceLineEnds(1:11/10))))
})
