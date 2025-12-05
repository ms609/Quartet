test_that("PairwiseQuartets() works", {
  expect_equal(PairwiseQuartets(sq_trees),
               QuartetDivergence(ManyToManyQuartetAgreement(sq_trees)))
  expect_equal(PairwiseQuartets(sq_trees, DoNotConflict),
               DoNotConflict(ManyToManyQuartetAgreement(sq_trees)))
})
