test_that("Quartets are correctly indexed", {
  expect_error(which_index(1:10, 1), "4 indices needed")
  expect_error(which_index(c(-4,3,2,1), 1), "indices[0] must be positive")
  expect_error(which_index(c(4,3,2,0), 1))
  expect_equal(seq_len(15) - 1L, apply(all_quartets(6) - 1L, 2, which_index, 6))
  expect_equal(AllQuartets(6), AllQuartets(BalancedTree(6)))
})

