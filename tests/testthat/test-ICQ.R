context("ICQ.R; AllQuartets.cpp")

test_that("quartets are correctly indexed", {
  expect_equal(seq_len(15) - 1L, apply(all_quartets(6) - 1L, 2, which_index, 6))
})