context('AllQuartets.cpp')
test_that('All quartets are generated', {
  expect_equal(combn(8, 4), all_quartets(8))
})