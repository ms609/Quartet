context("Counting unresolved quartets in a single tree")

test_that("Resolution is counted correctly", {
  unresolvers <- ape::read.tree('../trees/unresolved_list.new')
  quartets <- vapply(unresolvers, ResolvedQuartets, integer(2))
  triplets <- vapply(unresolvers, ResolvedTriplets, integer(2))
  expect_equal(c(3, 0, 6, 15), quartets[2, ])
  expect_equal(c(1, 0, 2, 20), triplets[2, ])
})