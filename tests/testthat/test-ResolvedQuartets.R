context("Counting unresolved quartets in a single tree")

TreePath <- function (fileName) {
  paste0(system.file(package='Quartet'), '/trees/', fileName, '.new')
}

test_that("Resolution is counted correctly", {
  unresolvers <- ape::read.tree(TreePath('unresolved_list'))
  quartets <- vapply(unresolvers, ResolvedQuartets, integer(2))
  triplets <- vapply(unresolvers, ResolvedTriplets, integer(2))
  expect_equal(c(3, 0, 6, 15), quartets[2, ])
  expect_equal(c(1, 0, 2, 20), triplets[2, ])
})
