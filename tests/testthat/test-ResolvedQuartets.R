context("Counting unresolved quartets in a single tree")

TreePath <- function (fileName) {
  paste0(system.file(package='Quartet'), '/trees/', fileName, '.new')
}

test_that("ResolvedQuartets() warns", {
  expect_warning(ResolvedQuartets(CollapseNode(BalancedTree(478), 500:600)))
})

test_that("Resolution is counted correctly", {
  unresolvers <- ape::read.tree(TreePath('unresolved_list'))
  quartets <- vapply(unresolvers, ResolvedQuartets, integer(2))
  triplets <- vapply(unresolvers, ResolvedTriplets, integer(2))
  expect_equal(c(3, 0, 6, 15), quartets[2, ])
  expect_equal(c(1, 0, 2, 20), triplets[2, ])
  
  # Uncanny node ordering
  tree <- structure(list(
    edge = matrix(c(6, 6, 6, 8, 8, 7, 7,
                    1, 2, 8, 3, 7, 4, 5), 7L, 2L),
    tip.label = paste0('t', 1:5),
    Nnode = 3L), class = 'phylo')
  expect_equal(c(5L, 0L), ResolvedQuartets(tree))

})
