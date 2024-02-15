test_that("CPDT produces sane results", {
  tree1 <- TreeTools::BalancedTree(8)
  rtree1 <- TreeTools::RootTree(TreeTools::BalancedTree(8), "t1")
  tree2 <- TreeTools::PectinateTree(8)
  q <- QuartetStatus(tree1, tree2)
  expect_error(CPDTDist(1, tree1), "`tree1` must be")
  expect_error(CPDTDist(tree1, 2), "`tree2` must be")
  # expect_warning(
  # expect_equal(CPDTDist(tree1, rtree1), 0),
  # "Unrooted"
  # )
  expect_equal(CPDTDist(rtree1, rtree1), 0)
  f1 <- TQFile(rtree1)
  f2 <- TQFile(rtree2)
  on.exit(unlink(c(f1, f2)))
  TripletDistance(f1, f2)
  #expect_equal(CPDTDist(tree2, tree2), 0)
  #expect_equal(CPDTDist(rtree1, tree2), 28)
})
