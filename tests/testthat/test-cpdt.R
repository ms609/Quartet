test_that("CPDT produces correct results with four leaves", {
  tree4.1 <- ape::read.tree(text="(1, (2, (3, 4)));")
  tree4.2 <- ape::read.tree(text="(1, (4, (3, 2)));")
  q <- QuartetStatus(tree4.1, tree4.2)
  expect_equal(CPDTDist(tree4.1, tree4.1), 0)
  expect_equal(CPDTDist(tree4.2, tree4.2), 0)
  
  
  f4.1 <- TQFile(tree4.1)
  f4.2 <- TQFile(tree4.2)
  on.exit(unlink(c(f4.1, f4.2)))
  
  expect_equal(CPDTDist(tree4.1, tree4.2), TripletDistance(f4.1, f4.2))
})

test_that("CPDT produces sane results", {
  skip_if(TRUE)
  #
  #
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
  f2 <- TQFile(tree2)
  on.exit(unlink(c(f1, f2)))
  
  #expect_equal(CPDTDist(tree2, tree2), 0)
  #expect_equal(CPDTDist(rtree1, tree2), TripletDistance(f1, f2))
})
