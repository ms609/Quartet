context("tqDist")

test_that("tqDist returns correct quartet distances", {
  expect_equal(QuartetDistance("../trees/quartet1.new", "../trees/quartet2.new"), 1L)
  expect_equal(QuartetDistance("../trees/quartet2.new", "../trees/quartet1.new"), 1L)

  expect_equal(QuartetDistance("../trees/test_tree1.new", "../trees/test_tree2.new"), 26L)
  expect_equal(QuartetDistance("../trees/test_tree2.new", "../trees/test_tree1.new"), 26L)
  
  expect_equal(QuartetDistance("../trees/test_tree3.new", "../trees/test_tree4.new"), 5485860L)
  expect_equal(QuartetDistance("../trees/test_tree4.new", "../trees/test_tree3.new"), 5485860L)
 
  allPairs <- AllPairsQuartetDistance("../trees/five_trees.new")
  
  expect_equal(allPairs[1, 0], 1L)
  expect_equal(allPairs[0, 0], 0L)
  expect_equal(allPairs[2, 1], 1L)
  expect_equal(allPairs[3, 2], 1L)
})

test_that("tqDist returns correct triplet distances", {
  
  expect_equal(TripletDistance("../trees/tree_ab-c.new", "../trees/tree_ac-b.new"), 1L)
  expect_equal(TripletDistance("../trees/tree_ac-b.new", "../trees/tree_ab-c.new"), 1L)
  
  expect_equal(TripletDistance("../trees/test_tree1.new", "../trees/test_tree2.new"), 26L)
  expect_equal(TripletDistance("../trees/test_tree2.new", "../trees/test_tree1.new"), 26L)
  
  expect_equal(TripletDistance("../trees/test_tree3.new", "../trees/test_tree4.new"), 187793L)
  expect_equal(TripletDistance("../trees/test_tree4.new", "../trees/test_tree3.new"), 187793L)

  allPairs <- AllPairsTripletDistance("../trees/two_trees.new");
  
  expect_equal(allPairs[1, 0],  26L)
  expect_equal(allPairs[0, 0],  0L)
  expect_equal(allPairs[2, 1],  26L)
})