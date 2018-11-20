context("tqDist")

test_that("tqDist returns correct quartet distances", {
  expect_true(file.exists("../trees/quartet1.new"))
  expect_equal(QuartetDistance("../trees/quartet1.new", "../trees/quartet1.new"), 0L)
  expect_equal(QuartetDistance("../trees/quartet1.new", "../trees/quartet2.new"), 1L)
  expect_error(QuartetDistance("../trees/quartet1.new", "../trees/file_does_not_exist.new"))
  expect_equal(QuartetDistance("../trees/quartet2.new", "../trees/quartet1.new"), 1L)
 
  cat(QuartetAgreement("../trees/unresolved1.new", "../trees/unresolved2.new"))
  expect_equal(QuartetAgreement("../trees/unresolved1.new", "../trees/unresolved2.new"),
               c(3, 0))
  
  expect_equal(QuartetDistance("../trees/test_tree1.new", "../trees/test_tree2.new"), 26L)
  expect_equal(QuartetDistance("../trees/test_tree2.new", "../trees/test_tree1.new"), 26L)
  
  expect_equal(QuartetDistance("../trees/test_tree3.new", "../trees/test_tree4.new"), 5485860L)
  expect_equal(QuartetDistance("../trees/test_tree4.new", "../trees/test_tree3.new"), 5485860L)
 
  expect_error(PairsQuartetDistance("../trees/quartet1.new", "../trees/two_quartets.new"))
  cat(PairsQuartetDistance("../trees/one_quartet_twice.new", "../trees/two_quartets.new"))
  expect_equal(PairsQuartetDistance("../trees/one_quartet_twice.new", "../trees/two_quartets.new"), c(0, 1))
  
  expect_equal(OneToManyQuartetAgreement("../trees/quartet_unresolved.new",
                                         "../trees/all_quartets.new"),
               matrix(c(rep(0, 7), 1), ncol=2, dimnames=list(NULL, c('A', 'E'))))
  
  expect_equal(OneToManyQuartetAgreement("../trees/quartet1.new",
                                         "../trees/all_quartets.new"),
               matrix(c(1, rep(0, 7)), ncol=2, dimnames=list(NULL, c('A', 'E'))))
  
  allPairs <- AllPairsQuartetDistance("../trees/five_trees.new")
  
  # Tests taken from those provided with tqDist
  # 1 added to convert from C to R (first element = 1)
  expect_equal(allPairs[1 + 1L, 0 + 1L], 1L)
  expect_equal(allPairs[0 + 1L, 0 + 1L], 0L)
  expect_equal(allPairs[2 + 1L, 1 + 1L], 1L)
  expect_equal(allPairs[3 + 1L, 2 + 1L], 1L)
  
  allPairsAgreement <- AllPairsQuartetAgreement("../trees/unresolved_list.new")
  
  expect_equal(c(12, 15, 9, 0), diag(allPairsAgreement[, , 1]))
  expect_equal(c(12, 15, 9, 0), 15-diag(allPairsAgreement[, , 2]))
  expect_equal(c(9L, 3L), as.integer(allPairsAgreement[1, 3, ]))
  expect_equal(c(0L, 6L), as.integer(allPairsAgreement[3, 4, ]))
})

test_that("tqDist returns correct triplet distances", {
  
  expect_equal(TripletDistance("../trees/tree_ab-c.new", "../trees/tree_ac-b.new"), 1L)
  expect_equal(TripletDistance("../trees/tree_ac-b.new", "../trees/tree_ab-c.new"), 1L)
  
  expect_equal(TripletDistance("../trees/test_tree1.new", "../trees/test_tree2.new"), 26L)
  expect_equal(TripletDistance("../trees/test_tree2.new", "../trees/test_tree1.new"), 26L)
  
  expect_equal(TripletDistance("../trees/test_tree3.new", "../trees/test_tree4.new"), 187793L)
  expect_equal(TripletDistance("../trees/test_tree4.new", "../trees/test_tree3.new"), 187793L)

  allPairs <- AllPairsTripletDistance("../trees/two_trees.new");
  
  # Tests taken from those provided with tqDist
  # 1 added to convert from C to R (first element = 1)
  expect_equal(allPairs[1 + 1L, 0 + 1L],  26L)
  expect_equal(allPairs[0 + 1L, 0 + 1L],  0L)
  expect_equal(allPairs[2 + 1L, 1 + 1L],  26L)
})
