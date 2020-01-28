context("tqDist tests")

TreePath <- function (fileName) {
  paste0(system.file(package='Quartet'), '/../tests/trees/',
         fileName, '.new')
}

test_that("tqDist returns correct quartet distances", {
  expect_true(file.exists(TreePath("quartet1")))
  expect_equal(QuartetDistance(TreePath("quartet1"), TreePath("quartet1")), 0L)
  expect_equal(QuartetDistance(TreePath("quartet1"), TreePath("quartet2")), 1L)
  expect_error(QuartetDistance(TreePath("quartet1"), TreePath("file_does_not_exist")))
  expect_equal(QuartetDistance(TreePath("quartet2"), TreePath("quartet1")), 1L)
 
  expect_equal(QuartetAgreement(TreePath("unresolved1"), TreePath("unresolved2")),
               c(3, 0))
  
  expect_equal(QuartetDistance(TreePath("test_tree1"), TreePath("test_tree2")), 26L)
  expect_equal(QuartetDistance(TreePath("test_tree2"), TreePath("test_tree1")), 26L)
  
  expect_equal(QuartetDistance(TreePath("test_tree3"), TreePath("test_tree4")), 5485860L)
  expect_equal(QuartetDistance(TreePath("test_tree4"), TreePath("test_tree3")), 5485860L)
 
  expect_error(PairsQuartetDistance(TreePath("quartet1"), TreePath("two_quartets")))
  expect_equal(PairsQuartetDistance(TreePath("one_quartet_twice"), TreePath("two_quartets")), c(0, 1))
  
  expect_equal(OneToManyQuartetAgreement(TreePath("quartet_unresolved"),
                                         TreePath("all_quartets")),
               matrix(c(rep(0, 7), 1), ncol=2, dimnames=list(NULL, c('A', 'E'))))
  
  expect_equal(OneToManyQuartetAgreement(TreePath("quartet1"),
                                         TreePath("all_quartets")),
               matrix(c(1, rep(0, 7)), ncol=2, dimnames=list(NULL, c('A', 'E'))))
  
  allPairs <- AllPairsQuartetDistance(TreePath("five_trees"))
  
  # Tests taken from those provided with tqDist
  # 1 added to convert from C to R (first element = 1)
  expect_equal(allPairs[1 + 1L, 0 + 1L], 1L)
  expect_equal(allPairs[0 + 1L, 0 + 1L], 0L)
  expect_equal(allPairs[2 + 1L, 1 + 1L], 1L)
  expect_equal(allPairs[3 + 1L, 2 + 1L], 1L)
  
  allPairsAgreement <- AllPairsQuartetAgreement(TreePath("unresolved_list"))
  
  expect_equal(c(12, 15, 9, 0), diag(allPairsAgreement[, , 1]))
  expect_equal(c(12, 15, 9, 0), 15-diag(allPairsAgreement[, , 2]))
  expect_equal(c(9L, 3L), as.integer(allPairsAgreement[1, 3, ]))
  expect_equal(c(0L, 6L), as.integer(allPairsAgreement[3, 4, ]))
})

test_that("tqDist runs from temporary files", {
  allQuartets <- ape::read.tree(TreePath("all_quartets"))
  tqae <- TQAE(allQuartets)
  expect_equal(dimnames(tqae), list(NULL, NULL, c('A', 'E')))
  expect_equal(dim(tqae), c(4, 4, 2))
  expect_equal(diag(4), tqae[, , 'A'] + tqae[, , 'E'])
  expect_equal(c('E' = 1L), tqae[4, 4, 'E'])
  
  mmqa <- ManyToManyQuartetAgreement(allQuartets)
  expect_equal(tqae[, , 'A'], mmqa[, , 's'])
  expect_equal(0L, unique(mmqa[, , 'r1'][-c(4, 8, 12)]))
  expect_equal(1L, unique(mmqa[, , 'r1'][c(4, 8, 12)]))
  expect_equal(mmqa[, , 'r1'], t(mmqa[, , 'r2']))  
  expect_equal(tqae[, , 'E'], mmqa[, , 'u'])
  
  expect_equal(c(N=2L, Q=1L, s=0, d=1, r1=0, r2=0, u=0),
               SingleTreeQuartetAgreement(allQuartets[[1]], allQuartets[[2]])[1, ])
  
  expect_true(file.remove(TQFile(list(allQuartets[[1]]))))
  expect_error(TQFile("Not class phylo"))
  
  expect_error(OneToManyQuartetAgreement(TreePath("all_quartets"), TreePath("all_quartets")))
  expect_error(OneToManyQuartetAgreement(TreePath("quartet_unresolved"), TreePath("none")))
})

test_that("tqDist returns correct triplet distances", {
  
  expect_equal(TripletDistance(TreePath("tree_ab-c"), TreePath("tree_ac-b")), 1L)
  expect_equal(TripletDistance(TreePath("tree_ac-b"), TreePath("tree_ab-c")), 1L)
  
  expect_equal(TripletDistance(TreePath("test_tree1"), TreePath("test_tree2")), 26L)
  expect_equal(TripletDistance(TreePath("test_tree2"), TreePath("test_tree1")), 26L)
  
  expect_equal(TripletDistance(TreePath("test_tree3"), TreePath("test_tree4")), 187793L)
  expect_equal(TripletDistance(TreePath("test_tree4"), TreePath("test_tree3")), 187793L)

  allPairs <- AllPairsTripletDistance(TreePath("two_trees"));
  
  expect_error(AllPairsTripletDistance(c(TreePath("quartet1"), TreePath("two_quartets"))))
  expect_error(PairsTripletDistance(TreePath("quartet1"), TreePath("two_quartets")))
  expect_equal(c(0, 4), PairsTripletDistance(TreePath("one_quartet_twice"), TreePath("two_quartets")))
  
  
  # Tests taken from those provided with tqDist
  # 1 added to convert from C to R (first element = 1)
  expect_equal(allPairs[1 + 1L, 0 + 1L],  26L)
  expect_equal(allPairs[0 + 1L, 0 + 1L],  0L)
  expect_equal(allPairs[2 + 1L, 1 + 1L],  26L)
})

test_that("QuartetStatus works", {
  randomTreeIds <- c(30899669, 9149275, 12823175, 19740197, 31296318,
                     6949843, 30957991, 32552966, 22770711, 21678908)
  randomTrees <- ape::as.phylo(randomTreeIds, 11L)
  expect_false(any(is.na(QuartetStatus(randomTrees, cf=TreeTools::PectinateTree(11)))))
})