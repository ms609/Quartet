context("Distances: Quartet distances")

data('sq_trees')
ref_tree <- sq_trees$ref_tree
Metrics <- list(DoNotConflict, ExplicitlyAgree, StrictJointAssertions,
                SemiStrictJointAssertions, SymmetricDifference,
                MarczewskiSteinhaus, SteelPenny, QuartetDivergence,
                SimilarityToReference)

test_that("Quartets are counted correctly", {
  easyTreesy <- list(
    ape::read.tree(text='((1, 2), ((3, 4), (6, 5)));'),
    ape::read.tree(text='((1, 5), (3, (4, (2, 6))));'))
  expect_identical(c(15L, 2L), QuartetStatus(easyTreesy)[, 's'])
  
  quartet_matches <- QuartetStatus(sq_trees)

  expected_identical <- c(330L, 322L, 278L, 254L, 306L, 252L, 238L, 
                          322L, 207L, 270L, 213L, 244L, 125L, 86L,
                          244L, 122L, 85L, # Values for these three new trees copied without checking
                          104L)
  expected_ambiguous <- c(rep(0L, 7), 8L, 123L, 8L, 65L, 8L, 205L, rep(0L, 5L))
  names(expected_identical) <- names(expected_ambiguous) <- names(sq_trees)
  
  expect_identical(expected_identical, quartet_matches[, 's'])
  expect_identical(expected_ambiguous, quartet_matches[, 'r2'])
  
})

test_that("Quartet metrics are sane", {
  sq_status <- QuartetStatus(sq_trees)
  sims  <- SimilarityMetrics(sq_status)
  dists <- SimilarityMetrics(sq_status, similarity = FALSE)

  expect_true(all(sims <= 1))
  expect_true(all(sims + dists == 1)[-4]) # SSJA doesn't sum to 1
  expect_true(all(dists['ref_tree', ] == 0))
  
  expect_equivalent(sims[, 'DoNotConflict'], DoNotConflict(sq_status))
  expect_equivalent(sims[, 'ExplicitlyAgree'], ExplicitlyAgree(sq_status))
  expect_equivalent(sims[, 'StrictJointAssertions'], StrictJointAssertions(sq_status))
  expect_equivalent(sims[, 'SemiStrictJointAssertions'], SemiStrictJointAssertions(sq_status))
  expect_equivalent(sims[, 'SymmetricDifference'], SymmetricDifference(sq_status))
  expect_equivalent(sims[, 'MarczewskiSteinhaus'], MarczewskiSteinhaus(sq_status))
  expect_equivalent(sims[, 'SteelPenny'], SteelPenny(sq_status))
  expect_equivalent(sims[, 'QuartetDivergence'], QuartetDivergence(sq_status))
  expect_equivalent(sims[, 'SimilarityToReference'], SimilarityToReference(sq_status))
  
  sim6 <- SimilarityMetrics(sq_status[6, ])
  expect_equivalent(sims[6, ], sim6)
  
  testData <- c(N = 16L, Q = 8, s = 1, d = 2, r1 = 1, r2 = 1, u = 3)
  expect_equal(c(tree = 2/8), DoNotConflict(testData, FALSE))
  expect_equal(c(tree = 7/8), ExplicitlyAgree(testData, FALSE))
  expect_equal(c(tree = 2/3), StrictJointAssertions(testData, FALSE))
  expect_equal(c(tree = 2/6), SemiStrictJointAssertions(testData, FALSE))
  expect_equal(c(tree = 6/8), SymmetricDifference(testData, FALSE))
  expect_equal(c(tree = 6/7), MarczewskiSteinhaus(testData, FALSE))
  expect_equal(c(tree = 4/8), SteelPenny(testData, FALSE))
  expect_equal(c(tree = 6/16), QuartetDivergence(testData, FALSE))
  expect_equal(c(tree = 1), SimilarityToReference(testData, FALSE, TRUE))
  expect_equal(c(tree = 0), SimilarityToReference(testData, TRUE, TRUE)) # rounding?
  
  # Metrics should be identical with bifurcating trees.
  treeNodes <- vapply(sq_trees, function (tr) tr$Nnode, double(1))
  n_tip <- 11L
  bifurcators <- treeNodes == n_tip - 1L
  expect_true(all(apply(sims[bifurcators, colnames(sims) != 'MarczewskiSteinhaus'],
                        1, var) < 1e-08))
  
  fncs <- vapply(Metrics, function (X) X(sq_status), double(length(sq_trees)))
  expect_true(all(fncs - sims < 1e-08))
})

test_that('Three-dimensional calculation is correct', {
  testTrees <- sq_trees[11:18]
  test2 <- sq_trees[5:6]
  lapply(Metrics, function (Func) {
    expect_equal(Func(QuartetStatus(testTrees)),
      Func(ManyToManyQuartetAgreement(testTrees))[, 1])
    expect_equal(Func(QuartetStatus(testTrees, test2[[1]])),
      Func(TwoListQuartetAgreement(testTrees, test2))[, 1])
  })
})

test_that("Quartet metrics handle polytomous pairs", {
  polytomous <- list(
    ape::read.tree(text='(A, (B, (C, (D, (E, F, G)))));'),
    ape::read.tree(text='(A, (B, (G, (C, E, F, D))));'),
    ape::read.tree(text='(A, (B, (C, (D, (E, (F, G))))));'),
    ape::read.tree(text='(A, (B, (C, ((D, E), (F, G)))));')
  )
  polyStates <- QuartetStates(polytomous)
  expect_equal(c(rep(3, 19), 0, rep(3, 9), 0, 3, 3, 3, 0, 0), polyStates[1, ])
  expect_equal(c(rep(3, 10), 0, 0, 1, 0, 1, 1, 0, 1, 
                 1, 1, 0, 0, 1, 0, 1, 1, 0, 1, 1, 1, rep(0, 5)), polyStates[2, ])
  
  qStat <- QuartetStatus(polytomous)
  expect_identical(qStat[1, ], c(N=70L, Q=35L, s=31L, d=0L, r1=0L, r2=0L, u=4L))
  expect_identical(qStat[2, ], c(N=70L, Q=35L, s=10L, d=10L, r1=2L, r2=11L, u=2L))
  expect_identical(qStat[3, ], c(N=70L, Q=35L, s=31L, d=0L, r1=4L, r2=0L, u=0L))
  expect_identical(qStat[4, ], c(N=70L, Q=35L, s=25L, d=6L, r1=4L, r2=0L, u=0L))
})

test_that("Random trees are 1/3 similar", {
  for (n_tip in c(7, 13, 26)) {
    random_trees <- lapply(rep(n_tip, 50), ape::rtree, tip.label=seq_len(n_tip), br=NULL)
    n_quartets <- choose(n_tip, 4)
    
    sq_matches <- QuartetStatus(random_trees)
    expect_equal(0, sum(sq_matches[, c('r1', 'r2', 'u')]))
    expect_true(t.test(sq_matches[, 's'], mu=n_quartets * 1 / 3)$p.value > 0.01)
    
    tq_distances <- TQDist(random_trees)
    tq_unique <- tq_distances[upper.tri(tq_distances)]
    expect_true(t.test(tq_unique, mu=n_quartets * 2 / 3)$p.value > 0.01)
    expect_equal(tq_distances[1, ], n_quartets - sq_matches[, 's'])
  }
})

test_that("Incomparable trees fail gracefully", {
  # Must have same number of tips
  expect_error(QuartetStatus(list(ref_tree, ape::rtree(6))))
  # Can't do SSJA for partitions
  expect_equal(NA, SemiStrictJointAssertions(SplitStatus(sq_trees)))
})

test_that("Cleanup was successful", {
  expect_identical(character(0), list.files(pattern='~temp.*'))
})

context("Distances: Partition distances")
test_that ("Partitions are counted correctly", {
  p_dist <- SplitStatus(sq_trees)
  unrooted_trees <- lapply(sq_trees, ape::unroot)
  rf_dist <- as.integer(lapply(unrooted_trees, ape::dist.topo, ape::unroot(sq_trees$ref_tree)))
  
  expect_true(all(p_dist[, 's'] + p_dist[, 'd1'] <= p_dist[, 'P2']))
  expect_true(all(p_dist[, 's'] + p_dist[, 'd2'] <= p_dist[, 'P1']))
  
  expect_true(all(rowSums(p_dist[, c('s', 'd1', 'r1')]) == p_dist[, 'P1']))
  expect_true(all(rowSums(p_dist[, c('s', 'd2', 'r2')]) == p_dist[, 'P2']))
  
  expect_equal(rf_dist, as.integer(RobinsonFoulds(p_dist)))
  expect_equal(rf_dist, as.integer(p_dist[, 'N'] - 
                                     RobinsonFoulds(p_dist, similarity = TRUE)))
  expect_equal(sum(p_dist['move_one_mid' , c('r1', 'd1')]),
               sum(p_dist['m1mid_col1'   , c('r1', 'd1')]),
               sum(p_dist['m1mid_colsome', c('r1', 'd1')]))
  expect_equal(1L, sum(p_dist['m1mid_col1'   , c('d2', 'r2')], 
                       -p_dist['m1mid_col1'   , c('d1', 'r1')]))
  expect_equal(3L, sum(p_dist['m1mid_colsome', c('d2', 'r2')],
                       - p_dist['m1mid_colsome', c('d1', 'r1')]))
  
  expect_equal(sum(p_dist['move_two_mid', c('d1', 'r1')]),
               sum(p_dist['m2mid_col1',   c('d1', 'r1')]))
  expect_equal(1L, sum(p_dist['m2mid_col1', c('d2', 'r2')],
                      - p_dist['m2mid_col1', c('d1', 'r1')]))
  expect_equal(5L, sum(p_dist['m2mid_colsome', c('d2', 'r2')], 
                 - p_dist['m2mid_colsome', c('d1', 'r1')]))
  
})

test_that("Incomparable trees fail gracefully", {
  # Must have same number of tips
  expect_error(SplitStatus(list(ref_tree, ape::rtree(6)))) 
})
