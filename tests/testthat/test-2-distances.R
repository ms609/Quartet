context("Quartet distances")

data('sq_trees')
ref_tree <- sq_trees$ref_tree

test_that("Quartets are counted correctly", {
  easyTreesy <- list(
    ape::read.tree(text='((1, 2), ((3, 4), (6, 5)));'),
    ape::read.tree(text='((1, 5), (3, (4, (2, 6))));'))
  expect_identical(c(15L, 2L), QuartetStatus(easyTreesy)[, 's'])
  
  quartet_matches <- QuartetStatus(sq_trees)

  expected_identical <- c(330L, 322L, 278L, 254L, 306L, 252L, 238L, 
                          322L, 207L, 270L, 213L, 244L, 125L, 86L, 104L)
  expected_ambiguous <- c(rep(0L, 7), 8L, 123L, 8L, 65L, 8L, 205L, 0L, 0L)
  names(expected_identical) <- names(expected_ambiguous) <- names(sq_trees)
  
  expect_identical(expected_identical, quartet_matches[, 's'])
  expect_identical(expected_ambiguous, quartet_matches[, 'r2'])
  
})

test_that("Quartet metrics are sane", {
  sq_status <- QuartetStatus(sq_trees)
  sims  <- QuartetMetrics(sq_status)
  dists <- QuartetMetrics(sq_status, similarity=FALSE)
  expect_true(all(sims <= 1))
  expect_true(all(sims + dists == 1))
  expect_true(all(dists['ref_tree', ] == 0))
  
  expect_equal(sims[, 'DoNotConflict'], as.double(DoNotConflict(sq_status)))
  expect_equal(sims[, 'ExplicitlyAgree'], as.double(ExplicitlyAgree(sq_status)))
  expect_equal(sims[, 'StrictJointAssertions'], as.double(StrictJointAssertions(sq_status)))
  expect_equal(sims[, 'SemiStrictJointAssertions'], as.double(SemiStrictJointAssertions(sq_status)))
  expect_equal(sims[, 'QuartetDivergence'], as.double(QuartetDivergence(sq_status)))
  
  # Metrics should be identical with bifurcating trees.
  treeNodes <- vapply(sq_trees, function (tr) tr$Nnode, double(1))
  n_tip <- 11L
  bifurcators <- treeNodes == n_tip - 1L
  expect_true(all(apply(sims[bifurcators, ], 1, var) < 1e-08))
  
  qStat <- QuartetStatus(sq_trees)
  fncs <- vapply(list(DoNotConflict, ExplicitlyAgree, StrictJointAssertions,
                      SemiStrictJointAssertions, QuartetDivergence),
                 function (X) X(qStat), double(length(sq_trees)))
  expect_true(all(fncs - sims < 1e-08))
})

test_that("Quartet metrics handle polytomous pairs", {
  polytomous <- list(
    ape::read.tree(text='(A, (B, (C, (D, (E, F, G)))));'),
    ape::read.tree(text='(A, (B, (G, (C, E, F, D))));'),
    ape::read.tree(text='(A, (B, (C, (D, (E, (F, G))))));'),
    ape::read.tree(text='(A, (B, (C, ((D, E), (F, G)))));')
  )
  polyStates <- QuartetStates(polytomous)
  expect_equal(c(rep(2, 19), 0, rep(2, 9), 0, 2, 2, 2, 0, 0), polyStates[[1]])
  expect_equal(c(rep(2, 10), 0, 0, 4, 0, 4, 4, 0, 4, 
                 4, 4, 0, 0, 4, 0, 4, 4, 0, 4, 4, 4, rep(0, 5)), polyStates[[2]])
  
  qStat <- QuartetStatus(polytomous)
  expect_identical(qStat[1, ], c(Q=35L, s=31L, d=0L, r1=0L, r2=0L, u=4L))
  expect_identical(qStat[2, ], c(Q=35L, s=10L, d=10L, r1=2L, r2=11L, u=2L))
  expect_identical(qStat[3, ], c(Q=35L, s=31L, d=0L, r1=4L, r2=0L, u=0L))
  expect_identical(qStat[4, ], c(Q=35L, s=25L, d=6L, r1=4L, r2=0L, u=0L))
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
})

test_that("Cleanup was successful", {
  expect_identical(character(0), list.files(pattern='~temp.*'))
})

context("Partition distances")
test_that ("Partitions are counted correctly", {
  p_dist <- SplitStatus(sq_trees)
  unrooted_trees <- lapply(sq_trees, ape::unroot)
  rf_dist <- as.integer(lapply(unrooted_trees, ape::dist.topo, ape::unroot(ref_tree)))
  
  expect_true(all(p_dist[, 'cf_and_ref'] + p_dist[, 'cf_not_ref'] <= p_dist[, 'cf']))
  expect_true(all(p_dist[, 'cf_and_ref'] + p_dist[, 'ref_not_cf'] <= p_dist[, 'ref']))
  expect_equal(rf_dist, as.integer(p_dist[, 'RF_dist']))
  expect_equal(p_dist['move_one_mid' , 'cf_not_ref'],
               p_dist['m1mid_col1'   , 'cf_not_ref'],
               p_dist['m1mid_colsome', 'cf_not_ref'])
  expect_equal(1L, p_dist['m1mid_col1'   , 'ref_not_cf'] - p_dist['m1mid_col1'   , 'cf_not_ref'])
  expect_equal(3L, p_dist['m1mid_colsome', 'ref_not_cf'] - p_dist['m1mid_colsome', 'cf_not_ref'])
  
  expect_equal(p_dist['move_two_mid', 'cf_not_ref'],
               p_dist['m2mid_col1',   'cf_not_ref'])
  expect_equal(1L, p_dist['m2mid_col1', 'ref_not_cf']
                 - p_dist['m2mid_col1', 'cf_not_ref'])
  expect_equal(5L, p_dist['m2mid_colsome', 'ref_not_cf'] 
                 - p_dist['m2mid_colsome', 'cf_not_ref'])
  
})

test_that("Incomparable trees fail gracefully", {
  # Must have same number of tips
  expect_error(SplitStatus(list(ref_tree, ape::rtree(6)))) 
})

