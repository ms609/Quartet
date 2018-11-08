context("Quartet distances")

data('sq_trees')
ref_tree <- sq_trees$ref_tree
n_tip <- 11

treeNodes <- vapply(sq_trees, function (tr) tr$Nnode, double(1))
bifurcators <- treeNodes == n_tip - 1L

test_that("Quartets are counted correctly", {
  easyTreesy <- list(
    ape::read.tree(text='((1, 2), ((3, 4), (6, 5)));'),
    ape::read.tree(text='((1, 5), (3, (4, (2, 6))));'))
  expect_identical(c(15, 2), MatchingQuartets(easyTreesy)['s', ])
  
  quartet_matches <- MatchingQuartets(sq_trees)
  qb_matches <- quartet_matches['s', bifurcators]

  tq_distances <- TQDist(treeList <- sq_trees[bifurcators])
  tq_matches <- choose(n_tip, 4) - tq_distances[1, ]
  expect_equal(tq_matches, as.integer(qb_matches))

  expected_identical <- c(330, 322, 278, 254, 306, 252, 238, 322, 207, 270, 213, 244, 125, 86, 104)
  expected_ambiguous <- c(rep(0, 7), 8, 123, 8, 65, 8, 205, 0, 0)
  
  expect_equal(expected_identical, as.integer(quartet_matches['s', ]))
  expect_equal(expected_ambiguous, as.integer(quartet_matches['r2', ]))
  
})

test_that("Quartet metrics are sane", {
  sims <- QuartetMetrics(sq_trees) 
  dists <- QuartetMetrics(sq_trees, similarity=FALSE)
  expect_true(all(sims <= 1))
  expect_true(all(sims + dists == 1))
  expect_true(all(dists['ref_tree', ] == 0))
  
  # Metrics should be identical with bifurcating trees.
  expect_true(all(apply(sims[bifurcators, ], 1, var) < 1e-08))
  
  mq <- MatchingQuartets(sq_trees)
  fncs <- vapply(list(DoNotConflict, ExplicitlyAgree, StrictJointAssertions, SemiStrictJointAssertions, QuartetDivergence), function (X) X(mq), double(length(sq_trees)))
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
  
  mq <- MatchingQuartets(polytomous)
  expect_equal(c(Q=35, s=31, d=0, r1=0, r2=0, u=4), mq[, 1])
  expect_equal(c(Q=35, s=10, d=10, r1=2, r2=11, u=2), mq[, 2])
  expect_equal(c(Q=35, s=31, d=0, r1=4, r2=0, u=0), mq[, 3])
  expect_equal(c(Q=35, s=25, d=6, r1=4, r2=0, u=0), mq[, 4])
})

test_that ("Partitions are counted correctly", {
  p_dist <- MatchingSplits(sq_trees)
  unrooted_trees <- lapply(sq_trees, ape::unroot)
  rf_dist <- as.integer(lapply(unrooted_trees, ape::dist.topo, ape::unroot(ref_tree)))
  
  expect_true(all(p_dist['cf_and_ref', ] + p_dist['cf_not_ref', ] <= p_dist['cf', ]))
  expect_true(all(p_dist['cf_and_ref', ] + p_dist['ref_not_cf', ] <= p_dist['ref', ]))
  expect_equal(rf_dist, as.integer(p_dist['RF_dist', ]))
  expect_equal(p_dist['cf_not_ref', 'move_one_mid'], p_dist['cf_not_ref', 'm1mid_col1'], p_dist['cf_not_ref', 'm1mid_colsome'])
  expect_equal(1, p_dist['ref_not_cf', 'm1mid_col1'] - p_dist['cf_not_ref', 'm1mid_col1'])
  expect_equal(3, p_dist['ref_not_cf', 'm1mid_colsome'] - p_dist['cf_not_ref', 'm1mid_colsome'])
  
  expect_equal(p_dist['cf_not_ref', 'move_two_mid'], p_dist['cf_not_ref', 'm2mid_col1'])
  expect_equal(1, p_dist['ref_not_cf', 'm2mid_col1']    - p_dist['cf_not_ref', 'm2mid_col1'])
  expect_equal(5, p_dist['ref_not_cf', 'm2mid_colsome'] - p_dist['cf_not_ref', 'm2mid_colsome'])
  
})

test_that("Incomparable trees fail gracefully", {
  # Must have same number of tips
  expect_error(MatchingSplits(list(ref_tree, ape::rtree(6)))) 
  expect_error(MatchingQuartets(list(ref_tree, ape::rtree(6))))
})

test_that("Random trees are 1/3 similar", {
  for (n_tip in c(7, 13, 26)) {
    random_trees <- lapply(rep(n_tip, 50), ape::rtree, tip.label=seq_len(n_tip), br=NULL)
    n_quartets <- choose(n_tip, 4)
    
    sq_matches <- MatchingQuartets(random_trees)
    expect_equal(0, sum(sq_matches[c('r1', 'r2', 'u'), ]))
    expect_true(t.test(sq_matches['s', ], mu=n_quartets * 1 / 3)$p.value > 0.01)
    
    tq_distances <- TQDist(random_trees)
    tq_unique <- tq_distances[upper.tri(tq_distances)]
    expect_true(t.test(tq_unique,       mu=n_quartets * 2 / 3)$p.value > 0.01)
    expect_equal(tq_distances[1, ], n_quartets - sq_matches['s', ])
  } 
})
