context("Slow quartet distance")
library("ape")

data('sq_trees')
ref_tree <- sq_trees$ref_tree
n_tip <- 11

treeNodes <- vapply(sq_trees, function (tr) tr$Nnode, double(1))
bifurcators <- treeNodes == n_tip - 1L


test_that("Quartets are counted correctly", {
  expect_identical(as.integer(rbind(c(15, 2), c(0, 0))), as.integer(MatchingQuartets(list(
    ape::read.tree(text='((1, 2), ((3, 4), (6, 5)));'),
    ape::read.tree(text='((1, 5), (3, (4, (2, 6))));'))
  )))
  
  quartet_matches <- MatchingQuartets(sq_trees)
  qb_matches <- quartet_matches['s', bifurcators]
  
  if ('rtqdist' %in% installed.packages()[, 'Package']) {
    tq_distances <- TQDist(treeList <- sq_trees[bifurcators])
    tq_matches <- choose(n_tip, 4) - tq_distances[1, ]
    expect_equal(tq_matches, as.integer(qb_matches))
  }
  
  expected_identical <- c(330, 322, 278, 254, 306, 252, 238, 322, 207, 270, 213, 244, 125, 86, 104)
  expected_ambiguous <- c(rep(0, 7), 8, 123, 8, 65, 8, 205, 0, 0)
  
  expect_equal(expected_identical, as.integer(quartet_matches['s', ]))
  expect_equal(expected_ambiguous, as.integer(quartet_matches['r2', ]))
  
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
    
    sq_matches <- MatchingQuartets(random_trees, use.tqDist=FALSE)
    expect_equal(0, sum(sq_matches[c('r1', 'r2', 'u'), ]))
    expect_true(t.test(sq_matches['s', ], mu=n_quartets * 1 / 3)$p.value > 0.01)
    
    if ('rtqdist' %in% installed.packages()[, 'Package']) {
      tq_distances <- TQDist(random_trees)
      tq_unique <- tq_distances[upper.tri(tq_distances)]
      expect_true(t.test(tq_unique,       mu=n_quartets * 2 / 3)$p.value > 0.01)
      expect_equal(tq_distances[1, ], n_quartets - sq_matches['s', ])
    }
  } 
})
