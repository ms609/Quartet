context("Slow quartet distance")
library("ape")

set.seed(1)
ref_tree <- ape::read.tree(text="(((1, 2), 3), (((4, 5), 6), ((7, (8, 9)), (10, 11))));")
n_tip <- 11
test_trees <- list (
  ref_tree      = ref_tree,
  move_one_near = ape::read.tree(text="(((2, 3), 1), (((4, 5), 6), ((7, (8, 9)), (10, 11))));"),
  move_one_mid  = ape::read.tree(text="((2, 3), ((((4, 5), 1), 6), ((7, (8, 9)), (10, 11))));"),
  move_one_far  = ape::read.tree(text="((2, 3), (((4, 5), 6), ((7, (8, 9)), (10, (11, 1)))));"),
  move_two_near = ape::read.tree(text="(((1, 2), 3), (((4, 5), 6), ((7, (10, 11)), (8, 9))));"),
  move_two_mid  = ape::read.tree(text="(((1, 2), 3), ((((4, 5), (10, 11)), 6), (7, (8, 9))));"),
  move_two_far  = ape::read.tree(text="((((1, (10, 11)), 2), 3), (((4, 5), 6), (7, (8, 9))));"),
  collapse_one  = ape::read.tree(text="(((1, 2), 3), (((4, 5), 6), ((7, 8, 9), (10, 11))));"),
  collapse_some = ape::read.tree(text="((1, 2, 3, 4, 5, 6), ((7, 8, 9), (10, 11)));"),
  m1mid_col1    = ape::read.tree(text="((2, 3), ((((4, 5), 1), 6), ((7, 8, 9), (10, 11))));"),
  m1mid_colsome = ape::read.tree(text="((2, 3), ((((4, 5), 1), 6), (7, 8, 9, 10, 11)));"),
  m2mid_col1    = ape::read.tree(text="(((1, 2), 3), ((((4, 5), (10, 11)), 6), (7, 8, 9)));"),
  m2mid_colsome = ape::read.tree(text="(((1, 2), 3), (4, (10, 11), 5, 6, 7, 8, 9));"),
  opposite_tree = ape::read.tree(text="(((1, 11), 3), (((4, 9), 6), ((10, (8, 2)), (5, 7))));"),
  random_tree   = ape::rtree(n_tip, tip.label=seq_len(n_tip), br=NULL)
)
treeNodes <- vapply(test_trees, function (tr) tr$Nnode, double(1))
bifurcators <- treeNodes == n_tip - 1L


test_that("Quartets are counted correctly", {
  expect_identical(as.integer(rbind(c(15, 2), c(0, 0))), as.integer(MatchingQuartets(list(
    ape::read.tree(text='((1, 2), ((3, 4), (6, 5)));'),
    ape::read.tree(text='((1, 5), (3, (4, (2, 6))));'))
  )))
  
  quartet_matches <- MatchingQuartets(test_trees)
  qb_matches <- quartet_matches[1, bifurcators]
  
  if ('rtqdist' %in% installed.packages()[, 'Package']) {
    tq_distances <- TQDist(treeList <- test_trees[bifurcators])
    tq_matches <- choose(n_tip, 4) - tq_distances[1, ]
    expect_equal(tq_matches, as.integer(qb_matches))
  }
  
  expected_identical <- c(330, 322, 278, 254, 306, 252, 238, 322, 207, 270, 213, 244, 125, 86, 104)
  expected_ambiguous <- c(rep(0, 7), 8, 123, 8, 65, 8, 205, 0, 0)
  
  expect_equal(expected_identical, as.integer(quartet_matches[1, ]))
  expect_equal(expected_ambiguous, as.integer(quartet_matches[2, ]))
  
})

test_that ("Partitions are counted correctly", {
  p_dist <- MatchingSplits(test_trees)
  unrooted_trees <- lapply(test_trees, ape::unroot)
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
    expect_equal(rep(0, length(random_trees)), sq_matches[2, ])
    expect_true(t.test(sq_matches[1, ], mu=n_quartets * 1 / 3)$p.value > 0.01)
    
    if ('rtqdist' %in% installed.packages()[, 'Package']) {
      tq_distances <- TQDist(random_trees)
      tq_unique <- tq_distances[upper.tri(tq_distances)]
      expect_true(t.test(tq_unique,       mu=n_quartets * 2 / 3)$p.value > 0.01)
      expect_equal(tq_distances[1, ], n_quartets - sq_matches[1, ])
    }
  } 
})