context("Slow quartet distance")
library("ape")

test_that("Quartets are counted correctly", {
  expect_identical(as.integer(rbind(c(15, 2), c(0, 0))), as.integer(MatchingQuartets(list(
    ape::read.tree(text='((1, 2), ((3, 4), (6, 5)));'),
    ape::read.tree(text='((1, 5), (3, (4, (2, 6))));'))
  )))
  
  set.seed(0)
  ref_tree <- ape::read.tree(text="(((1, 2), 3), (((4, 5), 6), ((7, (8, 9)), (10, 11))));")
  n_tip <- 11
  test_trees <- list (
    ref_tree      = ref_tree,
    move_one_near = ape::read.tree(text="(((2, 3), 1), (((4, 5), 6), ((7, (8, 9)), (10, 11))));"),
    move_one_mid  = ape::read.tree(text="((2, 3), ((((1, 4), 5), 6), ((7, (8, 9)), (10, 11))));"),
    move_one_far  = ape::read.tree(text="((2, 3), (((4, 5), 6), ((7, (8, 9)), (10, (11, 1)))));"),
    move_two_near = ape::read.tree(text="(((1, 2), 3), (((4, 5), 6), ((7, (10, 11)), (8, 9))));"),
    move_two_mid  = ape::read.tree(text="(((1, 2), 3), ((((4, 5), (10, 11)), 6), (7, (8, 9))));"),
    move_two_far  = ape::read.tree(text="((((1, (10, 11)), 2), 3), (((4, 5), 6), (7, (8, 9))));"),
    collapse_one  = ape::read.tree(text="(((1, 2), 3), (((4, 5), 6), ((7, 8, 9), (10, 11))));"),
    collapse_some = ape::read.tree(text="((1, 2, 3, 4, 5, 6), ((7, 8, 9), (10, 11)));"),
    m1mid_col1    = ape::read.tree(text="((2, 3), ((((1, 4), 5), 6), ((7, 8, 9), (10, 11))));"),
    m1mid_colsome = ape::read.tree(text="((2, 3), ((((1, 4), 5), 6), (7, 8, 9, 10, 11)));"),
    m2mid_col1    = ape::read.tree(text="(((1, 2), 3), ((((4, (10, 11)), 5), 6), (7, 8, 9)));"),
    m2mid_colsome = ape::read.tree(text="(((1, 2), 3), ((4, (10, 11)), 5, 6, 7, 8, 9));"),
    opposite_tree = ape::read.tree(text="(((1, 11), 3), (((4, 9), 6), ((10, (8, 2)), (5, 7))));"),
    random_tree   = ape::rtree(n=n_tip, tip.label=seq_len(n_tip), br=NULL)
  )
  treeNodes <- vapply(test_trees, function (tr) tr$Nnode, double(1))
  bifurcators <- treeNodes == n_tip - 1L
  tq_distances <- TQDist(treeList <- test_trees[bifurcators])
  tq_matches <- choose(n_tip, 4) - tq_distances[1, ]
  quartet_matches <- MatchingQuartets(test_trees)
  qb_matches <- quartet_matches[1, bifurcators]
  
  expect_equal(tq_matches, as.integer(qb_matches))
  
  expected_identical <- c(330, 322, 270, 254, 306, 238, 238, 322, 207, 262, 205, 230, 131, 86, 157)
  expected_ambiguous <- c(rep(0, 7), 8, 123, 8, 65, 8, 155, 0, 0)
  
  expect_equal(expected_identical, as.integer(quartet_matches[1, ]))
  expect_equal(expected_ambiguous, as.integer(quartet_matches[2, ]))
  
})

test_that("Random trees are 1/3 similar", {
  for (n_tip in c(7, 13, 26)) {
    random_trees <- lapply(rep(n_tip, 50), ape::rtree, tip.label=seq_len(n_tip), br=NULL)
    
    tq_distances <- TQDist(random_trees)
    tq_unique <- tq_distances[upper.tri(tq_distances)]
    tq_mean <- mean(tq_unique)
    tq_sd <- sd(tq_unique)
    
    sq_matches <- MatchingQuartets(random_trees, use.tqDist=FALSE)
    expect_equal(rep(0, length(random_trees)), sq_matches[2, ])
    sq_mean <- mean(sq_matches[1, -1])
    sq_sd   <-   sd(sq_matches[1, -1])
    
    n_quartets <- choose(n_tip, 4)
    
    expect_equal(tq_distances[1, ], n_quartets - sq_matches[1, ])
    cat("\n", n_tip, "tips: expect", n_quartets, "=", n_quartets * 2 / 3, "+", n_quartets / 3,
        "\n          TQ", tq_mean, "+-", tq_sd, "| SQ:", sq_mean, "+-", sq_sd)
    expect_true(abs((n_quartets * 1 / 3) - sq_mean) < (2 * sq_sd))
    expect_true(abs((n_quartets * 2 / 3) - tq_mean) < (2 * tq_sd))
  } 
})