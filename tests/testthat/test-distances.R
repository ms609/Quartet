context("Slow quartet distance")
library("ape")

test_that("Quartets are counted correctly", {
  expect_equal(matrix(c(2, 0), 2, 1), MatchingQuartets(list(
    ape::read.tree(text='((1, 2), ((3, 4), (6, 5)));'),
    ape::read.tree(text='((1, 5), (3, (4, (2, 6))));'))
  ))
  
  set.seed(0)
  ref_tree <- ape::read.tree(text="(((1, 2), 3), (((4, 5), 6), ((7, (8, 9)), (10, 11))));")
  n_tip <- 11
  test_trees <- list (
    ref_tree      = ref_tree,
    move_one_near = ape::read.tree(text="(((2, 3), 1), (((4, 5), 6), ((7, (8, 9)), (10, 11))));"),
    move_one_mid  = ape::read.tree(text="((2, 3), ((((1, 4), 5), 6), ((7, (8, 9)), (10, 11))));"),
    move_one_far  = ape::read.tree(text="((2, 3), (((4, 5), 6), ((7, (8, 9)), (10, (11, 1)))));"),
    move_two_near = ape::read.tree(text="(((1, 2), 3), (((4, 5), 6), ((7, (10, 11)), (8, 9))));"),
    move_two_mid  = ape::read.tree(text="(((1, 2), 3), ((((4, (10, 11)), 5), 6), (7, (8, 9))));"),
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
  quartet_matches <- cbind(ref_tree = c(choose(n_tip, 4), 0), MatchingQuartets(test_trees))
  qb_matches <- quartet_matches[1, bifurcators]
  qb_matches - tq_matches
  
  expect_equal(tq_matches, as.integer(qb_matches))
  
  expected_identical <- c(choose(n_tip, 4), 322, 270, 152, 306, 196, 186, 322, 207, 262, 205, 189, 119, 86, 141)
  expected_ambiguous <- c(rep(0, 7), 8, 123, 8, 65, 8, 155, 0, 0)
  
  expect_equal(expected_identical, as.integer(quartet_matches[1, ]))
  expect_equal(expected_ambiguous, as.integer(quartet_matches[2, ]))
  
})

test_that("Random trees are 1/3 similar", {
  for (n_tip in c(7, 13, 26)) {
    random_trees <- lapply(rep(n_tip, 200), ape::rtree, tip.label=seq_len(n_tip), br=NULL)
    matches <- MatchingQuartets(random_trees)[1, ]
    cat(n_tip, abs(mean(matches) - (choose(n_tip, 4) / 3)) / choose(n_tip, 4))
    expect_true(abs(mean(matches) - (choose(n_tip, 4) / 3)) / choose(n_tip, 4) < 0.01)
 } 
})
