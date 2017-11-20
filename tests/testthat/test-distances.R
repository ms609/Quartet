context("Slow quartet distance")
library("ape")

## Note: result will average 1/3 for random trees
## Another note: > zero when n_tip > 6.
## consider ((1, 2), (3, (4, 5))).  Its only 0-scoring neighbours are symmetric with ((1, 5), (3, (2, 4)).
## Now add 6th tip sister to 3 on Tree 1.  There's nowhere to add this to its opposite without fulfilling
## a 4-statment.

### vapply(4:20, function (n_tip) {
###   trees <- lapply(1:1000, function (X) rtree(n_tip, tip.label=seq_len(n_tip), br=NULL))
###   results <- TetraDist(trees)[1, ] / choose(n_tip, 4)
###   c(mean(results[-1]), sd(results[-1]))
### }, double(2))
### 
### [,3]      [,4]      [,5]      [,6]       [,7]
### [1,] 0.3403403 0.3283283 0.3354021 0.3341913 0.3246389 0.3338974 0.33211783
### [2,] 0.4740609 0.2703599 0.1882786 0.1483874 0.1161134 0.1033415 0.08334102
###            [,8]       [,9]      [,10]     [,11]      [,12]      [,13]
### [1,] 0.33222920 0.33670034 0.33342713 0.3304613 0.33139733 0.33110583
### [2,] 0.07861774 0.07389063 0.06364083 0.0544294 0.05323172 0.04608159
###           [,14]      [,15]      [,16]      [,17]
### [1,] 0.33153069 0.33179650 0.33317373 0.33204908
### [2,] 0.04726896 0.04048472 0.04188577 0.03807628


test_that("Quartets are counted correctly", {
  expect_equal(matrix(c(2, 0), 2, 1), MatchingQuartets(list(
  read.tree(text='((1, 2), ((3, 4), (6, 5)));'),
  read.tree(text='((1, 5), (3, (4, (2, 6))));'))
  ))
  
  ref_tree <- ape::read.tree(text="(((1, 2), 3), (((4, 5), 6), ((7, (8, 9)), (10, 11))));")
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
    opposite_tree = ape::read.tree(text="(((1, 11), 3), (((4, 9), 6), ((10, (8, 6)), (5, 7))));")
  )
  n_tip <- 11
  expected_identical <- c(322, 270, 152, 306, 196, 186, 322, 207, 262, 205, 189, 119, 87)
  expected_ambiguous <- c(rep(0, 6), 8, 123, 8, 65, 8, 155, 14)
  
  quartet_matches <- MatchingQuartets(test_trees)
  expect_equal(expected_identical, quartet_matches[1, ])
  expect_equal(expected_ambiguous, quartet_matches[2, ])
})

test_that("Random trees are 1/3 similar", {
  for (n_tip in c(11, 22, 44)) {
    random_trees <- lapply(rep(n_tip, 1000), ape::rtree, tip.label=seq_len(n_tip), br=NULL)
    matches <- MatchingQuartets(random_trees)[1, ]
    expect_true(abs(mean(matches) - (choose(n_tip, 4) / 3)) < 3)
  }
})
