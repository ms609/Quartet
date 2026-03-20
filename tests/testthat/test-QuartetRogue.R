test_that("QuartetRogue detects a known rogue taxon", {
  library(TreeTools)
  set.seed(5891)

  base <- BalancedTree(8)
  trees <- AddTipEverywhere(base, "rogue")
  trees <- trees[sample(length(trees), min(8, length(trees)))]

  result <- QuartetRogue(trees)

  expect_s3_class(result, "data.frame")
  expect_named(result, c("num", "taxon", "rawImprovement", "score"))
  expect_equal(result$num[1], 0)
  expect_true(is.na(result$taxon[1]))

  # The rogue taxon should be dropped
  expect_true("rogue" %in% result$taxon)

  # Score should improve after dropping the rogue
  expect_true(result$score[2] > result$score[1])
})


test_that("QuartetRogue returns no drops when trees agree perfectly", {
  library(TreeTools)
  tree <- BalancedTree(8)
  trees <- structure(rep(list(tree), 5), class = "multiPhylo")

  result <- QuartetRogue(trees)

  # Only the baseline row (no drops)
  expect_equal(nrow(result), 1)
  expect_equal(result$num, 0)
  expect_equal(result$score, 1)
})


test_that("neverDrop is respected", {
  library(TreeTools)
  set.seed(7204)

  base <- BalancedTree(8)
  trees <- AddTipEverywhere(base, "rogue")
  trees <- trees[sample(length(trees), min(8, length(trees)))]

  # Protect the rogue — it should not be dropped
  result <- QuartetRogue(trees, neverDrop = "rogue")

  if (nrow(result) > 1) {
    expect_false("rogue" %in% result$taxon)
  }
})


test_that("Score is non-decreasing through the drop sequence", {
  library(TreeTools)
  set.seed(3156)

  # Use AddTipEverywhere for both rogues to ensure consistent tip labels
  base <- PectinateTree(8)
  treesR1 <- AddTipEverywhere(base, "rogue1")
  # For each tree with rogue1, add rogue2 at a random position
  trees <- lapply(treesR1, function(tr) {
    candidates <- AddTipEverywhere(tr, "rogue2")
    candidates[[sample(length(candidates), 1)]]
  })
  class(trees) <- "multiPhylo"
  trees <- trees[sample(length(trees), min(8, length(trees)))]

  result <- QuartetRogue(trees)

  # Scores should be non-decreasing (each drop improves or is neutral)
  for (i in seq_len(nrow(result) - 1L)) {
    expect_true(result$score[i + 1] >= result$score[i] - 1e-10,
                info = paste("Score dropped at step", i))
  }
})


test_that("return = 'tree' gives a phylo object", {
  library(TreeTools)
  set.seed(8412)

  base <- BalancedTree(8)
  trees <- AddTipEverywhere(base, "rogue")[3:6]

  result <- QuartetRogue(trees, return = "tree")

  expect_s3_class(result, "phylo")
  expect_false("rogue" %in% result$tip.label)
})


test_that("QuartetRogue refuses fewer than 5 tips", {
  library(TreeTools)
  tree <- BalancedTree(4)
  trees <- structure(list(tree, tree), class = "multiPhylo")
  expect_error(QuartetRogue(trees), "5 tips")
})


test_that("QuartetRogue refuses fewer than 2 trees", {
  library(TreeTools)
  tree <- BalancedTree(8)
  expect_error(QuartetRogue(list(tree)), "2 trees")
})


test_that(".QuartetRogueScore is 1 for identical trees", {
  library(TreeTools)
  tree <- BalancedTree(8)
  trees <- structure(rep(list(tree), 5), class = "multiPhylo")

  score <- .QuartetRogueScore(trees)
  expect_equal(score, 1)
})


test_that(".QuartetRogueScore is finite for random trees", {
  library(TreeTools)
  set.seed(6734)
  trees <- structure(
    lapply(1:10, function(i) RandomTree(8)),
    class = "multiPhylo"
  )

  score <- .QuartetRogueScore(trees)
  expect_true(is.finite(score))
  # Should be near 0 (random trees) but may be slightly negative due to
  # normalization artefacts
  expect_true(score > -0.5 && score <= 1)
})


test_that("maxDrop limits the number of drops", {
  library(TreeTools)
  set.seed(2918)

  base <- BalancedTree(8)
  trees <- AddTipEverywhere(base, "rogue1")
  # Add a second rogue
  trees <- lapply(trees, function(tr) {
    candidates <- AddTipEverywhere(tr, "rogue2")
    candidates[[sample(length(candidates), 1)]]
  })
  class(trees) <- "multiPhylo"
  trees <- trees[sample(length(trees), min(6, length(trees)))]

  result <- QuartetRogue(trees, maxDrop = 1)

  # At most 1 taxon dropped (plus the baseline row)
  expect_lte(nrow(result), 2)
})
