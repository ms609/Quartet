test_that("neverDrop = TRUE gives same result as default", {
  library(TreeTools)
  trees <- as.phylo(1:20, nTip = 8)

  qc1 <- QuartetConsensus(trees)
  qc2 <- QuartetConsensus(trees, neverDrop = TRUE)

  expect_equal(NSplits(qc1), NSplits(qc2))
  expect_null(attr(qc2, "dropped"))
})


test_that("No drops for identical trees", {
  library(TreeTools)
  tree <- BalancedTree(8)
  trees <- structure(rep(list(tree), 10), class = "multiPhylo")

  qc <- QuartetConsensus(trees, neverDrop = FALSE)

  expect_equal(NTip(qc), 8)
  expect_equal(length(attr(qc, "dropped")), 0)
})


test_that("Single rogue is detected and dropped", {
  library(TreeTools)

  base <- BalancedTree(8)
  trees <- AddTipEverywhere(base, "rogue")
  class(trees) <- "multiPhylo"

  qc <- QuartetConsensus(trees, neverDrop = FALSE)

  dropped <- attr(qc, "dropped")
  expect_true("rogue" %in% dropped)
  expect_false("rogue" %in% qc$tip.label)

  # All base tips should survive
  for (tip in paste0("t", 1:8)) {
    expect_true(tip %in% qc$tip.label)
  }
})


test_that("neverDrop character vector protects tips", {
  library(TreeTools)

  base <- BalancedTree(8)
  trees <- AddTipEverywhere(base, "rogue")
  class(trees) <- "multiPhylo"

  qc <- QuartetConsensus(trees, neverDrop = "rogue")

  dropped <- attr(qc, "dropped")
  expect_false("rogue" %in% dropped)
})


test_that("neverDrop = FALSE with small tree works", {
  library(TreeTools)

  base <- BalancedTree(6)
  trees <- AddTipEverywhere(base, "rogue")
  class(trees) <- "multiPhylo"

  qc <- QuartetConsensus(trees, neverDrop = FALSE)

  expect_true("rogue" %in% attr(qc, "dropped"))
  expect_equal(NTip(qc), 6)
})


test_that("Invalid neverDrop labels produce an error", {
  library(TreeTools)
  trees <- as.phylo(1:10, nTip = 8)

  expect_error(
    QuartetConsensus(trees, neverDrop = "nonexistent"),
    "not found"
  )
})


test_that("Output tree has correct structure after dropping", {
  library(TreeTools)

  base <- BalancedTree(8)
  trees <- AddTipEverywhere(base, "rogue")
  class(trees) <- "multiPhylo"

  qc <- QuartetConsensus(trees, neverDrop = FALSE)

  expect_s3_class(qc, "phylo")
  expect_true(NTip(qc) < 9)  # at least the rogue was dropped
  expect_true(NSplits(qc) > 0)  # tree should be resolved
})


test_that("Two rogues are dropped without spurious extra drops", {
  library(TreeTools)

  base <- BalancedTree(8)
  base_tips <- paste0("t", 1:8)

  # Every combination of rogue1 + rogue2 attachment positions
  intermediate <- AddTipEverywhere(base, "rogue1")
  trees <- do.call(c, lapply(intermediate, function(tr) {
    AddTipEverywhere(tr, "rogue2")
  }))
  class(trees) <- "multiPhylo"

  qc <- QuartetConsensus(trees, neverDrop = FALSE)

  dropped <- attr(qc, "dropped")
  expect_true("rogue1" %in% dropped)
  expect_true("rogue2" %in% dropped)
  # No base tips should be dropped (regression: FP drift caused extra drops)
  expect_true(all(base_tips %in% qc$tip.label))
  expect_equal(NTip(qc), 8)
})
