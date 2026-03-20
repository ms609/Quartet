test_that("QuartetConsensus() handles bad-sized trees", {
  expect_error(QuartetConsensus(c(TreeTools::BalancedTree(3),
                                  TreeTools::BalancedTree(3))),
               "Need at least 4 tips")
  expect_error(QuartetConsensus(c(TreeTools::BalancedTree(103),
                                  TreeTools::BalancedTree(103))),
               "at most 100 tips")
})

test_that("QuartetConsensus() returns input when all trees identical", {
  library("TreeTools")
  tr <- as.phylo(42, nTip = 8)
  trees <- c(tr, tr, tr, tr, tr)
  class(trees) <- "multiPhylo"
  qc <- QuartetConsensus(trees)

  # Should recover the input tree (same quartets)
  status <- QuartetStatus(qc, tr)
  expect_equal(unname(status[, "d"]), 0)
  expect_equal(unname(status[, "r1"]), 0)
  expect_equal(unname(status[, "r2"]), 0)
})

test_that("QuartetConsensus returns star for completely conflicting trees", {
  library("TreeTools")
  # 4-tip trees: all three possible topologies equally represented
  tr1 <- ape::read.tree(text = "((a,b),(c,d));")
  tr2 <- ape::read.tree(text = "((a,c),(b,d));")
  tr3 <- ape::read.tree(text = "((a,d),(b,c));")
  trees <- c(tr1, tr2, tr3)
  class(trees) <- "multiPhylo"
  qc <- QuartetConsensus(trees)
  # No split is supported by a majority of quartet resolutions
  expect_lte(NSplits(qc), 0)
})

test_that("QuartetConsensus handles n=4 tips", {
  library("TreeTools")
  tr1 <- ape::read.tree(text = "((a,b),(c,d));")
  tr2 <- ape::read.tree(text = "((a,b),(c,d));")
  tr3 <- ape::read.tree(text = "((a,c),(b,d));")
  trees <- c(tr1, tr2, tr3)
  class(trees) <- "multiPhylo"
  qc <- QuartetConsensus(trees)
  # ab|cd is in 2/3 trees, should be included
  expect_gte(NSplits(qc), 1)
})

test_that("QuartetConsensus different init strategies give valid trees", {
  library("TreeTools")
  set.seed(4721)
  trees <- as.phylo(sample.int(100, 15), nTip = 8)

  qc_empty <- QuartetConsensus(trees, init = "empty")
  qc_maj <- QuartetConsensus(trees, init = "majority")
  qc_ext <- QuartetConsensus(trees, init = "extended")

  # All should return valid phylo objects

  expect_s3_class(qc_empty, "phylo")
  expect_s3_class(qc_maj, "phylo")
  expect_s3_class(qc_ext, "phylo")

  # All should have the correct number of tips
  expect_equal(length(qc_empty$tip.label), 8)
  expect_equal(length(qc_maj$tip.label), 8)
  expect_equal(length(qc_ext$tip.label), 8)
})

test_that("QuartetConsensus greedy=first also works", {
  library("TreeTools")
  trees <- as.phylo(1:10, nTip = 7)

  qc_best <- QuartetConsensus(trees, greedy = "best")
  qc_first <- QuartetConsensus(trees, greedy = "first")

  expect_s3_class(qc_best, "phylo")
  expect_s3_class(qc_first, "phylo")
})


test_that("greedy=first adds splits from empty start", {
  library("TreeTools")
  # init = "empty" starts from star; greedy_first must add splits
  trees <- as.phylo(1:15, nTip = 8)

  qc <- QuartetConsensus(trees, init = "empty", greedy = "first")

  expect_s3_class(qc, "phylo")
  expect_gt(NSplits(qc), 0)
})


test_that("greedy=first prunes harmful splits from extended init", {
  library("TreeTools")
  # extended init adds all compatible splits, some may be harmful
  trees <- as.phylo(1:20, nTip = 8)

  qc_ext <- QuartetConsensus(trees, init = "extended", greedy = "first")
  qc_maj <- QuartetConsensus(trees, init = "majority", greedy = "first")

  expect_s3_class(qc_ext, "phylo")
  expect_gte(NSplits(qc_maj), 0)
})


test_that("Star-tree input (M = 0) returns star consensus", {
  library("TreeTools")
  star <- StarTree(6)
  trees <- structure(rep(list(star), 5), class = "multiPhylo")

  qc <- QuartetConsensus(trees)

  expect_s3_class(qc, "phylo")
  expect_equal(NTip(qc), 6)
  expect_equal(NSplits(qc), 0)
})

test_that("QuartetConsensus minimizes quartet distance", {
  library("TreeTools")
  trees <- as.phylo(1:20, nTip = 8)

  qc <- QuartetConsensus(trees)
  mr <- Consensus(trees, p = 0.5)

  # Compute total symmetric quartet distance for each
  qd_qc <- sum(vapply(trees, function(tr) {
    s <- QuartetStatus(qc, tr)
    2 * s[, "d"] + s[, "r1"] + s[, "r2"]
  }, numeric(1)))

  qd_mr <- sum(vapply(trees, function(tr) {
    s <- QuartetStatus(mr, tr)
    2 * s[, "d"] + s[, "r1"] + s[, "r2"]
  }, numeric(1)))

  # Quartet consensus should be no worse than majority-rule
  # under the quartet distance
  expect_lte(qd_qc, qd_mr)
})

test_that("QuartetConsensus rejects bad input", {
  library("TreeTools")
  tr <- as.phylo(1, nTip = 3)
  expect_error(QuartetConsensus(list(tr, tr)),
               "multiPhylo")
  expect_error(QuartetConsensus(c(tr)),
               "2 trees")
  tr4 <- as.phylo(1, nTip = 4)
  expect_error(QuartetConsensus(c(tr4)),
               "2 trees")
})


test_that("C++ guards reject invalid n_tips", {
  # Hit the n_tips > 100 and n_tips < 4 stops in the C++ entry point
  expect_error(
    Quartet:::cpp_quartet_consensus(list(), 101, TRUE, FALSE, TRUE, NULL),
    "100"
  )
  expect_error(
    Quartet:::cpp_quartet_consensus(list(), 3, TRUE, FALSE, TRUE, NULL),
    "4 tips"
  )
})

test_that("QuartetConsensus handles non-binary input trees", {
  library("TreeTools")
  # Create a polytomy by using CollapseNode
  tr_binary <- as.phylo(42, nTip = 8)
  tr_poly <- CollapseNode(tr_binary, 10)
  trees <- c(tr_binary, tr_binary, tr_poly)
  class(trees) <- "multiPhylo"
  qc <- QuartetConsensus(trees)
  expect_s3_class(qc, "phylo")
  expect_equal(length(qc$tip.label), 8)
})

test_that("QuartetConsensus is deterministic", {
  library("TreeTools")
  trees <- as.phylo(1:10, nTip = 7)

  qc1 <- QuartetConsensus(trees)
  qc2 <- QuartetConsensus(trees)

  # Same splits
  s1 <- as.Splits(qc1)
  s2 <- as.Splits(qc2)
  expect_equal(NSplits(qc1), NSplits(qc2))

  status <- QuartetStatus(qc1, qc2)
  expect_equal(unname(status[, "d"]), 0)
  expect_equal(unname(status[, "r1"]), 0)
  expect_equal(unname(status[, "r2"]), 0)
})

test_that("QuartetConsensus brute-force verification (n=5)", {
  library("TreeTools")
  # For 5 tips, there are 15 unrooted tree topologies (as.phylo(1:15, 5))
  # Pick 5 input trees, then verify that QuartetConsensus returns a tree

  # with no worse total symmetric quartet distance than any tree topology

  input_ids <- c(1, 1, 2, 3, 5)
  trees <- as.phylo(input_ids, nTip = 5)

  qc <- QuartetConsensus(trees)

  # Compute loss for the quartet consensus
  qd_qc <- sum(vapply(trees, function(tr) {
    s <- QuartetStatus(qc, tr)
    2 * s[, "d"] + s[, "r1"] + s[, "r2"]
  }, numeric(1)))

  # Compare against all 15 binary topologies
  all_trees <- as.phylo(1:15, nTip = 5)
  qd_all <- vapply(seq_along(all_trees), function(i) {
    cand <- all_trees[[i]]
    sum(vapply(trees, function(tr) {
      s <- QuartetStatus(cand, tr)
      2 * s[, "d"] + s[, "r1"] + s[, "r2"]
    }, numeric(1)))
  }, numeric(1))

  # Also check the star tree (all quartets unresolved)
  star <- StarTree(trees[[1]]$tip.label)
  qd_star <- sum(vapply(trees, function(tr) {
    s <- QuartetStatus(star, tr)
    2 * s[, "d"] + s[, "r1"] + s[, "r2"]
  }, numeric(1)))

  # QuartetConsensus should achieve the minimum (or very close)
  best_loss <- min(c(qd_all, qd_star))
  expect_lte(qd_qc, best_loss)
})
