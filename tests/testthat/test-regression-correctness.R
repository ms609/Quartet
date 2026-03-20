# Correctness regression corpus — T-001
#
# Purpose: Guard exact quartet distance correctness against optimisation
# regressions.  These tests must pass on the UNMODIFIED package baseline, and
# must continue to pass after every subsequent change.
#
# All invariants are derived from first principles or hand-verified by
# independent calculation; they do not simply round-trip through the code.
#
# Run with:
#   R CMD INSTALL --library=.agent-X .
#   Rscript -e "library(Quartet, lib.loc='.agent-X');
#               testthat::test_file('tests/testthat/test-regression-correctness.R')"

library("TreeTools")

# ---------------------------------------------------------------------------
# Helper: extract (s, d, r1, r2, u) columns from SingleTreeQuartetAgreement
# for the first (and only) query tree.
sdrru <- function(tree, comparison) {
  res <- SingleTreeQuartetAgreement(list(tree), comparison)[1L, ]
  res[c("s", "d", "r1", "r2", "u")]
}

# Helper: total resolved quartets C(n, 4) for n tips
q4 <- function(n) choose(n, 4L)

TreePath <- function(fileName) {
  system.file("trees", paste0(fileName, ".new"), package = "Quartet")
}

# ===========================================================================
# 1. HAND-VERIFIED 4-TIP TREES
#    For n=4 there is exactly 1 quartet.  The three resolved topologies are
#    12|34, 13|24, and 14|23.  A star has 0 resolved quartets.
# ===========================================================================

test_that("4-tip hand-verified quartet agreement values are correct", {

  t_12_34 <- ape::read.tree(text = "((t1,t2),(t3,t4));") # resolved: {t1,t2}|{t3,t4}
  t_13_24 <- ape::read.tree(text = "((t1,t3),(t2,t4));") # resolved: {t1,t3}|{t2,t4}
  t_14_23 <- ape::read.tree(text = "((t1,t4),(t2,t3));") # resolved: {t1,t4}|{t2,t3}
  t_star4 <- ape::read.tree(text = "(t1,t2,t3,t4);")    # unresolved

  # Same tree: all agreed (s=1)
  expect_equal(sdrru(t_12_34, t_12_34), c(s = 1, d = 0, r1 = 0, r2 = 0, u = 0))
  expect_equal(sdrru(t_star4, t_star4), c(s = 0, d = 0, r1 = 0, r2 = 0, u = 1))

  # Different resolved topology: explicitly disagree (d=1)
  expect_equal(sdrru(t_12_34, t_13_24), c(s = 0, d = 1, r1 = 0, r2 = 0, u = 0))
  expect_equal(sdrru(t_12_34, t_14_23), c(s = 0, d = 1, r1 = 0, r2 = 0, u = 0))
  expect_equal(sdrru(t_13_24, t_14_23), c(s = 0, d = 1, r1 = 0, r2 = 0, u = 0))

  # Resolved tree vs star: unresolved in comparison (r1=1), no disagreement
  expect_equal(sdrru(t_12_34, t_star4), c(s = 0, d = 0, r1 = 1, r2 = 0, u = 0))
  expect_equal(sdrru(t_13_24, t_star4), c(s = 0, d = 0, r1 = 1, r2 = 0, u = 0))

  # Star vs resolved: unresolved in query (r2=1), no disagreement
  expect_equal(sdrru(t_star4, t_12_34), c(s = 0, d = 0, r1 = 0, r2 = 1, u = 0))

  # Q = 1 in all cases
  results <- SingleTreeQuartetAgreement(list(t_12_34, t_13_24, t_star4), t_12_34)
  expect_true(all(results[, "Q"] == 1L))
})

test_that("4-tip QuartetDistance matches hand-verified values", {
  t_12_34 <- ape::read.tree(text = "((t1,t2),(t3,t4));")
  t_13_24 <- ape::read.tree(text = "((t1,t3),(t2,t4));")
  t_star4 <- ape::read.tree(text = "(t1,t2,t3,t4);")

  trees <- structure(list(t_12_34, t_13_24, t_star4), class = "multiPhylo")
  d <- TQDist(trees)

  # Diagonal: always zero
  expect_equal(d[1, 1], 0L)
  expect_equal(d[2, 2], 0L)
  expect_equal(d[3, 3], 0L)

  # 12|34 vs 13|24: disagreed → distance = 1
  expect_equal(d[1, 2], 1L)
  expect_equal(d[2, 1], 1L)  # symmetry

  # Any resolved vs star: A=0, E=0 (star resolves nothing, so no pair is
  # "unresolved in both"), distance = C(n,4) - 0 - 0 = 1 for n=4
  expect_equal(d[1, 3], 1L)
  expect_equal(d[3, 1], 1L)
  expect_equal(d[2, 3], 1L)
})

# ===========================================================================
# 2. KNOWN VALUES FROM REFERENCE TREE FILES
#    These reproduce values established in test-1-tqdist.R for the file-based
#    API.  They are included here as an independent cross-check.
# ===========================================================================

test_that("Regression: known file-based distances still hold", {
  skip_if_not(file.exists(TreePath("quartet1")))

  # 4-tip trees
  expect_equal(QuartetDistance(TreePath("quartet1"), TreePath("quartet1")), 0L)
  expect_equal(QuartetDistance(TreePath("quartet1"), TreePath("quartet2")), 1L)
  expect_equal(QuartetDistance(TreePath("quartet2"), TreePath("quartet1")), 1L)

  # Medium-sized trees
  expect_equal(QuartetDistance(TreePath("test_tree1"), TreePath("test_tree2")), 26L)
  expect_equal(QuartetDistance(TreePath("test_tree2"), TreePath("test_tree1")), 26L)

  # Large-value test (important: catches integer overflow or counter errors)
  expect_equal(QuartetDistance(TreePath("test_tree3"), TreePath("test_tree4")), 5485860L)
  expect_equal(QuartetDistance(TreePath("test_tree4"), TreePath("test_tree3")), 5485860L)

  # Unresolved trees: A=3, E=0
  expect_equal(QuartetAgreement(TreePath("unresolved1"), TreePath("unresolved2")), c(3, 0))

  # Pairs distance
  expect_equal(
    PairsQuartetDistance(TreePath("one_quartet_twice"), TreePath("two_quartets")),
    c(0, 1)
  )
})

test_that("Regression: all-pairs file-based distances still hold", {
  skip_if_not(file.exists(TreePath("five_trees")))

  allPairs <- AllPairsQuartetDistance(TreePath("five_trees"))

  # Values from original tqDist test suite (0-based → +1 for R indexing)
  expect_equal(allPairs[2, 1], 1L)  # [1+1, 0+1]
  expect_equal(allPairs[1, 1], 0L)  # [0+1, 0+1]
  expect_equal(allPairs[3, 2], 1L)  # [2+1, 1+1]
  expect_equal(allPairs[4, 3], 1L)  # [3+1, 2+1]
})

test_that("Regression: all-pairs agreement file-based values still hold", {
  skip_if_not(file.exists(TreePath("unresolved_list")))

  allPairsAgreement <- AllPairsQuartetAgreement(TreePath("unresolved_list"))

  expect_equal(c(12, 15, 9, 0), diag(allPairsAgreement[, , 1]))
  expect_equal(c(12, 15, 9, 0), 15 - diag(allPairsAgreement[, , 2]))
  expect_equal(c(9L, 3L), as.integer(allPairsAgreement[1, 3, ]))
  expect_equal(c(0L, 6L), as.integer(allPairsAgreement[3, 4, ]))
})

# ===========================================================================
# 3. INVARIANT: IDENTITY  (d(t, t) = 0 for any tree)
# ===========================================================================

test_that("Identity: distance from a tree to itself is always zero", {
  set.seed(7391)
  for (n in c(5L, 8L, 15L, 30L)) {
    for (i in seq_len(5L)) {
      t <- ape::rtree(n, br = NULL)
      d <- TQDist(structure(list(t, t), class = "multiPhylo"))
      expect_equal(d[1, 2], 0L,
                   label = sprintf("Identity failed: n=%d, rep=%d", n, i))
      expect_equal(d[2, 1], 0L,
                   label = sprintf("Identity symmetry failed: n=%d, rep=%d", n, i))
    }
  }
})

test_that("Identity: all-pairs diagonal is zero", {
  set.seed(2847)
  trees <- structure(lapply(rep(12L, 8L), ape::rtree, br = NULL),
                     class = "multiPhylo")
  d <- TQDist(trees)
  expect_true(all(diag(d) == 0L))
})

# ===========================================================================
# 4. INVARIANT: SYMMETRY  (d(t1, t2) = d(t2, t1))
# ===========================================================================

test_that("Symmetry: d(t1, t2) equals d(t2, t1) for random trees", {
  set.seed(5513)
  for (n in c(6L, 12L, 20L, 50L)) {
    t1 <- ape::rtree(n, br = NULL)
    t2 <- ape::rtree(n, br = NULL)
    trees <- structure(list(t1, t2), class = "multiPhylo")
    d <- TQDist(trees)
    expect_equal(d[1, 2], d[2, 1],
                 label = sprintf("Symmetry failed: n=%d", n))
  }
})

test_that("Symmetry: all-pairs distance matrix is symmetric", {
  set.seed(9014)
  trees <- structure(lapply(rep(15L, 6L), ape::rtree, br = NULL),
                     class = "multiPhylo")
  d <- TQDist(trees)
  expect_equal(d, t(d))
})

# ===========================================================================
# 5. INVARIANT: NON-NEGATIVITY
# ===========================================================================

test_that("Non-negativity: distances are >= 0", {
  set.seed(4471)
  trees <- structure(lapply(rep(10L, 8L), ape::rtree, br = NULL),
                     class = "multiPhylo")
  d <- TQDist(trees)
  expect_true(all(d >= 0L))
})

test_that("Non-negativity: A, E >= 0 and A + E <= C(n, 4)", {
  set.seed(3308)
  trees <- structure(lapply(rep(12L, 6L), ape::rtree, br = NULL),
                     class = "multiPhylo")
  ae <- TQAE(trees)
  A <- ae[, , "A"]
  E <- ae[, , "E"]

  expect_true(all(A >= 0L), label = "A >= 0")
  expect_true(all(E >= 0L), label = "E >= 0")
  expect_true(all(A + E <= q4(12L)), label = "A + E <= C(n,4)")
})

# ===========================================================================
# 6. INVARIANT: ROUND-TRIP  (distance = C(n,4) - A - E)
# ===========================================================================

test_that("Round-trip: TQDist == C(n,4) - A - E for all pairs", {
  set.seed(6629)
  n <- 10L
  trees <- structure(lapply(rep(n, 8L), ape::rtree, br = NULL),
                     class = "multiPhylo")

  d   <- TQDist(trees)
  ae  <- TQAE(trees)
  A   <- ae[, , "A"]
  E   <- ae[, , "E"]
  expected_d <- q4(n) - A - E

  expect_equal(d, expected_d,
               label = "TQDist != C(n,4) - A - E")
})

test_that("Round-trip: A + E == C(n,4) for self-comparisons", {
  set.seed(1184)
  for (n in c(5L, 10L, 20L)) {
    t <- ape::rtree(n, br = NULL)
    ae_self <- TQAE(structure(list(t, t), class = "multiPhylo"))
    # as.integer() strips array dim-name attributes that survive extraction
    expect_equal(
      as.integer(ae_self[1, 2, "A"]) + as.integer(ae_self[1, 2, "E"]),
      as.integer(q4(n)),
      label = sprintf("A + E != C(n,4) for self-comparison at n=%d", n)
    )
  }
})

# ===========================================================================
# 7. INVARIANT: LABEL-ORDER INDEPENDENCE
#    Permuting tip labels must not change the distance.
# ===========================================================================

test_that("Label permutation does not change distance", {
  # Relabelling ALL tips consistently across both trees must not change
  # the distance (the topology relationship is preserved).
  set.seed(8801)
  n <- 12L
  t1 <- ape::rtree(n, br = NULL)
  t2 <- ape::rtree(n, br = NULL)

  # Rename every tip the same way in both trees (e.g. "t3" → "t7", etc.)
  perm <- sample(n)
  old_labels <- paste0("t", seq_len(n))
  new_labels  <- paste0("t", perm)
  label_map   <- setNames(new_labels, old_labels)

  relabel <- function(tree) {
    tree$tip.label <- label_map[tree$tip.label]
    tree
  }

  trees_orig <- structure(list(t1,          t2),          class = "multiPhylo")
  trees_perm <- structure(list(relabel(t1), relabel(t2)), class = "multiPhylo")

  expect_equal(TQDist(trees_orig)[1, 2],
               TQDist(trees_perm)[1, 2],
               label = "Distance changed after consistent tip-label relabelling")
})

# ===========================================================================
# 8. CONSISTENCY: ALL-PAIRS == REPEATED SINGLE-PAIR
#    The vectorised all-pairs C++ path must agree with iterated pair-wise
#    calls, which use a separate C++ code path.
# ===========================================================================

test_that("All-pairs distance matches iterated single-pair calls", {
  skip_on_cran()   # guards parallel/vectorised vs sequential code-path agreement
  set.seed(2259)
  n_trees <- 6L
  n_tips  <- 12L
  trees <- structure(lapply(rep(n_tips, n_trees), ape::rtree, br = NULL),
                     class = "multiPhylo")

  d_all <- TQDist(trees)

  # Compute every pair individually and check
  for (i in seq_len(n_trees)) {
    for (j in seq_len(n_trees)) {
      if (i == j) next
      pair <- structure(list(trees[[i]], trees[[j]]), class = "multiPhylo")
      d_pair <- TQDist(pair)[1, 2]
      expect_equal(d_all[i, j], d_pair,
                   label = sprintf("All-pairs[%d,%d] != single-pair", i, j))
    }
  }
})

test_that("All-pairs agreement matches iterated single-pair calls", {
  skip_on_cran()   # guards parallel/vectorised vs sequential code-path agreement
  set.seed(7723)
  n_trees <- 5L
  n_tips  <- 10L
  trees <- structure(lapply(rep(n_tips, n_trees), ape::rtree, br = NULL),
                     class = "multiPhylo")

  ae_all <- TQAE(trees)

  for (i in seq_len(n_trees)) {
    for (j in seq_len(n_trees)) {
      if (i == j) next
      pair <- structure(list(trees[[i]], trees[[j]]), class = "multiPhylo")
      ae_pair <- TQAE(pair)
      expect_equal(ae_all[i, j, "A"], ae_pair[1, 2, "A"],
                   label = sprintf("TQAE A[%d,%d] != single-pair", i, j))
      expect_equal(ae_all[i, j, "E"], ae_pair[1, 2, "E"],
                   label = sprintf("TQAE E[%d,%d] != single-pair", i, j))
    }
  }
})

# ===========================================================================
# 9. CONSISTENCY: ONE-TO-MANY == ALL-PAIRS
# ===========================================================================

test_that("OneToMany agrees with all-pairs for the same trees", {
  skip_if_not(file.exists(TreePath("all_quartets")))

  allQuartets <- ape::read.tree(TreePath("all_quartets"))
  tqae        <- TQAE(allQuartets)
  single_row  <- SingleTreeQuartetAgreement(allQuartets, allQuartets[[1]])

  # Row 1 of all-pairs A should equal SingleTreeQuartetAgreement's "s" column
  expect_equal(as.integer(tqae[1, , "A"]),
               as.integer(single_row[, "s"]))
})

# ===========================================================================
# 10. POLYTOMY HANDLING
#     Unresolved nodes must not corrupt counts.
# ===========================================================================

test_that("Polytomous trees: round-trip invariant holds", {
  set.seed(9934)
  n <- 10L
  t_full <- ape::rtree(n, br = NULL)
  # Collapse one internal node to create a polytomy
  t_poly <- CollapseNode(t_full, (n + 2L):(n + Nnode(t_full) - 1L))

  trees <- structure(list(t_full, t_poly, t_full), class = "multiPhylo")
  d  <- TQDist(trees)
  ae <- TQAE(trees)

  # Round-trip
  expect_equal(d, q4(n) - ae[, , "A"] - ae[, , "E"])

  # Distance from t_full to itself is still 0
  expect_equal(d[1, 3], 0L)
  expect_equal(d[3, 1], 0L)

  # All values non-negative
  expect_true(all(d >= 0L))
  expect_true(all(ae[, , "A"] >= 0L))
  expect_true(all(ae[, , "E"] >= 0L))
})

test_that("Star tree (fully unresolved) vs resolved: A=0, E=0, distance=C(n,4)", {
  # The tqDist distance is C(n,4) - A - E.  For star vs resolved:
  #   A = 0  (star resolves nothing, so no quartet is agreed)
  #   E = 0  (resolved tree has no unresolved quartets, so no quartet
  #            is unresolved in BOTH trees)
  # Therefore distance = C(n,4) - 0 - 0 = C(n,4): the maximum possible.
  set.seed(5151)
  n <- 8L
  t_resolved <- ape::rtree(n, br = NULL)
  t_star     <- CollapseNode(t_resolved,
                              (n + 2L):(n + Nnode(t_resolved)))

  trees <- structure(list(t_resolved, t_star), class = "multiPhylo")
  d   <- TQDist(trees)
  ae  <- TQAE(trees)

  expect_equal(as.integer(ae[1, 2, "A"]), 0L, label = "A should be 0 for resolved vs star")
  expect_equal(as.integer(ae[1, 2, "E"]), 0L, label = "E should be 0 for resolved vs star")
  expect_equal(d[1, 2], q4(n),   label = "distance should equal C(n,4) for resolved vs star")
  expect_equal(d[2, 1], q4(n),   label = "distance should equal C(n,4) for star vs resolved")
})

# ===========================================================================
# 11. MEDIUM-SCALE CROSS-CHECK (random seed, larger trees)
#     Run invariants at a scale where counter bugs are likely to surface.
# ===========================================================================

test_that("Medium-scale (50 tips, 20 trees): round-trip and symmetry hold", {
  skip_on_cran()   # runtime ~1-3 s; skip during CRAN checks
  set.seed(3841)
  n <- 50L
  trees <- structure(lapply(rep(n, 20L), ape::rtree, br = NULL),
                     class = "multiPhylo")

  d  <- TQDist(trees)
  ae <- TQAE(trees)

  # Round-trip
  expect_equal(d, q4(n) - ae[, , "A"] - ae[, , "E"],
               label = "Round-trip failed at n=50")

  # Symmetry
  expect_equal(d, t(d), label = "Symmetry failed at n=50")

  # Non-negativity
  expect_true(all(d >= 0L),           label = "Negative distance at n=50")
  expect_true(all(ae[, , "A"] >= 0L), label = "Negative A at n=50")
  expect_true(all(ae[, , "E"] >= 0L), label = "Negative E at n=50")

  # Identity diagonal
  expect_true(all(diag(d) == 0L),     label = "Non-zero diagonal at n=50")
})
