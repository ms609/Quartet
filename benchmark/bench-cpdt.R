library(bench)
library(Quartet)
library(TreeTools)

# --- Part 1: CPDT vs tqDist for triplet distance ---
# CPDT takes edge matrices directly; tqDist requires file I/O.
# This comparison is inherently unfair to tqDist, but there is no
# edge-based triplet entry point in tqDist.

cpdt_triplet <- function(t1, t2) CPDTDist(t1, t2)

tqdist_triplet <- function(t1, t2) {
  f1 <- TQFile(t1)
  f2 <- TQFile(t2)
  on.exit(unlink(c(f1, f2)))
  TripletDistance(f1, f2)
}

set.seed(3847)
cat("=== TRIPLET DISTANCE: CPDT vs tqDist (file) ===\n")
for (n in c(10, 50, 100, 500, 1000, 5000)) {
  t1 <- RootTree(BalancedTree(n), 1)
  t2 <- RootTree(PectinateTree(n), 1)
  bm <- mark(cpdt = cpdt_triplet(t1, t2),
             tqdist = tqdist_triplet(t1, t2),
             check = FALSE, min_iterations = 3)
  cat(sprintf("n=%5d: cpdt=%8.2fms  tqdist=%8.2fms  speedup=%.0fx\n",
              n,
              as.numeric(bm$median[1]) * 1000,
              as.numeric(bm$median[2]) * 1000,
              as.numeric(bm$median[2]) / as.numeric(bm$median[1])))
}

# --- Part 2: Quartet distance via CPDT rootings vs tqDist edge-based ---
# This is the fair comparison: tqDist's edge-based quartet (no file I/O)
# vs CPDT computing quartet distance as (1/4) * sum of triplet distances
# over all leaf rootings.

tqdist_quartet_edge <- function(t1, t2) {
  e1 <- Quartet:::.TreeToEdge.phylo(t1)
  e2 <- Quartet:::.TreeToEdge.phylo(t2, t1$tip.label)
  tqdist_QuartetAgreementEdge(e1, e2)
}

cpdt_quartet <- function(t1, t2) {
  tips <- t1$tip.label
  total <- 0L
  t2r <- RenumberTips(t2, t1)
  for (tip in tips) {
    r1 <- RootTree(t1, tip)
    r2 <- RootTree(t2r, tip)
    total <- total + CPDTDist(r1, r2)
  }
  total %/% 4L
}

# Validate
cat("\n=== VALIDATION: quartet_via_triplets ===\n")
for (n in c(5, 8, 12, 20, 50)) {
  t1 <- Preorder(RootTree(BalancedTree(n), 1))
  t2 <- Preorder(PectinateTree(n))
  ae <- tqdist_quartet_edge(t1, t2)
  Q <- choose(n, 4)
  tq_dist <- Q - ae[1] - ae[2]
  cpdt_dist <- cpdt_quartet(t1, t2)
  cat(sprintf("  n=%d: match=%s\n", n, identical(as.integer(tq_dist), cpdt_dist)))
}

# Binary trees
cat("\n=== QUARTET: tqDist (edge) vs CPDT-via-triplets (binary) ===\n")
set.seed(4821)
for (n in c(10, 20, 50, 100, 200, 500)) {
  t1 <- Preorder(RootTree(BalancedTree(n), 1))
  t2 <- Preorder(PectinateTree(n))
  bm <- mark(tqdist_edge = tqdist_quartet_edge(t1, t2),
             cpdt_via_triplets = cpdt_quartet(t1, t2),
             check = FALSE, min_iterations = 3)
  cat(sprintf("n=%4d: tqDist=%7.2fms  cpdt=%8.2fms  ratio=%.1fx\n",
              n,
              as.numeric(bm$median[1]) * 1000,
              as.numeric(bm$median[2]) * 1000,
              as.numeric(bm$median[2]) / as.numeric(bm$median[1])))
}

# Star trees (worst case for tqDist's O(nd log n))
cat("\n=== QUARTET: star trees (extreme polytomy) ===\n")
set.seed(9173)
for (n in c(20, 50, 100)) {
  t1 <- StarTree(n)
  t2 <- BalancedTree(n)
  bm <- mark(tqdist_edge = tqdist_quartet_edge(t1, t2),
             cpdt_via_triplets = cpdt_quartet(t1, t2),
             check = FALSE, min_iterations = 3)
  cat(sprintf("n=%3d (max_deg=%d): tqDist=%7.2fms  cpdt=%7.2fms  ratio=%.1fx\n",
              n, n,
              as.numeric(bm$median[1]) * 1000,
              as.numeric(bm$median[2]) * 1000,
              as.numeric(bm$median[2]) / as.numeric(bm$median[1])))
}
