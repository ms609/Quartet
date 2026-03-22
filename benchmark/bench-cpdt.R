library(bench)
library(Quartet)
library(TreeTools)

# --- Part 1: CPDT vs tqDist for triplet distance ---

cpdt_triplet <- function(t1, t2) {
  CPDTDist(t1, t2)
}

tqdist_triplet <- function(t1, t2) {
  f1 <- TQFile(t1)
  f2 <- TQFile(t2)
  on.exit(unlink(c(f1, f2)))
  TripletDistance(f1, f2)
}

set.seed(3847)
sizes <- c(10, 50, 100, 500, 1000, 5000)
triplet_results <- list()

for (n in sizes) {
  cat("Triplet benchmark: n =", n, "\n")
  t1 <- RootTree(BalancedTree(n), 1)
  t2 <- RootTree(PectinateTree(n), 1)
  
  bm <- mark(
    cpdt = cpdt_triplet(t1, t2),
    tqdist = tqdist_triplet(t1, t2),
    check = FALSE,
    min_iterations = 3
  )
  bm$n <- n
  bm$shape <- "bal_vs_pec"
  triplet_results <- c(triplet_results, list(bm))
}

# Random trees too
for (n in c(100, 500, 1000)) {
  cat("Triplet benchmark (random): n =", n, "\n")
  t1 <- RootTree(RandomTree(n), 1)
  t2 <- RootTree(RandomTree(n), 1)
  
  bm <- mark(
    cpdt = cpdt_triplet(t1, t2),
    tqdist = tqdist_triplet(t1, t2),
    check = FALSE,
    min_iterations = 3
  )
  bm$n <- n
  bm$shape <- "random"
  triplet_results <- c(triplet_results, list(bm))
}

cat("\n=== TRIPLET DISTANCE: CPDT vs tqDist ===\n")
triplet_df <- do.call(rbind, lapply(triplet_results, function(bm) {
  data.frame(
    expr = as.character(bm$expression),
    n = bm$n,
    shape = bm$shape,
    median_ms = as.numeric(bm$median) * 1000
  )
}))
print(reshape(triplet_df, direction = "wide", idvar = c("n", "shape"),
              timevar = "expr", v.names = "median_ms"))


# --- Part 2: Quartet distance via triplet rooting (proof of concept) ---

quartet_via_triplets <- function(tree1, tree2) {
  tips <- tree1$tip.label
  total <- 0L
  tree2r <- RenumberTips(tree2, tree1)
  for (tip in tips) {
    r1 <- RootTree(tree1, tip)
    r2 <- RootTree(tree2r, tip)
    total <- total + CPDTDist(r1, r2)
  }
  total %/% 4L
}

# Validate correctness first
cat("\n=== VALIDATING quartet_via_triplets ===\n")
for (n in c(5, 8, 12, 20)) {
  t1 <- RootTree(BalancedTree(n), 1)
  t2 <- RootTree(PectinateTree(n), 1)
  
  f1 <- TQFile(t1)
  f2 <- TQFile(t2)
  tqdist_qd <- QuartetDistance(f1, f2)
  unlink(c(f1, f2))
  
  cpdt_qd <- quartet_via_triplets(t1, t2)
  cat(sprintf("  n=%d: tqDist=%d, CPDT-via-triplets=%d, match=%s\n",
              n, tqdist_qd, cpdt_qd, identical(tqdist_qd, cpdt_qd)))
}


# --- Part 3: Benchmark quartet-via-triplets vs tqDist quartet ---

tqdist_quartet <- function(t1, t2) {
  f1 <- TQFile(t1)
  f2 <- TQFile(t2)
  on.exit(unlink(c(f1, f2)))
  QuartetDistance(f1, f2)
}

quartet_sizes <- c(10, 50, 100, 200)
quartet_results <- list()

for (n in quartet_sizes) {
  cat("Quartet benchmark: n =", n, "\n")
  t1 <- RootTree(BalancedTree(n), 1)
  t2 <- RootTree(PectinateTree(n), 1)
  
  bm <- mark(
    tqdist = tqdist_quartet(t1, t2),
    cpdt_via_triplets = quartet_via_triplets(t1, t2),
    check = FALSE,
    min_iterations = 3
  )
  bm$n <- n
  quartet_results <- c(quartet_results, list(bm))
}

cat("\n=== QUARTET DISTANCE: tqDist vs CPDT-via-triplets ===\n")
quartet_df <- do.call(rbind, lapply(quartet_results, function(bm) {
  data.frame(
    expr = as.character(bm$expression),
    n = bm$n,
    median_ms = as.numeric(bm$median) * 1000
  )
}))
print(reshape(quartet_df, direction = "wide", idvar = "n",
              timevar = "expr", v.names = "median_ms"))

# --- Part 4: Non-binary quartet comparison ---
cat("\n=== NON-BINARY QUARTET DISTANCE ===\n")
for (n in c(20, 50, 100)) {
  t1 <- CollapseNode(BalancedTree(n), n + 2)
  t2 <- CollapseNode(PectinateTree(n), n + 2)
  
  bm <- mark(
    tqdist = tqdist_quartet(t1, t2),
    cpdt_via_triplets = quartet_via_triplets(t1, t2),
    check = FALSE,
    min_iterations = 3
  )
  cat(sprintf("  n=%d: tqDist=%.1fms, CPDT-via-triplets=%.1fms, ratio=%.1fx\n",
              n,
              as.numeric(bm$median[1]) * 1000,
              as.numeric(bm$median[2]) * 1000,
              as.numeric(bm$median[2]) / as.numeric(bm$median[1])))
}
