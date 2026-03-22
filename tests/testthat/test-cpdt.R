test_that("CPDT reads tree", {
  tree4.1 <- ape::read.tree(text = "(1, (2, (3, 4)));")
  tree4.2 <- ape::read.tree(text = "(1, (4, (3, 2)));")
  
  expect_parse_size <- function(tr, binary = TRUE) {
    expect_equal(Quartet:::cpdt_tree(tr)[-4],
                 list(ntip = NTip(tr), nnode = tr$Nnode + NTip(tr), 
                      binary = binary))
  }
  expect_parse_size(tree4.1)
  expect_parse_size(tree4.2)
  
  Node <- function(id, children = "", parent = -1, taxa = -1) {
    sprintf("%i id=%i children = (%s), parent = %i, taxa = %i",
            id, id, paste(children, collapse = ", "), parent, taxa)
  }
  expect_equal(
    strsplit(Quartet:::cpdt_tree(RenumberTips(tree4.2, tree4.1))$string, "\\n")[[1]],
    c(Node(0, 1:2),
      Node(1, p = 0, t = 0),
      Node(2, c = c(3, 6), p = 0),
      Node(3, c = c(4, 5), p = 2),
      Node(4, p = 3, t = 1),
      Node(5, p = 3, t = 2),
      Node(6, p = 2, t = 3)
    )
  )
  
  bal7 <- BalancedTree(7)
  expect_parse_size(bal7)
  
  cl7 <- CollapseNode(bal7, 13)
  expect_parse_size(cl7, FALSE)
  expect_equal(
    strsplit(Quartet:::cpdt_tree(cl7)$string, "\\n")[[1]],
    c(Node(0, c(1, 8)),
      Node(1, p = 0, c = c(2, 5)),
      Node(2, p = 1, c = 3:4),
      Node(3, p = 2, t = 0),
      Node(4, p = 2, t = 1),
      Node(5, p = 1, c = c(6, 7)),
      Node(6, p = 5, t = 2),
      Node(7, p = 5, t = 3),
      Node(8, p = 0, c = 9:11),
      Node(9, p = 8, t = 4),
      Node(10, p = 8, t = 5),
      Node(11, p = 8, t = 6)
    )
  )
})

test_that("TripletDistance input validation", {
  expect_error(TripletDistance(1, BalancedTree(8)), "no applicable method")
  expect_error(TripletDistance(BalancedTree(8), 2), "`tree2` must be")
  expect_error(TripletDistance(BalancedTree(8)), "`tree2` is required")
})

# Helper: compute tqDist triplet distance via file interface
tqdist_triplet <- function(tree1, tree2) {
  f1 <- TQFile(tree1)
  f2 <- TQFile(tree2)
  on.exit(unlink(c(f1, f2)))
  tqdist_TripletDistance(f1, f2)
}

test_that("TripletDistance: identical trees give distance 0", {
  for (n in c(4, 8, 16, 50)) {
    bal <- RootTree(BalancedTree(n), 1)
    expect_equal(TripletDistance(bal, bal), 0L, label = paste0("balanced ", n))
    
    pec <- RootTree(PectinateTree(n), 1)
    expect_equal(TripletDistance(pec, pec), 0L, label = paste0("pectinate ", n))
  }
})

test_that("TripletDistance: 3-tip trees", {
  t3a <- ape::read.tree(text = "(1, (2, 3));")
  t3b <- ape::read.tree(text = "(2, (1, 3));")
  expect_equal(TripletDistance(t3a, t3a), 0L)
  expect_equal(TripletDistance(t3a, t3b), tqdist_triplet(t3a, t3b))
})

test_that("TripletDistance matches tqDist for binary trees at various sizes", {
  set.seed(7284)
  for (n in c(4, 8, 16, 50, 100)) {
    tree1 <- RootTree(BalancedTree(n), 1)
    tree2 <- RootTree(PectinateTree(n), 1)
    
    expected <- tqdist_triplet(tree1, tree2)
    got <- TripletDistance(tree1, tree2)
    expect_equal(got, expected,
                 label = paste0("balanced vs pectinate, n=", n))
    
    # Random trees
    tree3 <- RootTree(RandomTree(n), 1)
    tree4 <- RootTree(RandomTree(n), 1)
    expected2 <- tqdist_triplet(tree3, tree4)
    got2 <- TripletDistance(tree3, tree4)
    expect_equal(got2, expected2,
                 label = paste0("random vs random, n=", n))
  }
})

test_that("TripletDistance matches tqDist for non-binary trees", {
  bal7 <- BalancedTree(7)
  pec7 <- PectinateTree(7)
  
  # Collapse a node to create a polytomy
  cl7 <- CollapseNode(bal7, 13)
  expect_true(max(tabulate(cl7$edge[, 1])) > 2) # confirm polytomy
  
  expected <- tqdist_triplet(cl7, pec7)
  got <- TripletDistance(cl7, pec7)
  expect_equal(got, expected, label = "polytomy vs binary")
  
  # Both non-binary
  cl7b <- CollapseNode(pec7, 10)
  expected2 <- tqdist_triplet(cl7, cl7b)
  got2 <- TripletDistance(cl7, cl7b)
  expect_equal(got2, expected2, label = "polytomy vs polytomy")
})

test_that("TripletDistance: star tree has maximal distance to resolved tree", {
  n <- 8
  star <- StarTree(n)
  resolved <- RootTree(BalancedTree(n), 1)
  
  n_resolved <- ResolvedTriplets(resolved)[1]
  expect_equal(TripletDistance(star, resolved), n_resolved)
})

test_that("TripletDistance is symmetric", {
  tree1 <- RootTree(BalancedTree(20), 1)
  tree2 <- RootTree(PectinateTree(20), 1)
  expect_equal(TripletDistance(tree1, tree2), TripletDistance(tree2, tree1))
})

test_that("TripletDistance can be called repeatedly (state reset)", {
  tree1 <- RootTree(BalancedTree(16), 1)
  tree2 <- RootTree(PectinateTree(16), 1)
  
  result1 <- TripletDistance(tree1, tree2)
  result2 <- TripletDistance(tree1, tree2)
  result3 <- TripletDistance(tree1, tree2)
  
  expect_equal(result1, result2)
  expect_equal(result2, result3)
})

test_that("TripletDistance.list: all-pairs", {
  trees <- list(
    RootTree(BalancedTree(8), 1),
    RootTree(PectinateTree(8), 1),
    StarTree(8)
  )
  result <- TripletDistance(trees)
  expect_equal(dim(result), c(3, 3))
  expect_equal(diag(result), c(0L, 0L, 0L))
  # Symmetric
  expect_equal(result[1, 2], result[2, 1])
  expect_equal(result[1, 3], result[3, 1])
  # Matches pairwise calls
  expect_equal(result[1, 2], TripletDistance(trees[[1]], trees[[2]]))
  expect_equal(result[1, 3], TripletDistance(trees[[1]], trees[[3]]))
})

test_that("TripletDistance.list: paired distances", {
  trees1 <- list(RootTree(BalancedTree(8), 1), StarTree(8))
  trees2 <- list(RootTree(PectinateTree(8), 1), RootTree(BalancedTree(8), 1))
  result <- TripletDistance(trees1, trees2)
  expect_length(result, 2)
  expect_equal(result[1], TripletDistance(trees1[[1]], trees2[[1]]))
  expect_equal(result[2], TripletDistance(trees1[[2]], trees2[[2]]))
})

test_that("TripletDistance.list: length mismatch errors", {
  trees1 <- list(BalancedTree(8), PectinateTree(8))
  trees2 <- list(BalancedTree(8))
  expect_error(TripletDistance(trees1, trees2), "same number of trees")
})

test_that("TripletDistance.character: all-pairs from file", {
  trees <- list(RootTree(BalancedTree(8), 1), RootTree(PectinateTree(8), 1))
  f <- TQFile(trees)
  on.exit(unlink(f))
  result <- TripletDistance(f)
  expect_equal(dim(result), c(2, 2))
  expect_equal(result[1, 2], TripletDistance(trees[[1]], trees[[2]]))
})

test_that("TripletDistance.multiPhylo: all-pairs", {
  trees <- structure(
    list(RootTree(BalancedTree(8), 1), RootTree(PectinateTree(8), 1)),
    class = "multiPhylo"
  )
  result <- TripletDistance(trees)
  expect_equal(dim(result), c(2, 2))
  expect_equal(result[1, 2], TripletDistance(trees[[1]], trees[[2]]))
})

test_that("CPDTDist is deprecated", {
  tree1 <- RootTree(BalancedTree(8), 1)
  tree2 <- PectinateTree(8)
  lifecycle::expect_deprecated(CPDTDist(tree1, tree2))
})
