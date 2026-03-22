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

test_that("CPDT input validation", {
  expect_error(CPDTDist(1, BalancedTree(8)), "`tree1` must be")
  expect_error(CPDTDist(BalancedTree(8), 2), "`tree2` must be")
})

# Helper: compute tqDist triplet distance via file interface
tqdist_triplet <- function(tree1, tree2) {
  f1 <- TQFile(tree1)
  f2 <- TQFile(tree2)
  on.exit(unlink(c(f1, f2)))
  TripletDistance(f1, f2)
}

test_that("CPDT: identical trees give distance 0", {
  for (n in c(4, 8, 16, 50)) {
    bal <- RootTree(BalancedTree(n), 1)
    expect_equal(CPDTDist(bal, bal), 0L, label = paste0("balanced ", n))
    
    pec <- RootTree(PectinateTree(n), 1)
    expect_equal(CPDTDist(pec, pec), 0L, label = paste0("pectinate ", n))
  }
})

test_that("CPDT: 3-tip trees", {
  t3a <- ape::read.tree(text = "(1, (2, 3));")
  t3b <- ape::read.tree(text = "(2, (1, 3));")
  expect_equal(CPDTDist(t3a, t3a), 0L)
  expect_equal(CPDTDist(t3a, t3b), tqdist_triplet(t3a, t3b))
})

test_that("CPDT matches tqDist for binary trees at various sizes", {
  set.seed(7284)
  for (n in c(4, 8, 16, 50, 100)) {
    tree1 <- RootTree(BalancedTree(n), 1)
    tree2 <- RootTree(PectinateTree(n), 1)
    
    expected <- tqdist_triplet(tree1, tree2)
    got <- CPDTDist(tree1, tree2)
    expect_equal(got, expected,
                 label = paste0("balanced vs pectinate, n=", n))
    
    # Random trees
    tree3 <- RootTree(RandomTree(n), 1)
    tree4 <- RootTree(RandomTree(n), 1)
    expected2 <- tqdist_triplet(tree3, tree4)
    got2 <- CPDTDist(tree3, tree4)
    expect_equal(got2, expected2,
                 label = paste0("random vs random, n=", n))
  }
})

test_that("CPDT matches tqDist for non-binary trees", {
  bal7 <- BalancedTree(7)
  pec7 <- PectinateTree(7)
  
  # Collapse a node to create a polytomy
  cl7 <- CollapseNode(bal7, 13)
  expect_true(max(tabulate(cl7$edge[, 1])) > 2) # confirm polytomy
  
  expected <- tqdist_triplet(cl7, pec7)
  got <- CPDTDist(cl7, pec7)
  expect_equal(got, expected, label = "polytomy vs binary")
  
  # Both non-binary
  cl7b <- CollapseNode(pec7, 10)
  expected2 <- tqdist_triplet(cl7, cl7b)
  got2 <- CPDTDist(cl7, cl7b)
  expect_equal(got2, expected2, label = "polytomy vs polytomy")
})

test_that("CPDT: star tree has maximal distance to resolved tree", {
  # A star tree resolves no triplets, so distance = all resolved triplets
  # in the other tree
  n <- 8
  star <- StarTree(n)
  resolved <- RootTree(BalancedTree(n), 1)
  
  # ResolvedTriplets returns c(resolved, unresolved)
  n_resolved <- ResolvedTriplets(resolved)[1]
  expect_equal(CPDTDist(star, resolved), n_resolved)
})

test_that("CPDT is symmetric", {
  tree1 <- RootTree(BalancedTree(20), 1)
  tree2 <- RootTree(PectinateTree(20), 1)
  expect_equal(CPDTDist(tree1, tree2), CPDTDist(tree2, tree1))
})

test_that("CPDT can be called repeatedly (state reset between calls)", {
  tree1 <- RootTree(BalancedTree(16), 1)
  tree2 <- RootTree(PectinateTree(16), 1)
  
  result1 <- CPDTDist(tree1, tree2)
  result2 <- CPDTDist(tree1, tree2)
  result3 <- CPDTDist(tree1, tree2)
  
  expect_equal(result1, result2)
  expect_equal(result2, result3)
})
