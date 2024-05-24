test_that("CPDT reads tree", {
  tree4.1 <- ape::read.tree(text="(1, (2, (3, 4)));")
  tree4.2 <- ape::read.tree(text="(1, (4, (3, 2)));")
  
  expect_parse_size <- function(tr, binary = TRUE) {
    expect_equal(cpdt_tree(tr)[-4],
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
    strsplit(cpdt_tree(RenumberTips(tree4.2, tree4.1))$string, "\\n")[[1]],
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
  if (interactive()) {
    plot(cl7)
    nodelabels(c(0, 1, 2, 5, 8))
    tiplabels(c(3, 4, 6, 7, 9, 10, 11))
  }
  expect_equal(
    strsplit(cpdt_tree(cl7)$string, "\\n")[[1]],
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

test_that("CPDT produces correct results with four leaves", {
  tree4.1 <- ape::read.tree(text="(1, (2, (3, 4)));")
  tree4.2 <- ape::read.tree(text="(1, (4, (3, 2)));")
  q <- QuartetStatus(tree4.1, tree4.2)
  expect_equal(CPDTDist(tree4.1, tree4.1), 0)
  expect_equal(CPDTDist(tree4.2, tree4.2), 0)
  
  
  f4.1 <- TQFile(tree4.1)
  f4.2 <- TQFile(tree4.2)
  on.exit(unlink(c(f4.1, f4.2)))
  
  expect_equal(CPDTDist(tree4.1, tree4.2), TripletDistance(f4.1, f4.2))
})

test_that("CPDT produces sane results", {
  skip_if(TRUE)
  #
  #
  tree1 <- TreeTools::BalancedTree(8)
  rtree1 <- TreeTools::RootTree(TreeTools::BalancedTree(8), "t1")
  tree2 <- TreeTools::PectinateTree(8)
  q <- QuartetStatus(tree1, tree2)
  expect_error(CPDTDist(1, tree1), "`tree1` must be")
  expect_error(CPDTDist(tree1, 2), "`tree2` must be")
  # expect_warning(
  # expect_equal(CPDTDist(tree1, rtree1), 0),
  # "Unrooted"
  # )
  expect_equal(CPDTDist(rtree1, rtree1), 0)
  f1 <- TQFile(rtree1)
  f2 <- TQFile(tree2)
  on.exit(unlink(c(f1, f2)))
  
  #expect_equal(CPDTDist(tree2, tree2), 0)
  #expect_equal(CPDTDist(rtree1, tree2), TripletDistance(f1, f2))
})
