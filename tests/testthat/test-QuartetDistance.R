data("sq_trees")

test_that("Distances are calculated from strings", {
  set.seed(0)
  
  trees <- structure(lapply(rep(15, 5), ape::rtree, br = NULL),
                     class = "multiPhylo")
  strs <- write.tree(trees)
  
  fileName <- TQFile(trees)
  on.exit(file.remove(fileName))
  
  expect_equal(AllPairsQuartetDistance(fileName),
               TQDist(trees))
  expect_equal(AllPairsQuartetAgreement(fileName),
               TQAE(trees))
  expect_equal(ManyToManyQuartetAgreement(trees)[1, , ],
               SingleTreeQuartetAgreement(trees, trees[[1]])[, ])
})

test_that("Splits are compared", {
  trees <- TreeTools::UnshiftTree(
    ape::drop.tip(sq_trees$move_one_near, 10),
    ape::drop.tip(sq_trees$ref_tree, 11))
  expect_equal(c(N = 252L, Q = 126L, s = 120L, d = 6L, r1 = 0L, r2 = 0L, u = 0L), 
               SharedQuartetStatus(trees)[2, ])
})

test_that("PlotQuartet() works", {
  skip_if_not_installed("vdiffr")
  vdiffr::expect_doppelganger("PlotQuartet", function() {
    data("sq_trees")
    
    par(mfrow = c(3, 2), mar = rep(0.5, 4), cex = 1.1)
    PlotQuartet(sq_trees[c(1, 9, 13:16)], c(2, 5, 3, 8), overwritePar = FALSE)
  })
  vdiffr::expect_doppelganger("PlotQuartet-one-star", function() {
    PlotQuartet(StarTree(4), 1:4, caption = FALSE)
  })
  vdiffr::expect_doppelganger("PlotQuartet-gain", function() {
    par(mfrow = c(3, 2), mar = rep(0.5, 4), cex = 1.1)
    PlotQuartet(c(StarTree(4), BalancedTree(4)), 1:4, overwritePar = TRUE)
  })
})
