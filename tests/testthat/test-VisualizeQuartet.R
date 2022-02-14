test_that("VizualiseQuartets()", {
  skip_if_not_installed("vdiffr")
  skip_if_not_installed("TreeTools", "1.7.0")
  vdiffr::expect_doppelganger("VQ-label", function() {
    VisualizeQuartets(BalancedTree(10), CollapseNode(PectinateTree(10), 19),
                     style = 'label', precision = 1)
  })
  vdiffr::expect_doppelganger("VQ-tworow", function() {
    # Keep original plotting parameters:
    origPar <- par(mfrow = c(2, 2))
    VisualizeQuartets(BalancedTree(10), CollapseNode(PectinateTree(10), 19),
                     setPar = FALSE)
    VisualizeQuartets(BalancedTree(10), CollapseNode(PectinateTree(10), 19),
                     style = 'bar', legend = FALSE, setPar = FALSE,
                     spectrum = c(rep('red', 100), 'green'))
    par(origPar)
  })
  vdiffr::expect_doppelganger("VQ-size", function() {
    VisualizeQuartets(BalancedTree(20), CollapseNode(PectinateTree(20), 29:33),
                     style = 'size', scale = 2)
  })
})