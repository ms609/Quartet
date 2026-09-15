test_that("OpenMP thread count follows mc.cores", {
  library("TreeTools", quietly = TRUE)
  trees <- as.phylo(0:5, 12)
  tmp1 <- TQFile(trees)
  tmp2 <- TQFile(rev(trees))
  on.exit(file.remove(tmp1, tmp2))

  Run <- function() {
    list(
      one = SingleTreeQuartetAgreement(trees, trees[[1]]),
      pairs = PairsQuartetDistance(tmp1, tmp2),
      dist = TQDist(trees),
      ae = TQAE(trees)
    )
  }

  oldOpt <- options(mc.cores = NULL)
  on.exit(options(oldOpt), add = TRUE)
  serial <- Run()

  for (cores in list(2L, "2", 0L, NA)) {
    options(mc.cores = cores)
    expect_equal(Run(), serial)
  }
})
