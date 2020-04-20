context('CompareQuartets.R')
library('TreeTools')

TreePath <- function (fileName) {
  paste0(system.file(package='Quartet'), '/trees/', fileName, '.new')
}
quartets <- ape::read.tree(TreePath('all_quartets'))

test_that("QuartetStates works", {
  expect_equal(2L, QuartetStates(quartets[[1]]))
  expect_equal(3L, QuartetStates(as.Splits(quartets[[2]], letters[1:4])))
  expect_equal(c(2L, 3L, 4L, 0L), QuartetStates(quartets)[1, ])

  expect_equal(as.raw(2L), QuartetStates(quartets[[1]], TRUE))
  expect_equal(as.raw(3L), 
               QuartetStates(as.Splits(quartets[[2]], letters[1:4]), TRUE))
  expect_equal(as.raw(c(2L, 3L, 4L, 0L)), QuartetStates(quartets, TRUE)[1, ])
  
  expect_equal(c(2, 2, 2, 2, 2), QuartetStates(BalancedTree(5), asRaw = FALSE))
  expect_equal(c(3, 3, 4, 2, 3),
               QuartetStates(RenumberTips(BalancedTree(c(1, 3, 5, 4, 2)), 
                                                       BalancedTree(1:5)),
                             asRaw = FALSE))
})

test_that("QuartetState works", {
  expect_equal(as.raw(2L),
               QuartetState(letters[1:4], as.Splits(quartets[[1]]), asRaw = TRUE))
  expect_equal(as.raw(3L),
               QuartetState(letters[1:4], as.Splits(quartets[[2]]), asRaw = TRUE))
  expect_equal(as.raw(4L),
               QuartetState(letters[1:4], as.Splits(quartets[[3]]), asRaw = TRUE))
  expect_equal(as.raw(0L),
               QuartetState(letters[1:4], as.Splits(quartets[[4]]), asRaw = TRUE))
})

test_that("CompareQuartets works", {
  expect_equal(c(N = 16L, Q = 8L, s = 1, d = 2, r1 = 1, r2 = 1, u = 3),
               CompareQuartets(c(2, 2, 4, 2, 0, 0, 0, 0),
                               c(2, 3, 3, 0, 2, 0, 0, 0)))
})


test_that("CompareQuartetsMulti() fails if leaves don't match", {
  expect_error(CompareQuartetsMulti(PectinateTree(1:8), 
                                    list(PectinateTree(1:8),
                                         PectinateTree(1:9))))
  expect_error(CompareQuartetsMulti(PectinateTree(1:8), PectinateTree(2:9)))
})


test_that('CompareQuartetsMulti() gets values correct', {
  bal <- BalancedTree(6L)
  pec <- PectinateTree(6L)
  rnd <- CollapseNode(as.phylo(1337, 6L), 8:9)
  part <- CollapseNode(pec, 9:10)
  star <- CollapseNode(bal, 8:11)
  index <- cbind(
    rnd = QuartetStates(RenumberTips(rnd, bal), T),
    bal = QuartetStates(RenumberTips(bal, bal), T),
    pec = QuartetStates(RenumberTips(pec, bal), T),
    prt = QuartetStates(RenumberTips(part, bal), T)
  )
  rownames(index) <- vapply(AllQuartets(6), paste0, character(1), collapse='')
  
  expect_equivalent(as.raw(c(4, 3, 4, 3, 0, 4, 3, 0, 4, 0, 4, 2, 3, 3, 3)), 
                    index[, 'rnd'])
  expect_equivalent(as.raw(c(4, 4, 4, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2)), 
                    index[, 'bal'])
  expect_equivalent(as.raw(c(2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2)), 
                    index[, 'pec'])
  expect_equivalent(as.raw(c(0, 0, 0, 0, 0, 2, 0, 0, 2, 2, 0, 0, 2, 2, 2)), 
                    index[, 'prt'])
  index
  
  # par (mfrow = c(2, 2), mar = rep(0.1, 4))
  # plot(rnd, main='rnd'); plot(bal, main='bal')
  # plot(pec, main = 'pec'); plot(part, main = 'part')
  
  expect_equal(c(N = 15 * 4, Q = 15, s_all = 0, s_any = 6, #1..4
                 d_all = 0, d_any = 5, r1_all = 0, r1_any = 1, # 5..8
                 r2_all = 7, r2_any = 9, u_all = 0, u_any = 2, x_only = 0),
               CompareQuartetsMulti(x = part, cf = list(bal, pec, rnd)))
  
  expect_equal(c(N = 15 * 4, Q = 15, s_all = 0, s_any = 3,
                 d_all = 5, d_any = 11, r1_all = 0, r1_any = 7,
                 r2_all = 1, r2_any = 3, u_all = 0, u_any = 2, x_only = 9),
               CompareQuartetsMulti(x = rnd, cf = list(bal, pec, part)))
  
  expect_equal(c(N = 15 * 2, Q = 15, s_all = 0, s_any = 0, d_all = 0, d_any = 0,
                 r1_all = 6, r1_any = 6, r2_all = 0, r2_any = 0, u_all = 9,
                 u_any = 9, x_only = 6),
               CompareQuartetsMulti(part, star))
  
  expect_equal(c(N = 15 * 2, Q = 15, s_all = 0, s_any = 0, d_all = 0, d_any = 0,
                 r1_all = 0, r1_any = 0, r2_all = 6, r2_any = 6, u_all = 9,
                 u_any = 9, x_only = 0),
               CompareQuartetsMulti(star, part))
})

test_that('CompareQuartetsMulti() insensitive to label order', {
  all_same <- c(Q = 70, s_all = 70, s_any = 70, d_all = 0, d_any = 0,
                r1_all = 0, r1_any = 0, r2_all = 0, r2_any = 0, u_all = 0,
                u_any = 0, x_only = 0)
  expect_equal(all_same, 
               CompareQuartetsMulti(PectinateTree(1:8), PectinateTree(8:1))[-1])
  expect_equal(all_same,
               CompareQuartetsMulti(BalancedTree(1:8), 
                                    list(BalancedTree(8:1), 
                                         BalancedTree(c(1, 2, 4, 3, 8:5))))[-1])
  
  # Same representation, different labels
  expect_equivalent(c(210, 70, 53, 53, 17, 17, rep(0, 6), 17),
               CompareQuartetsMulti(BalancedTree(1:8), 
                                    list(BalancedTree(c(1, 3, 2, 4, 5:8)),
                                         BalancedTree(c(1, 3, 2, 4, 8:5)))))
})
