context('CompareQuartets.R')
library('TreeTools')

test_that("CompareQuartets works", {
  expect_equal(c(N=16L, Q=8L, s=1, d=2, r1=1, r2=1, u=3),
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

  expect_equal(c(N = 15 * 4, Q = 15, s_all = 0, s_any = 6, #1..4
                 d_all = 0, d_any = 3, r1_all = 0, r1_any = 3, # 5..8
                 r2_all = 3, r2_any = 9, u_all = 0, u_any = 6, x_only = 0),
               CompareQuartetsMulti(x = part, cf = list(bal, pec, rnd)))
  
  expect_equal(c(N = 15 * 4, Q = 15, s_all = 0, s_any = 0,
                 d_all = 3, d_any = 6, r1_all = 0, r1_any = 3,
                 r2_all = 3, r2_any = 9, u_all = 0, u_any = 6, x_only = 6),
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
