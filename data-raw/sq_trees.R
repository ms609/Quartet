suppressWarnings(RNGversion("3.5.0"))
set.seed(1) # 1 produces a random tree with a Qt dist of 226 ~= 330 * 2 / 3
ref_tree <- ape::read.tree(text="(((1, 2), 3), (((4, 5), 6), ((7, (8, 9)), (10, 11))));")
sq_trees <- structure(list (
  ref_tree      = ref_tree,
  move_one_near = ape::read.tree(text="(((2, 3), 1), (((4, 5), 6), ((7, (8, 9)), (10, 11))));"),
  move_one_mid  = ape::read.tree(text="((2, 3), ((((4, 5), 1), 6), ((7, (8, 9)), (10, 11))));"),
  move_one_far  = ape::read.tree(text="((2, 3), (((4, 5), 6), ((7, (8, 9)), (10, (11, 1)))));"),
  move_two_near = ape::read.tree(text="(((1, 2), 3), (((4, 5), 6), ((7, (10, 11)), (8, 9))));"),
  move_two_mid  = ape::read.tree(text="(((1, 2), 3), ((((4, 5), (10, 11)), 6), (7, (8, 9))));"),
  move_two_far  = ape::read.tree(text="((((1, (10, 11)), 2), 3), (((4, 5), 6), (7, (8, 9))));"),
  collapse_one  = ape::read.tree(text="(((1, 2), 3), (((4, 5), 6), ((7, 8, 9), (10, 11))));"),
  collapse_some = ape::read.tree(text="((1, 2, 3, 4, 5, 6), ((7, 8, 9), (10, 11)));"),
  m1mid_col1    = ape::read.tree(text="((2, 3), ((((4, 5), 1), 6), ((7, 8, 9), (10, 11))));"),
  m1mid_colsome = ape::read.tree(text="((2, 3), ((((4, 5), 1), 6), (7, 8, 9, 10, 11)));"),
  m2mid_col1    = ape::read.tree(text="(((1, 2), 3), ((((4, 5), (10, 11)), 6), (7, 8, 9)));"),
  m2mid_colsome = ape::read.tree(text="(((1, 2), 3), (4, (10, 11), 5, 6, 7, 8, 9));"),
  opposite_tree = ape::read.tree(text="(((1, 11), 3), (((4, 9), 6), ((10, (8, 2)), (5, 7))));"),
  caterpillar   = ape::read.tree(text="((((((1, 2), 3), 4), 5), 6), (7, (8, (9, (10, 11)))));"),
  top_and_tail  = ape::read.tree(text="((((((11, 2), 3), 4), 5), 6), (7, (8, (9, (10, 1)))));"),
  anti_pectinate = ape::read.tree(text="(9,((4,(6,((3,(1,10)),8))),(((2,11),5),7)));"),
  random_tree   = ape::rtree(n_tip, tip.label=seq_len(n_tip), br=NULL)
), class = 'multiPhylo')
usethis::use_data(sq_trees, overwrite=TRUE, compress='xz')
