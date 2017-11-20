source('dist_funcs.R')

## Note: result will average 1/3 for random trees
## Another note: > zero when n_tip > 6.
## consider ((1, 2), (3, (4, 5))).  Its only 0-scoring neighbours are symmetric with ((1, 5), (3, (2, 4)).
## Now add 6th tip sister to 3 on Tree 1.  There's nowhere to add this to its opposite without fulfilling
## a 4-statment.

### vapply(4:20, function (n_tip) {
###   trees <- lapply(1:1000, function (X) rtree(n_tip, tip.label=seq_len(n_tip), br=NULL))
###   results <- TetraDist(trees)[1, ] / choose(n_tip, 4)
###   c(mean(results[-1]), sd(results[-1]))
### }, double(2))
### 
### [,3]      [,4]      [,5]      [,6]       [,7]
### [1,] 0.3403403 0.3283283 0.3354021 0.3341913 0.3246389 0.3338974 0.33211783
### [2,] 0.4740609 0.2703599 0.1882786 0.1483874 0.1161134 0.1033415 0.08334102
###            [,8]       [,9]      [,10]     [,11]      [,12]      [,13]
### [1,] 0.33222920 0.33670034 0.33342713 0.3304613 0.33139733 0.33110583
### [2,] 0.07861774 0.07389063 0.06364083 0.0544294 0.05323172 0.04608159
###           [,14]      [,15]      [,16]      [,17]
### [1,] 0.33153069 0.33179650 0.33317373 0.33204908
### [2,] 0.04726896 0.04048472 0.04188577 0.03807628

test_trees <- list (
ref_tree = (ref_tree <- read.tree(text="(((1, 2), 3), (((4, 5), 6), ((7, (8, 9)), (10, 11))));")),
move_one_near = read.tree(text="(((2, 3), 1), (((4, 5), 6), ((7, (8, 9)), (10, 11))));"),
move_one_mid  = read.tree(text="((2, 3), ((((1, 4), 5), 6), ((7, (8, 9)), (10, 11))));"),
move_one_far  = read.tree(text="(((1, 2), 3), (((4, 5), 6), ((7, (10, 11)), (8, 9))));"),
move_two_near = read.tree(text="((2, 3), (((4, 5), 6), ((7, (8, 9)), (10, (11, 1)))));"),
move_two_mid  = read.tree(text="(((1, 2), 3), ((((4, (10, 11)), 5), 6), (7, (8, 9))));"),
move_two_far  = read.tree(text="((((1, (10, 11)), 2), 3), (((4, 5), 6), (7, (8, 9))));"),
collapse_one  = read.tree(text="(((1, 2), 3), (((4, 5), 6), ((7, 8, 9), (10, 11))));"),
collapse_som  = read.tree(text="((1, 2, 3, 4, 5, 6), ((7, 8, 9), (10, 11)));"),
m1mid_col1    = read.tree(text="((2, 3), ((((1, 4), 5), 6), ((7, 8, 9), (10, 11))));"),
m1mid_colsome = read.tree(text="((2, 3), ((((1, 4), 5), 6), (7, 8, 9, 10, 11)));"),
m2mid_col1    = read.tree(text="(((1, 2), 3), ((((4, (10, 11)), 5), 6), (7, 8, 9)));"),
m2mid_colsome = read.tree(text="(((1, 2), 3), ((4, (10, 11)), 5, 6, 7, 8, 9));"),
opposite_tree = read.tree(text="(((1, 11), 3), (((4, 9), 6), ((10, (8, 6)), (5, 7))));"),
random_tree = rtree(11, tip.label=1:11, br=NULL)
)
n_tip <- 11

tip_colours <- rainbow(42)[c(1, 3, 6, 11, 13, 16, 21, 24, 26, 31, 33, 38, 38)] #TODO Colourblind friendly
colplot <- function (tr, title=NULL) plot(tr, tip.col=tip_colours[as.double(tr$tip.label)], main=title)

par(mfrow=c(3,5), mar=rep(1.2, 4))
for (i in seq_along(test_trees)) colplot(test_trees[[i]], names(test_trees)[i])


dists <- matrix(NA, nrow=9, ncol=length(test_trees)); colnames(dists) <- names(test_trees)
rownames(dists) <- c('symmetric', 'path', 'R-F', 'uSPR', 'TBR', 'PH85', '4gram', '4gram_?', '4gram_X')
dists[1:2, ] <- vapply(test_trees, treedist, tree2=ref_tree, double(2))
dists[3, ] <- vapply(test_trees, RF.dist, tree2=ref_tree, double(1))
dists[4, ] <- c(0, 1, 1, 1, 1, 1, 1, 999)
dists[5, ] <- c(0, 1, 1, 1, 1, 1, 1, 999)
dists[6, ] <- vapply(test_trees, dist.topo, y=ref_tree, method='PH85', double(1))

tetra_solutions <- TetraDist(test_trees)
n_tetragrams <- choose(n_tip, 4)
dists[7:8, ] <- tetra_solutions / n_tetragrams * 100
dists[9, ] <- (n_tetragrams - colSums(tetra_solutions)) / n_tetragrams * 100
dists

TetraDist(list(
read.tree(text='((1, 2), ((3, 4), (6, 5)));'),
read.tree(text='((1, 5), (3, (4, (2, 6))));'))
)


