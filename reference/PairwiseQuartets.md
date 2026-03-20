# Pairwise quartet distances

Computes the quartet distance between each pair of trees in a list.

## Usage

``` r
PairwiseQuartets(trees, Measure = QuartetDivergence)
```

## Arguments

- trees:

  A list of trees of class
  [`phylo`](https://rdrr.io/pkg/ape/man/read.tree.html), with
  identically labelled tips.

- Measure:

  a function that calculates tree similarity or difference from quartet
  statuses. Default is
  [`QuartetDivergence()`](https://ms609.github.io/Quartet/reference/SimilarityMetrics.md).

## Value

a matrix specifying the distance between each tree and each other tree
in the `trees`.

## References

There are no references for Rd macro `\insertAllCites` on this help
page.

## See also

- Use splits (groups/clades defined by nodes or edges of the tree)
  instead of quartets as the unit of comparison:
  [`SplitStatus()`](https://ms609.github.io/Quartet/reference/SplitStatus.md).

- Generate distance metrics from quartet statuses:
  [`SimilarityMetrics()`](https://ms609.github.io/Quartet/reference/SimilarityMetrics.md).

Other element-by-element comparisons:
[`CompareQuartets()`](https://ms609.github.io/Quartet/reference/CompareQuartets.md),
[`CompareQuartetsMulti()`](https://ms609.github.io/Quartet/reference/CompareQuartetsMulti.md),
[`CompareSplits()`](https://ms609.github.io/Quartet/reference/CompareSplits.md),
[`PairSharedQuartetStatus()`](https://ms609.github.io/Quartet/reference/PairSharedQuartetStatus.md),
[`QuartetState()`](https://ms609.github.io/Quartet/reference/QuartetState.md),
[`SharedQuartetStatus()`](https://ms609.github.io/Quartet/reference/QuartetStatus.md),
[`SplitStatus()`](https://ms609.github.io/Quartet/reference/SplitStatus.md)

## Author

[Martin R. Smith](https://orcid.org/0000-0001-5660-1727)
(<martin.smith@durham.ac.uk>)

## Examples

``` r
data("sq_trees")
# Calculate the status of each quartet relative to the first entry in 
# sq_trees
sq_status <- QuartetStatus(sq_trees)

# Calculate Estabrook et al's similarity measures:
SimilarityMetrics(sq_status)
#>                DoNotConflict ExplicitlyAgree StrictJointAssertions
#> ref_tree           1.0000000       1.0000000             1.0000000
#> move_one_near      0.9757576       0.9757576             0.9757576
#> move_one_mid       0.8424242       0.8424242             0.8424242
#> move_one_far       0.7696970       0.7696970             0.7696970
#> move_two_near      0.9272727       0.9272727             0.9272727
#> move_two_mid       0.7636364       0.7636364             0.7636364
#> move_two_far       0.7212121       0.7212121             0.7212121
#> collapse_one       1.0000000       0.9757576             1.0000000
#> collapse_some      1.0000000       0.6272727             1.0000000
#> m1mid_col1         0.8424242       0.8181818             0.8385093
#> m1mid_colsome      0.8424242       0.6454545             0.8037736
#> m2mid_col1         0.7636364       0.7393939             0.7577640
#> m2mid_colsome      1.0000000       0.3787879             1.0000000
#> opposite_tree      0.2606061       0.2606061             0.2606061
#> caterpillar        0.7393939       0.7393939             0.7393939
#> top_and_tail       0.3696970       0.3696970             0.3696970
#> anti_pectinate     0.2575758       0.2575758             0.2575758
#> random_tree        0.3151515       0.3151515             0.3151515
#>                SemiStrictJointAssertions SymmetricDifference
#> ref_tree                       1.0000000           1.0000000
#> move_one_near                  0.9757576           0.9757576
#> move_one_mid                   0.8424242           0.8424242
#> move_one_far                   0.7696970           0.7696970
#> move_two_near                  0.9272727           0.9272727
#> move_two_mid                   0.7636364           0.7636364
#> move_two_far                   0.7212121           0.7212121
#> collapse_one                   1.0000000           0.9877301
#> collapse_some                  1.0000000           0.7709497
#> m1mid_col1                     0.8385093           0.8282209
#> m1mid_colsome                  0.8037736           0.7159664
#> m2mid_col1                     0.7577640           0.7484663
#> m2mid_colsome                  1.0000000           0.5494505
#> opposite_tree                  0.2606061           0.2606061
#> caterpillar                    0.7393939           0.7393939
#> top_and_tail                   0.3696970           0.3696970
#> anti_pectinate                 0.2575758           0.2575758
#> random_tree                    0.3151515           0.3151515
#>                MarczewskiSteinhaus SteelPenny QuartetDivergence
#> ref_tree                 1.0000000  1.0000000         1.0000000
#> move_one_near            0.9526627  0.9757576         0.9757576
#> move_one_mid             0.7277487  0.8424242         0.8424242
#> move_one_far             0.6256158  0.7696970         0.7696970
#> move_two_near            0.8644068  0.9272727         0.9272727
#> move_two_mid             0.6176471  0.7636364         0.7636364
#> move_two_far             0.5639810  0.7212121         0.7212121
#> collapse_one             0.9757576  0.9757576         0.9878788
#> collapse_some            0.6272727  0.6272727         0.8136364
#> m1mid_col1               0.7068063  0.8181818         0.8303030
#> m1mid_colsome            0.5575916  0.6454545         0.7439394
#> m2mid_col1               0.5980392  0.7393939         0.7515152
#> m2mid_colsome            0.3787879  0.3787879         0.6893939
#> opposite_tree            0.1498258  0.2606061         0.2606061
#> caterpillar              0.5865385  0.7393939         0.7393939
#> top_and_tail             0.2267658  0.3696970         0.3696970
#> anti_pectinate           0.1478261  0.2575758         0.2575758
#> random_tree              0.1870504  0.3151515         0.3151515
#>                SimilarityToReference
#> ref_tree                   1.0000000
#> move_one_near              0.9757576
#> move_one_mid               0.8424242
#> move_one_far               0.7696970
#> move_two_near              0.9272727
#> move_two_mid               0.7636364
#> move_two_far               0.7212121
#> collapse_one               0.9838384
#> collapse_some              0.7515152
#> m1mid_col1                 0.8262626
#> m1mid_colsome              0.7111111
#> m2mid_col1                 0.7474747
#> m2mid_colsome              0.5858586
#> opposite_tree              0.2606061
#> caterpillar                0.7393939
#> top_and_tail               0.3696970
#> anti_pectinate             0.2575758
#> random_tree                0.3151515

# Compare trees that include a subset of the taxa 1..10
library("TreeTools", quietly = TRUE, warn.conflict = FALSE)
QuartetStatus(BalancedTree(1:5), BalancedTree(3:8), nTip = 10)
#>        N   Q s d r1 r2   u
#> [1,] 420 210 0 0  5 15 190

# If all taxa studied occur in `trees` or `cf`, set `nTip = TRUE`
QuartetStatus(BalancedTree(1:5), BalancedTree(3:10), nTip = TRUE)
#>        N   Q s d r1 r2   u
#> [1,] 420 210 0 0  5 70 135
 
```
