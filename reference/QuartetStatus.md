# Status of quartets

Determines the number of quartets that are consistent within pairs of
trees.

## Usage

``` r
SharedQuartetStatus(trees, cf = trees[[1]])

QuartetStatus(trees, cf = trees[[1]], nTip = NULL)

ManyToManyQuartetAgreement(trees, nTip = NULL)

TwoListQuartetAgreement(trees1, trees2)

SingleTreeQuartetAgreement(trees, comparison)
```

## Arguments

- trees:

  A list of trees of class
  [`phylo`](https://rdrr.io/pkg/ape/man/read.tree.html), with
  identically labelled tips.

- cf:

  Comparison tree of class
  [`phylo`](https://rdrr.io/pkg/ape/man/read.tree.html). If unspecified,
  each tree is compared to the first tree in `trees`.

- nTip:

  Integer specifying number of tips that could have occurred in `trees`.
  Useful if comparing trees from different data sources that contain
  non-overlapping tips. If `NULL`, the default, then trees are assumed
  to contain the same tips. If `TRUE`, then a vector is generated
  automatically by counting all unique tip labels found in `trees` or
  `cf`.

- trees1, trees2:

  List or `multiPhylo` objects containing trees of class `phylo`.

- comparison:

  A tree of class [`phylo`](https://rdrr.io/pkg/ape/man/read.tree.html)
  against which to compare `trees`.

## Value

`QuartetStatus()` returns a two dimensional array. Rows correspond to
the input trees; the first row will report a perfect match if the first
tree is specified as the comparison tree (or if `cf` is not specified).
Columns list the status of each quartet:

- N:

  The total number of quartet *statements* for two trees of *n* leaves,
  i.e. 2 *Q*.

- Q:

  The total number of quartets for *n* leaves.

- s:

  The number of quartets that are resolved identically in both trees.

- d:

  The number of quartets that are resolved differently in each tree.

- r1:

  The number of quartets that are resolved in tree 1, but not in tree 2.

- r2:

  The number of quartets that are resolved in tree 2, but not in tree 1.

- u:

  The number of quartets that are unresolved in both trees.

`ManyToManyQuartetAgreement()` returns a three-dimensional array
listing, for each pair of trees in turn, the number of quartets in each
category.

`TwoListQuartetAgreement()` returns a three-dimensional array listing,
for each pair of trees in turn, the number of quartets in each category.

`SingleTreeQuartetAgreement()` returns a two-dimensional array listing,
for tree in `trees`, the total number of quartets and the number of
quartets in each category. The `comparison` tree is treated as `tree2`.

## Details

Given a list of trees, returns the number of quartet statements
(Estabrook et al. 1985) present in the reference tree (the first entry
in `trees`, if `cf` is not specified) that are also present in each
other tree. A random pair of fully resolved trees is expected to share
`choose(n_tip, 4) / 3` quartets.

If trees do not bear the same number of tips, `SharedQuartetStatus()`
will consider only the quartets that include taxa common to both trees.

From this information it is possible to calculate how many of all
possible quartets occur in one tree or the other, though there is not
yet a function calculating this; [let us
know](https://github.com/ms609/Quartet/issues/new) if you would
appreciate this functionality.

The status of each quartet is calculated using the algorithms of Brodal
et al. (2013) and Holt et al. (2014) , implemented in the tqdist C
library (Sand et al. 2014) .

## Functions

- `SharedQuartetStatus()`: Reports split statistics obtained after
  removing all tips that do not occur in both trees being compared.

- `ManyToManyQuartetAgreement()`: Agreement of each quartet, comparing
  each pair of trees in a list.

- `TwoListQuartetAgreement()`: Agreement of each quartet in trees in one
  list with each quartet in trees in a second list.

- `SingleTreeQuartetAgreement()`: Agreement of each quartet in trees in
  a list with the quartets in a comparison tree.

## References

Brodal GS, Fagerberg R, Mailund T, Pedersen CNS, Sand A (2013).
“Efficient algorithms for computing the triplet and quartet distance
between trees of arbitrary degree.” *SODA '13 Proceedings of the
Twenty-Fourth Annual ACM-SIAM Symposium on Discrete Algorithms*,
1814–1832.
[doi:10.1137/1.9781611973105.130](https://doi.org/10.1137/1.9781611973105.130)
.  
  
Estabrook GF, McMorris FR, Meacham CA (1985). “Comparison of undirected
phylogenetic trees based on subtrees of four evolutionary units.”
*Systematic Zoology*, **34**(2), 193–200.
[doi:10.2307/2413326](https://doi.org/10.2307/2413326) .  
  
Holt MK, Johansen J, Brodal GS (2014). “On the scalability of computing
triplet and quartet distances.” In *Proceedings of 16th Workshop on
Algorithm Engineering and Experiments (ALENEX) Portland, Oregon, USA*.  
  
Sand A, Holt MK, Johansen J, Brodal GS, Mailund T, Pedersen CNS (2014).
“tqDist: a library for computing the quartet and triplet distances
between binary or general trees.” *Bioinformatics*, **30**(14),
2079–2080. ISSN 1460-2059,
[doi:10.1093/bioinformatics/btu157](https://doi.org/10.1093/bioinformatics/btu157)
.

## See also

- Use splits (groups/clades defined by nodes or edges of the tree)
  instead of quartets as the unit of comparison:
  [`SplitStatus()`](SplitStatus.md).

- Generate distance metrics from quartet statuses:
  [`SimilarityMetrics()`](SimilarityMetrics.md).

Other element-by-element comparisons:
[`CompareQuartets()`](CompareQuartets.md),
[`CompareQuartetsMulti()`](CompareQuartetsMulti.md),
[`CompareSplits()`](CompareSplits.md),
[`PairSharedQuartetStatus()`](PairSharedQuartetStatus.md),
[`QuartetState()`](QuartetState.md), [`SplitStatus()`](SplitStatus.md)

## Author

[Martin R. Smith](https://orcid.org/0000-0001-5660-1727)
(<martin.smith@durham.ac.uk>)

## Examples

``` r
data("sq_trees")
# Calculate the status of each quartet relative to the first entry in 
# sq_trees
sq_status <- QuartetStatus(sq_trees)

# Calculate the status of each quartet relative to a given tree
two_moved <- sq_trees[5:7]
sq_status <- QuartetStatus(two_moved, sq_trees$ref_tree)

# Calculate Estabrook et al's similarity measures:
SimilarityMetrics(sq_status)
#>               DoNotConflict ExplicitlyAgree StrictJointAssertions
#> move_two_near     0.9272727       0.9272727             0.9272727
#> move_two_mid      0.7636364       0.7636364             0.7636364
#> move_two_far      0.7212121       0.7212121             0.7212121
#>               SemiStrictJointAssertions SymmetricDifference MarczewskiSteinhaus
#> move_two_near                 0.9272727           0.9272727           0.8644068
#> move_two_mid                  0.7636364           0.7636364           0.6176471
#> move_two_far                  0.7212121           0.7212121           0.5639810
#>               SteelPenny QuartetDivergence SimilarityToReference
#> move_two_near  0.9272727         0.9272727             0.9272727
#> move_two_mid   0.7636364         0.7636364             0.7636364
#> move_two_far   0.7212121         0.7212121             0.7212121

# Compare trees that include a subset of the taxa 1..10
library("TreeTools", quietly = TRUE, warn.conflict = FALSE)
QuartetStatus(BalancedTree(1:5), BalancedTree(3:8), nTip = 10)
#>        N   Q s d r1 r2   u
#> [1,] 420 210 0 0  5 15 190

# If all taxa studied occur in `trees` or `cf`, set `nTip = TRUE`
QuartetStatus(BalancedTree(1:5), BalancedTree(3:10), nTip = TRUE)
#>        N   Q s d r1 r2   u
#> [1,] 420 210 0 0  5 70 135
 
# Calculate Quartet Divergence between each tree and each other tree in a 
# list
QuartetDivergence(ManyToManyQuartetAgreement(two_moved))
#>               move_two_near move_two_mid move_two_far
#> move_two_near     1.0000000    0.6909091    0.6484848
#> move_two_mid      0.6909091    1.0000000    0.6484848
#> move_two_far      0.6484848    0.6484848    1.0000000
# Calculate Quartet Divergence between each tree in one list and each 
# tree in another
QuartetDivergence(TwoListQuartetAgreement(sq_trees[1:3], sq_trees[10:13]))
#>               m1mid_col1 m1mid_colsome m2mid_col1 m2mid_colsome
#> ref_tree       0.8303030     0.7439394  0.7515152     0.6893939
#> move_one_near  0.8545455     0.7681818  0.7272727     0.6651515
#> move_one_mid   0.9878788     0.9015152  0.6060606     0.5621212
```
