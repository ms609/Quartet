# Compare one tree's quartets against others'

`CompareQuartetsMulti()` counts how many quartets in one tree are
resolved in the same way or different ways in a forest of comparison
trees.

## Usage

``` r
CompareQuartetsMulti(x, cf)
```

## Arguments

- x:

  Object of class `phylo` representing the tree of interest.

- cf:

  Comparison tree of class `phylo`, or list thereof, each with the same
  leaves as `x`.

## Value

`CompareQuartetsMulti()` returns a named integer vector specifying the
number of quartets whose resolution in `x` matches all or any of the
resolutions in `cf`.

Named elements are:

- N:

  The total number of quartet *statements* for the given number of
  *n*-leaf trees, i.e. *n_trees* × *Q*.

- Q:

  The total number of quartets for *n* leaves.

- s_all:

  The number of quartets that are resolved identically in all trees.

- s_any:

  The number of quartets that are resolved in `x`, and identically in at
  least one of `cf`.

- d_all:

  The number of quartets that are resolved in every tree in `cf`, but
  never in the same way as they are resolved in in `x`.

- d_any:

  The number of quartets in `x` that are resolved differently (i.e.
  contradicted) in at least one tree in `cf`.

- r1_all:

  The number of quartets that are resolved in `x`, but not in any of
  `cf`.

- r1_any:

  The number of quartets that are resolved in `x`, but unresolved in at
  least one of `cf`.

- r2_all:

  The number of quartets that are resolved in all of `cf`, but not in
  `x`.

- r2_any:

  The number of quartets that are resolved in at least one of `cf`, but
  not in `x`.

- u_all:

  The number of quartets that are unresolved in all trees.

- u_any:

  The number of quartets that are unresolved in `x` and at least one
  tree in `cf`.

- x_only:

  The number of quartets in `x` that are not resolved the same way in
  any of `cf`.

## Details

`CompareQuartetsMulti()` explicitly evaluates each quartet in each tree.
As such its runtime will increase hyper-exponentially with the number of
leaves in trees being compared. 30 leaves will take around 5 seconds; 40
closer to 20 s, and 50 around a minute.

## See also

Other element-by-element comparisons:
[`CompareQuartets()`](CompareQuartets.md),
[`CompareSplits()`](CompareSplits.md),
[`PairSharedQuartetStatus()`](PairSharedQuartetStatus.md),
[`QuartetState()`](QuartetState.md),
[`SharedQuartetStatus()`](QuartetStatus.md),
[`SplitStatus()`](SplitStatus.md)

Other quartet counting functions: [`AllQuartets()`](AllQuartets.md),
[`CompareQuartets()`](CompareQuartets.md),
[`ResolvedQuartets()`](ResolvedQuartets.md)

## Author

[Martin R. Smith](https://orcid.org/0000-0001-5660-1727)
(<martin.smith@durham.ac.uk>)

## Examples

``` r
library("TreeTools")
CompareQuartetsMulti(x  = CollapseNode(as.phylo(42, 6), 8:9),
                     cf = list(BalancedTree(6), PectinateTree(6), 
                               CollapseNode(as.phylo(1337, 6), 9:10)))
#>      N      Q  s_all  s_any  d_all  d_any r1_all r1_any r2_all r2_any  u_all 
#>     60     15      0      4      4      9      0      7      1      3      0 
#>  u_any x_only 
#>      2      8 
```
