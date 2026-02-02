# Status of quartets that exist in two trees

Removes all tips that do not occur in both `tree1` and `tree2`, then
calculates the status of the remaining quartets.

## Usage

``` r
PairSharedQuartetStatus(tree1, tree2)
```

## Arguments

- tree1, tree2:

  Trees of class [`phylo`](https://rdrr.io/pkg/ape/man/read.tree.html)
  to compare.

## Value

Returns a named array of six integers corresponding to the quantities of
Estabrook *et al*. (1985):

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

## See also

Other element-by-element comparisons:
[`CompareQuartets()`](CompareQuartets.md),
[`CompareQuartetsMulti()`](CompareQuartetsMulti.md),
[`CompareSplits()`](CompareSplits.md),
[`QuartetState()`](QuartetState.md),
[`SharedQuartetStatus()`](QuartetStatus.md),
[`SplitStatus()`](SplitStatus.md)

## Author

[Martin R. Smith](https://orcid.org/0000-0001-5660-1727)
(<martin.smith@durham.ac.uk>)
