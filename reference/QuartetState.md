# Quartet State(s)

Report the status of the specified quartet(s) in given trees or lists of
splits (Estabrook et al. 1985) .

## Usage

``` r
QuartetState(tips, bips, splits = bips, asRaw = FALSE)

QuartetStates(splits, asRaw = FALSE)

# S3 method for class 'Splits'
QuartetStates(splits, asRaw = FALSE)

# S3 method for class 'list'
QuartetStates(splits, asRaw = FALSE)

# S3 method for class 'multiPhylo'
QuartetStates(splits, asRaw = FALSE)
```

## Arguments

- tips:

  A four-element array listing a quartet of leaves, either by their
  number (if class `numeric`) or their name (if class `character`).

- bips:

  Deprecated; included for compatibility with v1.0.2 and below.

- splits:

  An object, such as a tree of class `phylo`, that can be induced to a
  `Splits` object using
  [`as.Splits`](https://ms609.github.io/TreeTools/reference/Splits.html).

- asRaw:

  Logical specifying whether return format should be `raw`, which uses
  less memory and can be processed faster than `integer` type. Default
  is currently set to `FALSE` for backwards compatibility; suggest
  overriding to `TRUE`.

## Value

`QuartetState()` returns `0` if the relationships of the four leaves are
not constrained by the provided splits, or the index of the closest
relative to `tips[4]`, otherwise.

`QuartetStates()` returns a raw vector listing the status of each
quartet of leaves (in the order listed by
[`AllQuartets()`](AllQuartets.md)) in turn, or if multiple trees are
provided, a matrix in which each row corresponds to such a vector.

## Details

One of the three possible four-leaf trees will be consistent with any
set of splits generated from a fully resolved tree. If the leaves are
numbered 1 to 4, this tree can be identified by naming the leaf most
closely related to leaf 4. If a set of splits is generated from a tree
that contains polytomies, it is possible that all three four-leaf trees
are consistent with the set of splits.

## References

Estabrook GF, McMorris FR, Meacham CA (1985). “Comparison of undirected
phylogenetic trees based on subtrees of four evolutionary units.”
*Systematic Zoology*, **34**(2), 193–200.
[doi:10.2307/2413326](https://doi.org/10.2307/2413326) .

## See also

Compare quartet states between trees (slowly) using
[`CompareQuartets()`](CompareQuartets.md) and
[`CompareQuartetsMulti()`](CompareQuartetsMulti.md).

Other element-by-element comparisons:
[`CompareQuartets()`](CompareQuartets.md),
[`CompareQuartetsMulti()`](CompareQuartetsMulti.md),
[`CompareSplits()`](CompareSplits.md),
[`PairSharedQuartetStatus()`](PairSharedQuartetStatus.md),
[`SharedQuartetStatus()`](QuartetStatus.md),
[`SplitStatus()`](SplitStatus.md)

## Author

[Martin R. Smith](https://orcid.org/0000-0001-5660-1727)
(<martin.smith@durham.ac.uk>)

## Examples

``` r
trees <- list(TreeTools::BalancedTree(6),
              TreeTools::PectinateTree(6))

trees[[3]] <- TreeTools::CollapseNode(trees[[2]], 9:10)

QuartetState(c(1, 3, 4, 6), trees[[2]])  
#> [1] 3
QuartetState(1:4, trees[[1]]) == QuartetState(1:4, trees[[2]])
#> [1] TRUE
QuartetState(c(1, 3, 4, 6), trees[[3]])  
#> [1] 0

QuartetStates(trees[[2]])
#>  [1] 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3
QuartetStates(trees[[3]])
#>  [1] 0 0 0 0 0 3 0 0 3 3 0 0 3 3 3

CompareQuartets(QuartetStates(trees[[2]]), QuartetStates(trees[[3]]))
#>  N  Q  s  d r1 r2  u 
#> 30 15  6  0  9  0  0 
CompareQuartetsMulti(trees[[1]], trees[2:3])
#>      N      Q  s_all  s_any  d_all  d_any r1_all r1_any r2_all r2_any  u_all 
#>     45     15      3     12      3      3      0      9      0      0      0 
#>  u_any x_only 
#>      0      3 
```
