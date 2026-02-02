# Matching partitions

Calculates how many of the partitions present in tree 1 are also present
in tree 2 (`s`), how many of the partitions in tree 1 are absent in tree
2 (`d1`), and how many of the partitions in tree 2 are absent in tree 1
(`d2`). The Robinson-Foulds (symmetric partition) distance is the sum of
the latter two quantities, i.e. `d1` + `d2`.

## Usage

``` r
SplitStatus(trees, cf = trees[[1]])

SharedSplitStatus(trees, cf)
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

## Value

Returns a two dimensional array. Rows correspond to the input trees, and
are named if names were present. Columns report:

**N**: The total number of partitions present in the two trees, i.e.
*P1* + *P2*.

**P1**: The number of partitions present in tree 1.

**P2**: The number of partitions present in tree 2.

**s**: The number of partitions present in both trees.

**d1**: The number of partitions present in tree 1, but contradicted by
tree 2.

**d2**: The number of partitions present in tree 2, but contradicted by
tree 1.

**r1**: The number of partitions present in tree 1, and neither present
nor contradicted in tree 2.

**r2**: The number of partitions present in tree 2, and neither present
nor contradicted in tree 1.

## Functions

- `SharedSplitStatus()`: Reports split statistics obtained after
  removing all tips that do not occur in both trees being compared.

## References

- Robinson DF, Foulds LR (1981). “Comparison of phylogenetic trees.”
  *Mathematical Biosciences*, **53**(1-2), 131–147.
  [doi:10.1016/0025-5564(81)90043-2](https://doi.org/10.1016/0025-5564%2881%2990043-2)
  .

- Penny D, Hendy MD (1985). “The use of tree comparison metrics.”
  *Systematic Zoology*, **34**(1), 75–82.
  [doi:10.2307/2413347](https://doi.org/10.2307/2413347) .

## See also

Other element-by-element comparisons:
[`CompareQuartets()`](CompareQuartets.md),
[`CompareQuartetsMulti()`](CompareQuartetsMulti.md),
[`CompareSplits()`](CompareSplits.md),
[`PairSharedQuartetStatus()`](PairSharedQuartetStatus.md),
[`QuartetState()`](QuartetState.md),
[`SharedQuartetStatus()`](QuartetStatus.md)

## Author

[Martin R. Smith](https://orcid.org/0000-0001-5660-1727)
(<martin.smith@durham.ac.uk>)

## Examples

``` r
data("sq_trees")

# Calculate the status of each quartet
splitStatuses <- SplitStatus(sq_trees)

# Calculate the raw symmetric difference (i.e. Robinson–Foulds distance)
RawSymmetricDifference(splitStatuses)
#>       ref_tree  move_one_near   move_one_mid   move_one_far  move_two_near 
#>              0              2              6              8              2 
#>   move_two_mid   move_two_far   collapse_one  collapse_some     m1mid_col1 
#>              4              6              1              5              7 
#>  m1mid_colsome     m2mid_col1  m2mid_colsome  opposite_tree    caterpillar 
#>              9              5              5             16              8 
#>   top_and_tail anti_pectinate    random_tree 
#>             16             16             16 

# Normalize the Robinson Foulds distance by dividing by the number of 
# splits present in the two trees:
RawSymmetricDifference(splitStatuses) / splitStatuses[, "N"]
#>       ref_tree  move_one_near   move_one_mid   move_one_far  move_two_near 
#>     0.00000000     0.12500000     0.37500000     0.50000000     0.12500000 
#>   move_two_mid   move_two_far   collapse_one  collapse_some     m1mid_col1 
#>     0.25000000     0.37500000     0.06666667     0.45454545     0.46666667 
#>  m1mid_colsome     m2mid_col1  m2mid_colsome  opposite_tree    caterpillar 
#>     0.69230769     0.33333333     0.45454545     1.00000000     0.50000000 
#>   top_and_tail anti_pectinate    random_tree 
#>     1.00000000     1.00000000     1.00000000 

# Normalize the Robinson Foulds distance by dividing by the total number of 
# splits that it is possible to resolve for `n` tips:
nTip <- length(sq_trees[[1]]$tip.label)
nPartitions <- 2 * (nTip - 3L) # Does not include the nTip partitions that 
                               # comprise but a single tip
RawSymmetricDifference(splitStatuses) / nPartitions
#>       ref_tree  move_one_near   move_one_mid   move_one_far  move_two_near 
#>         0.0000         0.1250         0.3750         0.5000         0.1250 
#>   move_two_mid   move_two_far   collapse_one  collapse_some     m1mid_col1 
#>         0.2500         0.3750         0.0625         0.3125         0.4375 
#>  m1mid_colsome     m2mid_col1  m2mid_colsome  opposite_tree    caterpillar 
#>         0.5625         0.3125         0.3125         1.0000         0.5000 
#>   top_and_tail anti_pectinate    random_tree 
#>         1.0000         1.0000         1.0000 

```
