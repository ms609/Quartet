# Count resolved quartets

Counts how many quartets are resolved or unresolved in a given tree,
following Brodal et al. (2013) .

## Usage

``` r
ResolvedQuartets(tree, countTriplets = FALSE)

ResolvedTriplets(tree)
```

## Arguments

- tree:

  A tree of class [`phylo`](https://rdrr.io/pkg/ape/man/read.tree.html).

- countTriplets:

  Logical; if `TRUE`, the function will return the number of triplets
  instead of the number of quartets.

## Value

`ResolvedQuartets()` returns a vector of length two, listing the number
of quartets (or triplets) that are `[1]` resolved; `[2]` unresolved in
the specified tree.

## Details

Trees with more than 477 leaves risk encountering integer overflow
errors, as the number of quartets is larger than can be stored in R's
signed 32-bit integer representation. If warnings are thrown, check
subsequent calculations for errors.

## Functions

- `ResolvedTriplets()`: Convenience function to calculate the number of
  resolved/unresolved triplets.

## References

Brodal GS, Fagerberg R, Mailund T, Pedersen CNS, Sand A (2013).
“Efficient algorithms for computing the triplet and quartet distance
between trees of arbitrary degree.” *SODA '13 Proceedings of the
Twenty-Fourth Annual ACM-SIAM Symposium on Discrete Algorithms*,
1814–1832.
[doi:10.1137/1.9781611973105.130](https://doi.org/10.1137/1.9781611973105.130)
.

## See also

Other quartet counting functions: [`AllQuartets()`](AllQuartets.md),
[`CompareQuartets()`](CompareQuartets.md),
[`CompareQuartetsMulti()`](CompareQuartetsMulti.md)

## Author

[Martin R. Smith](https://orcid.org/0000-0001-5660-1727)
(<martin.smith@durham.ac.uk>)

## Examples

``` r
data(sq_trees)

ResolvedTriplets(sq_trees$collapse_some)
#> [1] 144  21
# Equivalent to:
ResolvedQuartets(sq_trees$collapse_some, countTriplets = TRUE)
#> [1] 144  21

vapply(sq_trees, ResolvedQuartets, integer(2))
#>      ref_tree move_one_near move_one_mid move_one_far move_two_near
#> [1,]      330           330          330          330           330
#> [2,]        0             0            0            0             0
#>      move_two_mid move_two_far collapse_one collapse_some m1mid_col1
#> [1,]          330          330          322           207        322
#> [2,]            0            0            8           123          8
#>      m1mid_colsome m2mid_col1 m2mid_colsome opposite_tree caterpillar
#> [1,]           265        322           125           330         330
#> [2,]            65          8           205             0           0
#>      top_and_tail anti_pectinate random_tree
#> [1,]          330            330         330
#> [2,]            0              0           0

```
