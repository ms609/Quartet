# Status vector to matrix

Converts a vector to a matrix that can be analysed by the
[`DoNotConflict()`](SimilarityMetrics.md) function family.

## Usage

``` r
.StatusToMatrix(statusVector)

.StatusToArray(status)
```

## Arguments

- statusVector:

  Either (i) a named vector of integers, with names `N`, `s`, `r1`,
  `r2`, either `d` or `d1` and `d2`, and optionally `u`; or (ii) a
  matrix whose named rows correspond to the same quantities.

- status:

  A named three-dimensional array of integers, with slices named `s`,
  `r1`, `r2`, either `d` or `d1` and `d2`, and either `N` or `u`.

## Value

A matrix, containing the input columns plus `2d`, representing either
`2 * d` or `d1 + d2`, and row names.

The row name means that column names are dropped in the output of
`DoNotConflict` etc.

A three-dimensional array containing a slice labelled `2d`, equivalent
to either `d + d` or `d1 + d2` as appropriate.

## Author

[Martin R. Smith](https://orcid.org/0000-0001-5660-1727)
(<martin.smith@durham.ac.uk>)

## Examples

``` r
data("sq_trees")

.StatusToArray(ManyToManyQuartetAgreement(sq_trees[5:7]))
#> , , N
#> 
#>               move_two_near move_two_mid move_two_far
#> move_two_near           660          660          660
#> move_two_mid            660          660          660
#> move_two_far            660          660          660
#> 
#> , , Q
#> 
#>               move_two_near move_two_mid move_two_far
#> move_two_near           330          330          330
#> move_two_mid            330          330          330
#> move_two_far            330          330          330
#> 
#> , , s
#> 
#>               move_two_near move_two_mid move_two_far
#> move_two_near           330          228          214
#> move_two_mid            228          330          214
#> move_two_far            214          214          330
#> 
#> , , d
#> 
#>               move_two_near move_two_mid move_two_far
#> move_two_near             0          102          116
#> move_two_mid            102            0          116
#> move_two_far            116          116            0
#> 
#> , , r1
#> 
#>               move_two_near move_two_mid move_two_far
#> move_two_near             0            0            0
#> move_two_mid              0            0            0
#> move_two_far              0            0            0
#> 
#> , , r2
#> 
#>               move_two_near move_two_mid move_two_far
#> move_two_near             0            0            0
#> move_two_mid              0            0            0
#> move_two_far              0            0            0
#> 
#> , , u
#> 
#>               move_two_near move_two_mid move_two_far
#> move_two_near             0            0            0
#> move_two_mid              0            0            0
#> move_two_far              0            0            0
#> 
#> , , 2d
#> 
#>               move_two_near move_two_mid move_two_far
#> move_two_near             0          204          232
#> move_two_mid            204            0          232
#> move_two_far            232          232            0
#> 
```
