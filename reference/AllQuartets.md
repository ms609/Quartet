# List all quartets

Lists all choices of four taxa from a tree.

A more computationally efficient alternative to
[`combn`](https://rdrr.io/r/utils/combn.html).

## Usage

``` r
AllQuartets(nTips)

# S3 method for class 'numeric'
AllQuartets(nTips)

# S3 method for class 'phylo'
AllQuartets(nTips)
```

## Arguments

- nTips:

  Integer, specifying the number of tips in a tree; or a tree, whose
  tips will be counted.

## Value

`AllQuartets()` returns a matrix with four rows and `choose(n_tips, 4)`
columns, with each column corresponding to a unique selection of four
different integers less than or equal to `nTips`.

## See also

States of quartets in given trees: [`QuartetStates()`](QuartetState.md)

Other quartet counting functions:
[`CompareQuartets()`](CompareQuartets.md),
[`CompareQuartetsMulti()`](CompareQuartetsMulti.md),
[`ResolvedQuartets()`](ResolvedQuartets.md)

## Author

[Martin R. Smith](https://orcid.org/0000-0001-5660-1727)
(<martin.smith@durham.ac.uk>)

## Examples

``` r
AllQuartets(5)
#>      [,1] [,2] [,3] [,4] [,5]
#> [1,]    1    1    1    1    2
#> [2,]    2    2    2    3    3
#> [3,]    3    3    4    4    4
#> [4,]    4    5    5    5    5
 
combn(5, 4) # Provides the same information, but for large 
#>      [,1] [,2] [,3] [,4] [,5]
#> [1,]    1    1    1    1    2
#> [2,]    2    2    2    3    3
#> [3,]    3    3    4    4    4
#> [4,]    4    5    5    5    5
            # values of n_tips is significantly slower.
```
