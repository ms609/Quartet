# Pair shared split status

Removes all tips that do not occur in both `ref` and `cf`, then
calculates the status of the remaining splits.

## Usage

``` r
PairSharedSplitStatus(ref, cf)
```

## Arguments

- ref, cf:

  Trees of class [`phylo`](https://rdrr.io/pkg/ape/man/read.tree.html)
  to compare.

## Value

Named integer of length 6, as per [`CompareSplits()`](CompareSplits.md)

## Author

[Martin R. Smith](https://orcid.org/0000-0001-5660-1727)
(<martin.smith@durham.ac.uk>)

## Examples

``` r
library("TreeTools")
ref <- BalancedTree(letters[1:9])
cf <- BalancedTree(letters[3:13])

PairSharedSplitStatus(ref, cf)
#>  N P1 P2  s d1 d2 r1 r2 
#>  8  4  4  2  2  2  0  0 
```
