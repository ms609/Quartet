# Compare status of splits

Reports whether splits are present or contradicted in a set of reference
splits.

## Usage

``` r
CompareSplits(splits, splits2)

CompareBipartitions(splits, splits2)
```

## Arguments

- splits:

  An object that can be coerced into class `Splits` using
  [`as.Splits`](https://ms609.github.io/TreeTools/reference/Splits.html).

- splits2:

  Splits against which to compare `splits`.

## Value

A named vector of eight integers, listing the number of unique splits
that:

- **N** exist in total; i.e. the number of splits in `splits1` plus the
  number in `splits2`, equivalent to 2 *s* + *d1* + *d2* + *r1* + *r2*;

- **P1** occur in `splits1`

- **P2** occur in `splits2`

- **s** occur in both `splits1` and `splits2`;

- **d1** occur in `splits1` but are contradicted by `splits2`;

- **d2** occur in `splits2` but are contradicted by `splits1`;

- **r1** occur in `splits1` only, being neither present in nor
  contradicted by `splits2`;

- **r2** occur in `splits2` only, being neither present in nor
  contradicted by `splits1`;

- **RF** occur in one tree only; i.e. *d1* + *d2* + *r1* + *r2*, the
  Robinson-Foulds distance.

## References

- Estabrook GF, McMorris FR, Meacham CA (1985). “Comparison of
  undirected phylogenetic trees based on subtrees of four evolutionary
  units.” *Systematic Zoology*, **34**(2), 193–200.
  [doi:10.2307/2413326](https://doi.org/10.2307/2413326) .

- Robinson DF, Foulds LR (1981). “Comparison of phylogenetic trees.”
  *Mathematical Biosciences*, **53**(1-2), 131–147.
  [doi:10.1016/0025-5564(81)90043-2](https://doi.org/10.1016/0025-5564%2881%2990043-2)
  .

## See also

Equivalent function for quartets:
[`CompareQuartets()`](CompareQuartets.md)

Other element-by-element comparisons:
[`CompareQuartets()`](CompareQuartets.md),
[`CompareQuartetsMulti()`](CompareQuartetsMulti.md),
[`PairSharedQuartetStatus()`](PairSharedQuartetStatus.md),
[`QuartetState()`](QuartetState.md),
[`SharedQuartetStatus()`](QuartetStatus.md),
[`SplitStatus()`](SplitStatus.md)

## Author

[Martin R. Smith](https://orcid.org/0000-0001-5660-1727)
(<martin.smith@durham.ac.uk>)

## Examples

``` r
splits1 <- TreeTools::BalancedTree(8)
splits2 <- TreeTools::PectinateTree(8)

CompareSplits(splits1, splits2)
#>  N P1 P2  s d1 d2 r1 r2 
#> 10  5  5  3  2  2  0  0 
        
```
