# Normalize element statuses to generate metric

Handles vectors and matrices of two or three dimensions.

## Usage

``` r
.NormalizeStatus(elementStatus, numerator, denominator, takeFromOne)
```

## Arguments

- elementStatus:

  Two-dimensional integer array, with rows corresponding to counts of
  matching quartets or partitions for each tree, and columns named
  according to the output of [`QuartetStatus()`](QuartetStatus.md) or
  [`SplitStatus()`](SplitStatus.md).

- numerator, denominator:

  Character vector listing elements to sum in numerator / denominator.

- takeFromOne:

  Logical specifying whether to deduct value from one.
