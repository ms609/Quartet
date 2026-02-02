# Plot contours of equal symmetric difference on a ternary plot

Assumes that tree 1 is perfectly resolved, but that the resolution of
tree 2 can vary.

## Usage

``` r
SymmetricDifferenceLineEnds(nsd)

SymmetricDifferenceLines(nsd, ...)
```

## Arguments

- nsd:

  Vector specifying normalized symmetric differences to plot.

- ...:

  Further parameters to pass to
  [`TernaryLines()`](https://ms609.github.io/Ternary/reference/AddToTernary.html).

## Value

Returns a matrix of dim `(length(nsd), 6)`, with columns named `r2a`,
`da`, `sa`, `r2b`, `db` and `sb`. Lines from `a` to `b` in each row
connect points of equal symmetric difference.

## Functions

- `SymmetricDifferenceLines()`: Plot the lines onto the active ternary
  plot.

## Author

[Martin R. Smith](https://orcid.org/0000-0001-5660-1727)
(<martin.smith@durham.ac.uk>)
