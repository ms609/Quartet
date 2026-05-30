# Quartet consensus (C++ implementation)

Quartet consensus (C++ implementation)

## Usage

``` r
cpp_quartet_consensus(
  splits_list,
  n_tips,
  init_majority,
  init_extended,
  greedy_best_flag
)
```

## Arguments

- splits_list:

  List of raw matrices (one per tree), from as.Splits().

- n_tips:

  Number of tips.

- init_majority:

  Logical: TRUE to start from majority-rule splits.

- init_extended:

  Logical: TRUE to start from extended majority splits.

- greedy_best_flag:

  Logical: TRUE for "best", FALSE for "first".

## Value

A list with `included` (logical), `raw_splits` (raw matrix), and
`light_side` (integer).
