# Eighteen example trees

A list of class
[`multiPhylo`](https://rdrr.io/pkg/ape/man/multiphylo.html) containing
phylogenetic trees:

- `ref_tree`:

  A reference tree, bearing tips labelled 1 to 11.

- `move_one_near`:

  Tip 1 has been moved a short distance.

- `move_one_mid`:

  Tip 1 has been moved further.

- `move_one_far`:

  Tip 1 has been moved further still.

- `move_two_near`:

  Tips 10 & 11 have been moved a short distance.

- `move_two_mid`:

  Tips 10 & 11 have been moved further.

- `move_two_far`:

  Tips 10 & 11 have been moved further still.

- `collapse_one`:

  One node has been collapsed into a polytomy.

- `collapse_some`:

  Several nodes have been collapsed.

- `m1mid_col1`:

  Tree `move_one_mid` with one node collapsed.

- `m1mid_colsome`:

  Tree `move_one_mid` with several nodes collapsed.

- `m2mid_col1`:

  Tree `move_two_mid` with one node collapsed.

- `m2mid_colsome`:

  Tree `move_two_mid` with several nodes collapsed.

- `opposite_tree`:

  A tree that shares fewer quartets with `ref_tree` than expected by
  chance.

- `caterpillar`:

  A pectinate "caterpillar" tree.

- `top_and_tail`:

  Tree `caterpillar`, with its outermost taxa swapped such that it
  shares no partitions with `caterpillar`.

- `anti_pectinate`:

  A random tree that shares no partitions with `caterpillar`.

- `random_tree`:

  A random tree.

## Usage

``` r
sq_trees
```

## Format

An object of class `multiPhylo` of length 18.
