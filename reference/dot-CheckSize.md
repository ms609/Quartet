# Check tree size

Trees with \> 477 leaves may have counts \> .Machine\$integer.max so
cannot be reliably evaluated.

## Usage

``` r
.CheckSize(tree)

# S3 method for class 'phylo'
.CheckSize(tree)

# S3 method for class 'list'
.CheckSize(tree)

# S3 method for class 'multiPhylo'
.CheckSize(tree)
```

## Details

It may be possible to increase this number to 568 by converting what R
represents as negative integers to the unsigned equivalent that is sent
from C.
