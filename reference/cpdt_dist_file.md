# Direct entry points to 'cpdt' functions

Functions to calculate rooted triplet distances between pairs of trees.
Input is not checked for sanity.

## Usage

``` r
cpdt_dist_file(file1, file2)
```

## Arguments

- file1, file2:

  Paths to files containing a tree or trees in Newick format.

## Value

The distance between the requested trees.

## Details

Functions are called from R with functions such as
[`TripletDistance`](https://ms609.github.io/Quartet/reference/Distances.md).

## References

Jansson J, Rajaby R (2017). “A More Practical Algorithm for the Rooted
Triplet Distance.” *Journal of Computational Biology*, **24**(2),
106–126.
[doi:10.1089/cmb.2016.0185](https://doi.org/10.1089/cmb.2016.0185) .

## Author

Martin R. Smith, after Ramesh Rajaby
