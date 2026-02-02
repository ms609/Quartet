# Direct entry points to "tqDist" functions

Wrappers for functions in "tqDist", which calculate triplet and quartet
distances between pairs of trees.

## Usage

``` r
QuartetDistance(file1, file2)

QuartetAgreement(file1, file2)

PairsQuartetDistance(file1, file2)

OneToManyQuartetAgreement(file1, file2)

AllPairsQuartetDistance(file)

AllPairsQuartetAgreement(file)

TripletDistance(file1, file2)

PairsTripletDistance(file1, file2)

AllPairsTripletDistance(file)
```

## Arguments

- file, file1, file2:

  Paths to files containing a tree or trees in Newick format, possibly
  created using [`TQFile()`](TQFile.md).

## Value

`...Distance()` functions return the distance between the requested
trees.

`...Agreement()` functions return the number of triplets or quartets
that are:

- `A`, resolved in the same fashion in both trees;

- `E`, unresolved in both trees.

Comparing a tree against itself yields the totals (`A+B+C`) and (`D+E`)
referred to by Brodal et al. (2013) and Holt et al. (2014) .

## Functions

- `QuartetDistance()`: Returns the quartet distance between the tree. in
  `file1` and the tree in `file2`.

- `QuartetAgreement()`: Returns a vector of length two, listing \[1\]
  the number of resolved quartets that agree (`A`); \[2\] the number of
  quartets that are unresolved in both trees (`E`). See Brodal et
  al. (2013) .

- `PairsQuartetDistance()`: Quartet distance between the tree on each
  line of `file1` and the tree on the corresponding line of `file2`.

- `OneToManyQuartetAgreement()`: Quartet distance between the tree in
  `file1` and the tree on each line of `file2`.

- `AllPairsQuartetDistance()`: Quartet distance between each tree listed
  in `file` and each other tree therein.

- `AllPairsQuartetAgreement()`: Quartet status for each pair of trees in
  `file`.

- `TripletDistance()`: Triplet distance between the single tree given in
  each file.

- `PairsTripletDistance()`: Triplet distance between the tree on each
  line of `file1` and the tree on the corresponding line of `file2`.

- `AllPairsTripletDistance()`: Triplet distance between each tree listed
  in `file` and each other tree therein.

## References

Brodal GS, Fagerberg R, Mailund T, Pedersen CNS, Sand A (2013).
“Efficient algorithms for computing the triplet and quartet distance
between trees of arbitrary degree.” *SODA '13 Proceedings of the
Twenty-Fourth Annual ACM-SIAM Symposium on Discrete Algorithms*,
1814–1832.
[doi:10.1137/1.9781611973105.130](https://doi.org/10.1137/1.9781611973105.130)
.  
  
Holt MK, Johansen J, Brodal GS (2014). “On the scalability of computing
triplet and quartet distances.” In *Proceedings of 16th Workshop on
Algorithm Engineering and Experiments (ALENEX) Portland, Oregon, USA*.  
  
Sand A, Holt MK, Johansen J, Brodal GS, Mailund T, Pedersen CNS (2014).
“tqDist: a library for computing the quartet and triplet distances
between binary or general trees.” *Bioinformatics*, **30**(14),
2079–2080. ISSN 1460-2059,
[doi:10.1093/bioinformatics/btu157](https://doi.org/10.1093/bioinformatics/btu157)
.

## See also

- [`QuartetStatus()`](QuartetStatus.md) takes trees, rather than files,
  as input.

- [`TQFile()`](TQFile.md) creates a temporary file containing specified
  trees.

## Author

- Algorithms: Brodal et al. (2013) ; Holt et al. (2014) .

- C implementation: Sand et al. (2014) ; modified for portability by
  Martin R. Smith.

- R interface: Martin R. Smith.
