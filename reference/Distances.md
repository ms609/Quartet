# Direct entry points to "tqDist" functions

Wrappers for functions in "tqDist", which calculate triplet and quartet
distances between pairs of trees.

## Usage

``` r
# S3 method for class 'phylo'
TripletDistance(tree1, tree2 = NULL)

# S3 method for class 'list'
TripletDistance(tree1, tree2 = NULL)

# S3 method for class 'multiPhylo'
TripletDistance(tree1, tree2 = NULL)

QuartetDistance(file1, file2)

QuartetAgreement(file1, file2)

PairsQuartetDistance(file1, file2)

OneToManyQuartetAgreement(file1, file2)

AllPairsQuartetDistance(file)

AllPairsQuartetAgreement(file)

TripletDistance(tree1, tree2 = NULL)

# S3 method for class 'character'
TripletDistance(tree1, tree2 = NULL)

PairsTripletDistance(file1, file2)

AllPairsTripletDistance(file)
```

## Arguments

- tree1, tree2:

  Arguments to `TripletDistance`: file paths containing Newick trees,
  phylogenetic trees of class `phylo`, or lists/`multiPhylo` of trees.
  If `tree1` is a list and `tree2 = NULL`, returns a distance matrix of
  all pairwise distances.

- file, file1, file2:

  Paths to files containing a tree or trees in Newick format, possibly
  created using
  [`TQFile()`](https://ms609.github.io/Quartet/reference/TQFile.md).

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

- `TripletDistance(phylo)`: Triplet distance between two trees of class
  `phylo`. Uses the CPDT algorithm (Jansson and Rajaby 2017) .

- `TripletDistance(list)`: Triplet distance between each pair of trees
  in a list. If `tree2` is provided, returns pairwise distances between
  corresponding trees in `tree1` and `tree2`. If `tree2 = NULL`, returns
  a distance matrix of all pairwise distances within `tree1`.

- `TripletDistance(multiPhylo)`: Triplet distance between each pair of
  trees in a `multiPhylo` object.

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

- `TripletDistance()`: Triplet distance between trees. `tree1` and
  `tree2` can be `phylo` objects, file paths to Newick tree files, or
  lists/`multiPhylo` objects. If `tree1` is a list and `tree2 = NULL`,
  computes all pairwise distances; if both are lists, computes distances
  between corresponding pairs. Uses the CPDT algorithm (Jansson and
  Rajaby 2017) .

- `TripletDistance(character)`: Triplet distance between single trees
  read from Newick-format files `tree1` and `tree2`.

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
  
Jansson J, Rajaby R (2017). “A More Practical Algorithm for the Rooted
Triplet Distance.” *Journal of Computational Biology*, **24**(2),
106–126.
[doi:10.1089/cmb.2016.0185](https://doi.org/10.1089/cmb.2016.0185) .  
  
Sand A, Holt MK, Johansen J, Brodal GS, Mailund T, Pedersen CNS (2014).
“tqDist: a library for computing the quartet and triplet distances
between binary or general trees.” *Bioinformatics*, **30**(14),
2079–2080. ISSN 1460-2059.
[doi:10.1093/bioinformatics/btu157](https://doi.org/10.1093/bioinformatics/btu157)
.

## See also

- [`QuartetStatus()`](https://ms609.github.io/Quartet/reference/QuartetStatus.md)
  takes trees, rather than files, as input.

- [`TQFile()`](https://ms609.github.io/Quartet/reference/TQFile.md)
  creates a temporary file containing specified trees.

Other TQDist functions:
[`TQDist()`](https://ms609.github.io/Quartet/reference/TQDist.md)

## Author

- Quartet algorithms: Brodal et al. (2013) ; Holt et al. (2014) . C
  implementation: Sand et al. (2014) ; modified for portability by
  Martin R. Smith.

- Triplet algorithm: Jansson and Rajaby (2017) . C++ implementation by
  Ramesh Rajaby; R integration by Martin R. Smith.
