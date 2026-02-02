# Direct entry points to 'tqDist' functions

Functions to calculate triplet and quartet distances between pairs of
trees. Input is not checked for sanity.

## Usage

``` r
tqdist_QuartetDistance(file1, file2)

tqdist_QuartetAgreement(file1, file2)

tqdist_QuartetAgreementEdge(edge1, edge2)

tqdist_QuartetAgreementChar(string1, string2)

tqdist_PairsQuartetDistance(file1, file2)

tqdist_OneToManyQuartetAgreement(file1, fileMany)

tqdist_OneToManyQuartetAgreementChar(tree, trees)

tqdist_OneToManyQuartetAgreementEdge(edge, edges)

tqdist_AllPairsQuartetDistance(file)

tqdist_AllPairsQuartetDistanceChar(string)

tqdist_AllPairsQuartetDistanceEdge(edges)

tqdist_AllPairsQuartetAgreement(file)

tqdist_AllPairsQuartetAgreementChar(string)

tqdist_AllPairsQuartetAgreementEdge(edges)

tqdist_TripletDistance(file1, file2)

tqdist_PairsTripletDistance(file1, file2)

tqdist_AllPairsTripletDistance(file)
```

## Arguments

- file, file1, file2:

  Paths to files containing a tree or trees in Newick format.

## Value

The distance between the requested trees.

## Details

Functions are called from R with user-friendly functions such as
[`AllPairsQuartetDistance`](Distances.md).

## Functions

- `tqdist_QuartetAgreement()`: Agreement of each quartet

- `tqdist_QuartetAgreementEdge()`: Agreement of each quartet

- `tqdist_QuartetAgreementChar()`: Agreement of each quartet

- `tqdist_PairsQuartetDistance()`: Distance between pairs

- `tqdist_OneToManyQuartetAgreement()`: Distance between pairs

- `tqdist_OneToManyQuartetAgreementChar()`: Distance between pairs

- `tqdist_OneToManyQuartetAgreementEdge()`: Distance between pairs

- `tqdist_AllPairsQuartetDistance()`: Distance between all pairs

- `tqdist_AllPairsQuartetDistanceChar()`: Distance between all pairs

- `tqdist_AllPairsQuartetDistanceEdge()`: Distance between all pairs

- `tqdist_AllPairsQuartetAgreement()`: Agreement between all pairs of
  trees

- `tqdist_AllPairsQuartetAgreementChar()`: Agreement between all pairs
  of trees

- `tqdist_AllPairsQuartetAgreementEdge()`: Agreement between all pairs
  of trees

- `tqdist_TripletDistance()`: Triplet distance between two trees

- `tqdist_PairsTripletDistance()`: Triplet distance between pairs

- `tqdist_AllPairsTripletDistance()`: Triplet distance between all pairs

## References

Sand A, Holt MK, Johansen J, Brodal GS, Mailund T, Pedersen CNS (2014).
“tqDist: a library for computing the quartet and triplet distances
between binary or general trees.” *Bioinformatics*, **30**(14),
2079–2080. ISSN 1460-2059,
[doi:10.1093/bioinformatics/btu157](https://doi.org/10.1093/bioinformatics/btu157)
.

## Author

Martin R. Smith, after Andreas Sand
