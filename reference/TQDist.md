# Wrapper for tqDist

`TQDist()` and `TQAE()` are convenience functions that writes a list of
trees to text files that can be processed by the C implementation of
tqDist (Sand et al. 2014) . tqDist is then called, and the temporary
file is deleted when analysis is complete.

## Usage

``` r
TQDist(trees)

TQAE(trees)
```

## Arguments

- trees:

  List of phylogenetic trees, of class `list` or
  [`multiPhylo`](https://rdrr.io/pkg/ape/man/read.tree.html).

## Value

`TQDist()` returns the quartet distance between each pair of trees.

`TQAE()` returns the number of resolved quartets in agreement between
each pair of trees ("A" in Brodal *et al*. 2013) and the number of
quartets that are unresolved in both trees ("E" in Brodal *et al*.
2013).

## Details

Quartets can be resolved in one of five ways, which Brodal et al. (2013)
and Holt et al. (2014) distinguish using the letters A-E, and Estabrook
et al. (1985) refer to as:

- A:

  *s* = resolved the **s**ame in both trees;

- B:

  *d* = resolved **d**ifferently in both trees;

- C:

  *r1* = **r**esolved only in tree **1**;

- D:

  *r2* = **r**esolved only in tree **2** (the comparison tree);

- E:

  *u* = **u**nresolved in both trees.

## References

Brodal GS, Fagerberg R, Mailund T, Pedersen CNS, Sand A (2013).
“Efficient algorithms for computing the triplet and quartet distance
between trees of arbitrary degree.” *SODA '13 Proceedings of the
Twenty-Fourth Annual ACM-SIAM Symposium on Discrete Algorithms*,
1814–1832.
[doi:10.1137/1.9781611973105.130](https://doi.org/10.1137/1.9781611973105.130)
.  
  
Estabrook GF, McMorris FR, Meacham CA (1985). “Comparison of undirected
phylogenetic trees based on subtrees of four evolutionary units.”
*Systematic Zoology*, **34**(2), 193–200.
[doi:10.2307/2413326](https://doi.org/10.2307/2413326) .  
  
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

[`CompareQuartets()`](CompareQuartets.md),
[`QuartetStatus()`](QuartetStatus.md)

## Author

[Martin R. Smith](https://orcid.org/0000-0001-5660-1727)
(<martin.smith@durham.ac.uk>)
