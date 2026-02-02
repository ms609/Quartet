# Tree similarity measures

Measure tree similarity or difference.

## Usage

``` r
SimilarityMetrics(elementStatus, similarity = TRUE)

DoNotConflict(elementStatus, similarity = TRUE)

ExplicitlyAgree(elementStatus, similarity = TRUE)

StrictJointAssertions(elementStatus, similarity = TRUE)

SemiStrictJointAssertions(elementStatus, similarity = TRUE)

SymmetricDifference(elementStatus, similarity = TRUE)

RawSymmetricDifference(elementStatus, similarity = FALSE)

RobinsonFoulds(elementStatus, similarity = FALSE)

MarczewskiSteinhaus(elementStatus, similarity = TRUE)

SteelPenny(elementStatus, similarity = TRUE)

QuartetDivergence(elementStatus, similarity = TRUE)

SimilarityToReference(elementStatus, similarity = TRUE, normalize = FALSE)
```

## Arguments

- elementStatus:

  Two-dimensional integer array, with rows corresponding to counts of
  matching quartets or partitions for each tree, and columns named
  according to the output of [`QuartetStatus()`](QuartetStatus.md) or
  [`SplitStatus()`](SplitStatus.md).

- similarity:

  Logical specifying whether to calculate the similarity or
  dissimilarity.

- normalize:

  Logical; if `TRUE`, a random or star tree has expected similarity 0
  (or difference 1), and the maximum possible score is one. If `FALSE`,
  zero similarity corresponds to all quartets contradicted, whereas one
  corresponds to all quartets correctly resolved – which will be
  unattainable if the reference tree contains polytomies.

## Value

`SimilarityMetrics()` returns a named two-dimensional array in which
each row corresponds to an input tree, and each column corresponds to
one of the listed measures.

`DoNotConflict()` and others return a named vector describing the
requested similarity (or difference) between the trees.

## Details

Estabrook et al. (1985) (table 2) define four similarity metrics in
terms of the total number of quartets (*N*, their *Q*), the number of
quartets resolved in the same manner in two trees (*s*), the number
resolved differently in both trees (*d*), the number resolved in tree 1
or 2 but unresolved in the other tree (*r1*, *r2*), and the number that
are unresolved in both trees (*u*).

The similarity metrics are then given as below. The dissimilarity
metrics are their complement (i.e. 1 - *similarity*), and can be
calculated algebraically using the identity *N* = *s* + *d* + *r1* +
*r2* + *u*.

Although defined using quartets, analogous values can be calculated
using partitions – though for a number of reasons, quartets may offer a
more meaningful measure of the amount of information shared by two trees
(Smith 2020) .

- Do Not Conflict (DC): (*s* + *r1* + *r2* + *u*) / *N*

- Explicitly Agree (EA): *s* / *N*

- Strict Joint Assertions (SJA): *s* / (*s* + *d*)

- SemiStrict Joint Assertions (SSJA): *s* / (*s* + *d* + *u*)

(The numerator of the SemiStrict Joint Assertions similarity metric is
given in Estabrook et al. (1985) table 2 as *s* + *d*, but this is
understood, with reference to their text, to be a typographic error.)

Steel and Penny (1993) propose a further metric, which they denote
d_Q\_, which this package calculates using the function `SteelPenny()`:

- Steel & Penny's quartet metric (dQ): (*s* + *u*) / *N*

Another take on tree similarity is to consider the symmetric difference:
that is, the number of partitions or quartets present in one tree that
do not appear in the other, originally used to measure tree similarity
by Robinson and Foulds (1981) . (Note that, given the familiarity of the
Robinson–Foulds distance metric, this quantity is be default expressed
as a difference rather than a similarity.)

- Raw symmetric difference (RF): *d1* + *d2* + *r1* + *r2*

A pair of trees will have a high symmetric difference if they are
well-resolved but disagree on many relationships; or if they agree on
most relationships but are poorly resolved. As such, it is essential to
contextualize the symmetric difference by appropriate normalization
(Smith 2019) . Multiple approaches to normalization have been proposed:

The total number of resolved quartets or partitions present in both
trees (Day 1986) :

- Symmetric Difference (SD): (2 *d* + *r1* + *r2*) / (2 *d* + 2 *s* +
  *r1* + *r2*)

The total distinctly resolved quartets or partitions (Marczewski and
Steinhaus 1958; Day 1986) :

- Marczewski-Steinhaus (MS): (2 *d* + *r1* + *r2*) / (2 *d* + *s* +
  *r1* + *r2*)

The maximum number of quartets or partitions that could have been
resolved, given the number of tips (Smith 2019) :

- Symmetric Divergence: (*d* + *d* + *r1* + *r2*) / *N*

Finally, in cases where a reconstructed tree `r1` is being compared to a
reference tree `r2` taken to represent "true" relationships, a symmetric
difference is not desired. In such settings, the desired score is the
expectation that a given quartet's resolution in the reconstructed tree
is "correct", given by Asher and Smith (2022) :

- Similarity to Reference (S2R): (*s* + (*r1* + *r2* + *u*) / 3) / *Q*

This may optionally be normalized with reference to the maximum possible
similarity, (*s* + *d* + *r2* + (*r1* + *u*) / 3) / *Q*, subtracting 1/3
(the probability of matching at random) from both the S2R score and
maximum possible score before dividing; then, a tree scores zero if it
is as different from the true tree as a random or fully unresolved tree,
and one if it is as "true" as can be known.

## References

Asher R, Smith MR (2022). “Phylogenetic signal and bias in
paleontology.” *Systematic Biology*, **71**(4), 986–1008.
[doi:10.1093/sysbio/syab072](https://doi.org/10.1093/sysbio/syab072) .  
  
Day WH (1986). “Analysis of quartet dissimilarity measures between
undirected phylogenetic trees.” *Systematic Biology*, **35**(3),
325–333.
[doi:10.1093/sysbio/35.3.325](https://doi.org/10.1093/sysbio/35.3.325)
.  
  
Estabrook GF, McMorris FR, Meacham CA (1985). “Comparison of undirected
phylogenetic trees based on subtrees of four evolutionary units.”
*Systematic Zoology*, **34**(2), 193–200.
[doi:10.2307/2413326](https://doi.org/10.2307/2413326) .  
  
Marczewski E, Steinhaus H (1958). “On a certain distance of sets and the
corresponding distance of functions.” *Colloquium Mathematicae*,
**6**(1), 319–327. <https://eudml.org/doc/210378>.  
  
Robinson DF, Foulds LR (1981). “Comparison of phylogenetic trees.”
*Mathematical Biosciences*, **53**(1-2), 131–147.
[doi:10.1016/0025-5564(81)90043-2](https://doi.org/10.1016/0025-5564%2881%2990043-2)
.  
  
Smith MR (2019). “Bayesian and parsimony approaches reconstruct
informative trees from simulated morphological datasets.” *Biology
Letters*, **15**(2), 20180632.
[doi:10.1098/rsbl.2018.0632](https://doi.org/10.1098/rsbl.2018.0632) .  
  
Smith MR (2020). “Information theoretic Generalized Robinson-Foulds
metrics for comparing phylogenetic trees.” *Bioinformatics*, **36**(20),
5007–5013.
[doi:10.1093/bioinformatics/btaa614](https://doi.org/10.1093/bioinformatics/btaa614)
.  
  
Steel MA, Penny D (1993). “Distributions of tree comparison metrics—some
new results.” *Systematic Biology*, **42**(2), 126–141.
[doi:10.1093/sysbio/42.2.126](https://doi.org/10.1093/sysbio/42.2.126) ,
<http://www.math.canterbury.ac.nz/~m.steel/Non_UC/files/research/distributions.pdf>.

## See also

- Calculate status of each quartet – the raw material from which the
  Estabrook *et al.* metrics are calculated – with
  [`QuartetStatus()`](QuartetStatus.md):

- Equivalent metrics for bipartition splits:
  [`SplitStatus()`](SplitStatus.md),
  [`CompareSplits()`](CompareSplits.md)

## Author

[Martin R. Smith](https://orcid.org/0000-0001-5660-1727)
(<martin.smith@durham.ac.uk>)

## Examples

``` r
data("sq_trees")

sq_status <- QuartetStatus(sq_trees)
SimilarityMetrics(sq_status)
#>                DoNotConflict ExplicitlyAgree StrictJointAssertions
#> ref_tree           1.0000000       1.0000000             1.0000000
#> move_one_near      0.9757576       0.9757576             0.9757576
#> move_one_mid       0.8424242       0.8424242             0.8424242
#> move_one_far       0.7696970       0.7696970             0.7696970
#> move_two_near      0.9272727       0.9272727             0.9272727
#> move_two_mid       0.7636364       0.7636364             0.7636364
#> move_two_far       0.7212121       0.7212121             0.7212121
#> collapse_one       1.0000000       0.9757576             1.0000000
#> collapse_some      1.0000000       0.6272727             1.0000000
#> m1mid_col1         0.8424242       0.8181818             0.8385093
#> m1mid_colsome      0.8424242       0.6454545             0.8037736
#> m2mid_col1         0.7636364       0.7393939             0.7577640
#> m2mid_colsome      1.0000000       0.3787879             1.0000000
#> opposite_tree      0.2606061       0.2606061             0.2606061
#> caterpillar        0.7393939       0.7393939             0.7393939
#> top_and_tail       0.3696970       0.3696970             0.3696970
#> anti_pectinate     0.2575758       0.2575758             0.2575758
#> random_tree        0.3151515       0.3151515             0.3151515
#>                SemiStrictJointAssertions SymmetricDifference
#> ref_tree                       1.0000000           1.0000000
#> move_one_near                  0.9757576           0.9757576
#> move_one_mid                   0.8424242           0.8424242
#> move_one_far                   0.7696970           0.7696970
#> move_two_near                  0.9272727           0.9272727
#> move_two_mid                   0.7636364           0.7636364
#> move_two_far                   0.7212121           0.7212121
#> collapse_one                   1.0000000           0.9877301
#> collapse_some                  1.0000000           0.7709497
#> m1mid_col1                     0.8385093           0.8282209
#> m1mid_colsome                  0.8037736           0.7159664
#> m2mid_col1                     0.7577640           0.7484663
#> m2mid_colsome                  1.0000000           0.5494505
#> opposite_tree                  0.2606061           0.2606061
#> caterpillar                    0.7393939           0.7393939
#> top_and_tail                   0.3696970           0.3696970
#> anti_pectinate                 0.2575758           0.2575758
#> random_tree                    0.3151515           0.3151515
#>                MarczewskiSteinhaus SteelPenny QuartetDivergence
#> ref_tree                 1.0000000  1.0000000         1.0000000
#> move_one_near            0.9526627  0.9757576         0.9757576
#> move_one_mid             0.7277487  0.8424242         0.8424242
#> move_one_far             0.6256158  0.7696970         0.7696970
#> move_two_near            0.8644068  0.9272727         0.9272727
#> move_two_mid             0.6176471  0.7636364         0.7636364
#> move_two_far             0.5639810  0.7212121         0.7212121
#> collapse_one             0.9757576  0.9757576         0.9878788
#> collapse_some            0.6272727  0.6272727         0.8136364
#> m1mid_col1               0.7068063  0.8181818         0.8303030
#> m1mid_colsome            0.5575916  0.6454545         0.7439394
#> m2mid_col1               0.5980392  0.7393939         0.7515152
#> m2mid_colsome            0.3787879  0.3787879         0.6893939
#> opposite_tree            0.1498258  0.2606061         0.2606061
#> caterpillar              0.5865385  0.7393939         0.7393939
#> top_and_tail             0.2267658  0.3696970         0.3696970
#> anti_pectinate           0.1478261  0.2575758         0.2575758
#> random_tree              0.1870504  0.3151515         0.3151515
#>                SimilarityToReference
#> ref_tree                   1.0000000
#> move_one_near              0.9757576
#> move_one_mid               0.8424242
#> move_one_far               0.7696970
#> move_two_near              0.9272727
#> move_two_mid               0.7636364
#> move_two_far               0.7212121
#> collapse_one               0.9838384
#> collapse_some              0.7515152
#> m1mid_col1                 0.8262626
#> m1mid_colsome              0.7111111
#> m2mid_col1                 0.7474747
#> m2mid_colsome              0.5858586
#> opposite_tree              0.2606061
#> caterpillar                0.7393939
#> top_and_tail               0.3696970
#> anti_pectinate             0.2575758
#> random_tree                0.3151515
QuartetDivergence(sq_status, similarity = FALSE)
#>       ref_tree  move_one_near   move_one_mid   move_one_far  move_two_near 
#>     0.00000000     0.02424242     0.15757576     0.23030303     0.07272727 
#>   move_two_mid   move_two_far   collapse_one  collapse_some     m1mid_col1 
#>     0.23636364     0.27878788     0.01212121     0.18636364     0.16969697 
#>  m1mid_colsome     m2mid_col1  m2mid_colsome  opposite_tree    caterpillar 
#>     0.25606061     0.24848485     0.31060606     0.73939394     0.26060606 
#>   top_and_tail anti_pectinate    random_tree 
#>     0.63030303     0.74242424     0.68484848 

library("TreeTools", quietly = TRUE, warn.conflict = FALSE)
set.seed(0)
reference <- CollapseNode(as.phylo(101, 10), 16:18)
trees <- c(
  reference = reference,
  binaryRef = MakeTreeBinary(reference),
  balanced = BalancedTree(reference),
  pectinate = PectinateTree(reference),
  star = StarTree(reference),
  random = RandomTree(reference),
  random2 = RandomTree(reference)
)
elementStatus <- QuartetStatus(trees, reference)
SimilarityToReference(elementStatus)
#> reference binaryRef  balanced pectinate      star    random   random2 
#> 0.7682540 0.7682540 0.3015873 0.3444444 0.3333333 0.3492063 0.4301587 
SimilarityToReference(elementStatus, normalize = TRUE)
#>   reference   binaryRef    balanced   pectinate        star      random 
#>  1.00000000  1.00000000 -0.07299270  0.02554745  0.00000000  0.03649635 
#>     random2 
#>  0.22262774 
```
