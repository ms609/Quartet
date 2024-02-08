# Quartet v1.2.6 (2024-02-08)

- Drop obsolete C++11 specification.
- Update minimum R version to 3.5 (for Ternary).
- R <3.6 is no longer formally supported.

# Quartet v1.2.5 (2022-07-08)

- Fix memory leaks.

# Quartet v1.2.4 (2022-05-02)

 - `QuartetStates()` handles absent leaves gracefully.
 - Copy-edit documentation.

# Quartet v1.2.3

 - Replace deprecated function `TreeTools::in.Splits()`.
 - Update test cases in readiness for TreeTools v1.7.0.
 
# Quartet v1.2.2 (2020-12-09)

 - Package 'vdiffr' now used conditionally.

# Quartet v1.2.0 (2020-10-22)

## New features
 - `CompareQuartetsMulti()` compares quartet status in one tree with status in
   multiple others.
 - `VisualizeQuartets()` depicts contribution of splits to quartet score.
 - `SimilarityToReference()` allows comparison against specified reference
   topology.

## Changes
 - Deprecate `RobinsonFoulds()`: renamed to `RawSymmetricDifference()`.
 - `QuartetState[s]()` now uses sister-of-4 notation, rather than sister-of-1,
   to give values within [0, 3].
 - `ManyToManyQuartetAgreement()` now returns entries for `N` and `Q`. 
 
## Improvements
 - Faster C++ implementation of `AllQuartets()` and `QuartetStates()`.
 -  `QuartetStatus()` now supports non-identical leaf samples.
 - `SimilarityMetrics()` now handles single tree comparisons.
 - Correctly calculate resolution of unrooted trees with unconventional node
   numbering conventions.
 - Remove errant name when comparing unnamed tree pairs.
 - Small improvements to `PlotQuartet()`.
 - Minor efficiency improvements.
 - Documentation improvements.


# Quartet v1.1.0 (2020-01-29)

 - Pass trees directly to C, without writing to temporary intermediate file.
 - Use `TreeTools` in place of `TreeSearch`, supporting new `Splits` class.
 - Facilitate comparison between lists of multiple trees.
 - Fix Latex error in vignettes.

# Quartet v1.0.3

 - Remove deprecated C function `ptr_fun`.
 - Clarify some documentation pages.

# Quartet v1.0.2

 - Force R 3.5.0-style random number generation in examples,
   to ensure backward compatibility.
 - Add caterpillar trees to `sq_trees`, to illustrate issues with partition-based
   distance metrics.

# Quartet v1.0.1

 - Improved portability of C++ code.

# Quartet v1.0.0 (2019-01-09)

## New features
 - Add functions to generate ternary plots from tree similarity measures.
 
## Improvements
 - Use tqDist for all quartet calculations.
 - Remove single "splits" from splits objects.
 - Simplify installation via `github_install`.
 - Add missing details to documentation.
 
## Modifications
 - Function `SimilarityMetrics` now takes the output of `QuartetStatus(trees)`,
     rather than a list of trees.
 - Function `SplitStatus` now returns output using the same column headers as `QuartetStatus` 
 - Column `N` added to `QuartetStatus` output.
 - Function names changed, to become more precise:
   - `QuartetMetrics` → `SimilarityMetrics`
   - `MatchingSplits` → `SplitStatus`
   - `MatchingQuartets` → `QuartetStatus`
   - `Choices` → `AllQuartets`

# SlowQuartet v0.2.0 (2017-11-22)
## Modifications
 - Add `MatchingSplits` function to calculate number of splits contradicted / unresolved.
 - Add vignettes to document when the quartet metric may be preferable to other tree distance metrics.
