# Quartet v1.1.0
## Improvements
 - Pass trees directly to C, without writing to temporary intermediate file.
 - Use `TreeTools` in place of `TreeSearch`, supporting new `Splits` class.
 - Facilitate comparison between lists of multiple trees
 - Remove deprecated C function `ptr_fun`.
 - Clarify some documentation pages.

# Quartet v1.0.2
## Modifications
 - Force R 3.5.0-style random number generation in examples,
   to ensure backward compatibility.
 - Add caterpillar trees to `sq_trees`, to illustrate issues with partition-based
   distance metrics.

# Quartet v1.0.1
## Improvements
 - Improved portability of C++ code.

# Quartet v1.0.0
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

# SlowQuartet v0.2.0
## Modifications
 - Add `MatchingSplits` function to calculate number of splits contradicted / unresolved.
 - Add vignettes to document when the quartet metric may be preferable to other tree distance metrics.
