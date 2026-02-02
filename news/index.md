# Changelog

## Quartet v1.2.7.9000 (2026-02-02)

- R \<4.1 is no longer formally supported.

## Quartet v1.2.7 (2024-10-31)

CRAN release: 2024-10-31

- `VizualizeQuartet()` now returns calculation results.
- Use spectrum legend in `VisualizeQuartet()`.
- Include CSL file within package to remove external dependency.

## Quartet v1.2.6 (2024-02-08)

CRAN release: 2024-02-09

- Drop obsolete C++11 specification.
- Update minimum R version to 3.5 (for Ternary).
- R \<3.6 is no longer formally supported.

## Quartet v1.2.5 (2022-07-08)

CRAN release: 2022-07-08

- Fix memory leaks.

## Quartet v1.2.4 (2022-05-02)

CRAN release: 2022-05-02

- [`QuartetStates()`](../reference/QuartetState.md) handles absent
  leaves gracefully.
- Copy-edit documentation.

## Quartet v1.2.3

- Replace deprecated function `TreeTools::in.Splits()`.
- Update test cases in readiness for TreeTools v1.7.0.

## Quartet v1.2.2 (2020-12-09)

CRAN release: 2020-12-09

- Package ‘vdiffr’ now used conditionally.

## Quartet v1.2.0 (2020-10-22)

CRAN release: 2020-10-21

### New features

- [`CompareQuartetsMulti()`](../reference/CompareQuartetsMulti.md)
  compares quartet status in one tree with status in multiple others.
- [`VisualizeQuartets()`](../reference/VisualizeQuartets.md) depicts
  contribution of splits to quartet score.
- [`SimilarityToReference()`](../reference/SimilarityMetrics.md) allows
  comparison against specified reference topology.

### Changes

- Deprecate [`RobinsonFoulds()`](../reference/SimilarityMetrics.md):
  renamed to
  [`RawSymmetricDifference()`](../reference/SimilarityMetrics.md).
- `QuartetState[s]()` now uses sister-of-4 notation, rather than
  sister-of-1, to give values within \[0, 3\].
- [`ManyToManyQuartetAgreement()`](../reference/QuartetStatus.md) now
  returns entries for `N` and `Q`.

### Improvements

- Faster C++ implementation of
  [`AllQuartets()`](../reference/AllQuartets.md) and
  [`QuartetStates()`](../reference/QuartetState.md).
- [`QuartetStatus()`](../reference/QuartetStatus.md) now supports
  non-identical leaf samples.
- [`SimilarityMetrics()`](../reference/SimilarityMetrics.md) now handles
  single tree comparisons.
- Correctly calculate resolution of unrooted trees with unconventional
  node numbering conventions.
- Remove errant name when comparing unnamed tree pairs.
- Small improvements to [`PlotQuartet()`](../reference/PlotQuartet.md).
- Minor efficiency improvements.
- Documentation improvements.

## Quartet v1.1.0 (2020-01-29)

CRAN release: 2020-01-28

- Pass trees directly to C, without writing to temporary intermediate
  file.
- Use `TreeTools` in place of `TreeSearch`, supporting new `Splits`
  class.
- Facilitate comparison between lists of multiple trees.
- Fix Latex error in vignettes.

## Quartet v1.0.3

CRAN release: 2019-12-30

- Remove deprecated C function `ptr_fun`.
- Clarify some documentation pages.

## Quartet v1.0.2

CRAN release: 2019-03-06

- Force R 3.5.0-style random number generation in examples, to ensure
  backward compatibility.
- Add caterpillar trees to `sq_trees`, to illustrate issues with
  partition-based distance metrics.

## Quartet v1.0.1

CRAN release: 2019-01-22

- Improved portability of C++ code.

## Quartet v1.0.0 (2019-01-09)

CRAN release: 2019-01-16

### New features

- Add functions to generate ternary plots from tree similarity measures.

### Improvements

- Use tqDist for all quartet calculations.
- Remove single “splits” from splits objects.
- Simplify installation via `github_install`.
- Add missing details to documentation.

### Modifications

- Function `SimilarityMetrics` now takes the output of
  `QuartetStatus(trees)`, rather than a list of trees.
- Function `SplitStatus` now returns output using the same column
  headers as `QuartetStatus`
- Column `N` added to `QuartetStatus` output.
- Function names changed, to become more precise:
  - `QuartetMetrics` → `SimilarityMetrics`
  - `MatchingSplits` → `SplitStatus`
  - `MatchingQuartets` → `QuartetStatus`
  - `Choices` → `AllQuartets`

## SlowQuartet v0.2.0 (2017-11-22)

### Modifications

- Add `MatchingSplits` function to calculate number of splits
  contradicted / unresolved.
- Add vignettes to document when the quartet metric may be preferable to
  other tree distance metrics.
