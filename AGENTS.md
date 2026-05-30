# Quartet Agent Notes

### Key files

| File | Purpose |
|------|---------|
| `issues.md` | Human-entered issues (agents triage → `to-do.md`) |
| `to-do.md` | Task queue |
| `dev/coordination.md` | Strategic plan |

## Test conventions

Test files live in `tests/testthat/`. Use `Quartet:::` to access internal
functions in tests. Build and run the full suite with:

```bash
R CMD INSTALL --library=.agent-X .
Rscript -e "library(Quartet, lib.loc='.agent-X'); testthat::test_dir('tests/testthat')"
```

Snapshot tests (in `tests/testthat/_snaps/`) must be reviewed and updated
explicitly — never auto-accept changed snapshots without inspecting the diff.

**Coverage target: 100%.** The GHA test suite runs codecov; uncovered lines
will block the PR. Use `// # nocov start` / `// # nocov end` in C++ (or
`# nocov` in R) only for truly unreachable defensive guards, with a comment
explaining why the code can't be reached.


## R source file conventions

- `DESCRIPTION` has no explicit `Collate:` field; R sources alphabetically.
- Documentation is generated with `roxygen2`. Always use
  `roxygen2::roxygenise(load_code = roxygen2::load_installed)`.
- **When any function signature changes** (parameters added, removed, renamed,
  or reordered — in R or C++), run `devtools::check_man()` before committing.
  This catches `\usage` / `\arguments` mismatches in `.Rd` files.
  For C++ exports, also run `Rcpp::compileAttributes()` first so that
  `R/RcppExports.R` stays in sync with the `// [[Rcpp::export]]` annotations.

## Architecture reference

### Package purpose

Quartet calculates tree similarity metrics based on four-taxon subtrees
(quartets). It wraps the **tqDist algorithm** (Sand et al. 2014, O(n log n))
via C++ and provides a suite of R-level similarity/distance metrics.

Supports trees with up to **477 leaves** (32-bit integer constraint in the
underlying C library).

### R-level API

| Function | Purpose |
|----------|---------|
| `QuartetStates()` / `QuartetStatus()` | Quartet states for all four-taxon subsets |
| `CompareQuartets()` / `CompareQuartetsMulti()` | Element-wise quartet comparison |
| `QuartetAgreement()` / `QuartetDistance()` | Summary agreement / distance |
| `AllPairsQuartetAgreement()` / `AllPairsQuartetDistance()` | All-pairs matrix |
| `OneToManyQuartetAgreement()` / `TwoListQuartetAgreement()` | Batch comparisons |
| `SimilarityMetrics()` / `SimilarityToReference()` | Multi-metric table |
| `SplitStatus()` / `SymmetricDifference()` | Robinson-Foulds / partition metrics |
| `DoNotConflict()`, `ExplicitlyAgree()`, `SteelPenny()`, etc. | Individual metrics |
| `PlotQuartet()` / `VisualizeQuartets()` | Quartet visualizations |
| `QuartetPoints()` / `BipartitionPoints()` / `SplitPoints()` | Ternary plot helpers |

### C++ layer

The tqDist C++ code lives in `src/`. The main Rcpp bridge is
`src/rQuartetDist.cpp`. These files implement the original tqDist library
(Brodal et al. 2004 / Sand et al. 2014) and should be treated as vendored
upstream code — modifications should be minimal and well-documented.

**Shared file rule:** If multiple agents need to touch `src/`, coordinate via
`to-do.md` to avoid conflicting edits. Prefer R-level changes over C++ changes
wherever possible.

**OpenMP**: All-pairs, one-to-many, and pairs loops are parallelised with
OpenMP (per-thread `QuartetDistanceCalculator` instances). `Rcpp::stop()` must
not be called inside parallel regions — use error flags instead.

### Dependencies

| Type | Packages |
|------|----------|
| Depends | `TreeTools (≥1.4.0)`, `R (≥3.5.0)` |
| Imports | `ape`, `PlotTools`, `Rdpack`, `Ternary`, `TreeDist` |
| Suggests | `future`, `future.apply`, `phangorn`, `testthat`, `knitr`, `rmarkdown`, `vdiffr` |

## Version and CRAN status

- **Version**: 1.2.7.9000 (development)
- **Last CRAN release**: 1.2.7 (2024-10-31)
- **R CMD check**: CI via `.github/workflows/R-CMD-check.yml`

## Remaining optimization opportunities

- `CountingLinkedList` → flat array conversion (~11% of C++ time from VTune
  profiling). Deferred — invasive change across ~8 files, and the pool-size
  increase captured most of the cache benefit.
- `.TreeToEdge` S3 methods are not registered in NAMESPACE; called via S3
  dispatch internally but inaccessible to tests via `library()`.

## Benchmarks

Baseline and post-optimization benchmarks are in `inst/benchmarks/`.
Profiling results and methodology are in `.positai/expertise/profiling.md`.

## ICQ (Information-Corrected Quartet Distance)

**Branch**: `icq` (worktree: `Q-IC`)
**Status**: Phases 1–4 complete; Phase 5 (vignette, NEWS, CRAN prep) pending
**Plan file**: `.positai/plans/2026-03-22-0723-icq-implementation-in-quartet-package.md`

### What it is

ICQ measures shared phylogenetic information between two trees in bits,
accounting for the non-independence of quartet statements. Unlike the
standard quartet distance (which counts disagreeing quartets uniformly),
ICQ weights information by how much each agreement constrains the space
of possible trees.

### Algorithm (3-layer decomposition)

1. **`ICQ(tr1, tr2)`** — entry point; dispatches pairwise, all-pairs,
   one-to-many, or element-wise based on input types.
2. **`.ICQ_pair()`** — core pairwise computation:
   a. **ReduceTrees** (Allen & Steel 2001) collapses shared pendant subtrees.
      Distance is invariant under this reduction (identity:
      `NUnrooted(m+1) = NRooted(m)` ensures split weight = CI loss).
   b. **`.ICQ_decompose()`** — recursive divide-and-conquer at shared splits.
      Each shared split contributes
      `Log2Unrooted(n) - Log2Rooted(|A|) - Log2Rooted(|B|)` bits.
   c. **`.ICQ_base_case()`** — when no shared splits remain. Exact
      enumeration of all topologies, filtered by agreed quartets.
      `log2(total) - log2(consistent)`.
3. **`.ICQ_multi_compute()`** — sequential `vapply` or, when `future.apply`
   is installed and a non-sequential `plan()` is set, `future_vapply`.
   No cache to transfer — on-the-fly enumeration is self-contained.

### Key findings

- **Consensus shortcut fails**: the set of trees consistent with agreed
  quartets is NOT the set of resolutions of any multifurcating tree.
  Independence approximation, quartet closure, and clustering metrics
  (SPI, MSI, MCI) all fail — anti-correlated by n=8.
- **ReduceTrees is the key optimisation**: for 1-swap tree pairs,
  1000-leaf trees reduce to ≤9 leaves. Verified invariant on 150+ pairs.

### Current limits

- **Exact**: residual subtrees ≤ 11 leaves (NUnrooted(11) ≈ 34.5M;
  enumerated on-the-fly in C++, ~10 s per base-case call at n=11)
- **NA**: residual subtrees > 11 (hit for very divergent trees at n ≈ 30+)
- No sampling fallback (uniform sampling ineffective above n ≈ 9)

### API

```r
ICQ(tr1, tr2, similarity = FALSE, normalize = FALSE)
ICQ(trees)                          # all-pairs → dist
ICQ(tree, trees)                    # one-to-many → vector
ICQ(trees1, trees2)                 # element-wise → vector
# Parallelism: library(future); plan(multisession); ICQ(trees)
```

- `normalize = TRUE`: divide by `CladisticInfo(tr1)`, giving [0, 1]
- Returns `NA` (with warning) for incalculable pairs

### Dependencies

- **Imports**: `TreeDist` (for `ReduceTrees`)
- **Suggests**: `future.apply` (for parallelism)

### Files

- `R/ICQ.R` — all R code (dispatch, `.ICQ_pair`, legacy
  `.ICQ_decompose`/`.ICQ_base_case`/`.ICQ_state_matrix` for regression
  testing, `.ICQ_multi_compute`, `.ICQ_normalize`)
- `src/icq.cpp` — C++ decomposition + on-the-fly topology enumeration
  (`count_matching_topologies`, `decompose`, `edges_to_splits`,
  `quartet_state`, legacy `icq_base_case_cpp`)
- `tests/testthat/test-ICQ.R` — tests (pairwise, multi-tree,
  normalize, incalculable, input validation)
- `man/ICQ.Rd` — generated documentation
