# Quartet Agent Notes

## Build isolation — per-agent library directories

Each agent **must** build and test to its own private library:

```bash
R CMD INSTALL --library=.agent-X .
Rscript -e "library(Quartet, lib.loc='.agent-X'); testthat::test_dir('tests/testthat')"
```

**Never** install to the default library. On Windows, a loaded DLL locks
the file and blocks other agents.

**Never** use `devtools::load_all()` or `pkgbuild::compile_dll()` — these
target a shared temp location and will conflict.

### Build failure recovery

`roxygen2::roxygenise()` (default mode) leaves debug `.o` files in `src/`.
Always clean before building, and use the installed-code loader for docs:

```bash
rm -f src/*.o src/*.dll
R CMD INSTALL --library=.agent-X .
Rscript -e ".libPaths(c('.agent-X', .libPaths())); roxygen2::roxygenise(load_code = roxygen2::load_installed)"
```

If `R CMD INSTALL` fails with "Access is denied", another R process has the
DLL loaded. Kill it or wait, then retry.

## CPU limits — max 2 cores per agent

Use at most `-j2` for make. Avoid spawning many parallel R processes.

## Multi-agent workflow protocol

### Assignment

On `/assign X`:

1. Read `agent-X.md`. If a task is already in-progress, resume it.
2. Otherwise, check `issues.md` **before** `to-do.md`:
   a. Claim the bottom-most unclaimed issue, triage it into `to-do.md`
      tasks, then delete the issue block from `issues.md`.
   b. While `issues.md` still has unclaimed issues, triaging takes priority.
3. If `issues.md` is empty, claim the next OPEN task from `to-do.md`.

### During work

- Update `agent-X.md` after every significant step (crash-recovery record).
- All work uses `.agent-X/` as library directory.
- **All builds, tests, and benchmarks in bash subprocesses** — never in the
  RStudio R session.

### On task completion

1. Move task to Completed in `to-do.md`.
2. Set `agent-X.md` to IDLE.
3. Update `coordination.md` if strategic objectives are affected.
4. Take next task.

### Key files

| File | Purpose |
|------|---------|
| `issues.md` | Human-entered issues (agents triage → `to-do.md`) |
| `to-do.md` | Task queue |
| `coordination.md` | Strategic plan |
| `agent-X.md` | Agent progress log |
| `AGENTS.md` | Conventions + architecture reference |
| `.positai/expertise/*.md` | Standing task methodology |

## Test conventions

Test files live in `tests/testthat/`. Use `Quartet:::` to access internal
functions in tests. Build and run the full suite with:

```bash
R CMD INSTALL --library=.agent-X .
Rscript -e "library(Quartet, lib.loc='.agent-X'); testthat::test_dir('tests/testthat')"
```

Snapshot tests (in `tests/testthat/_snaps/`) must be reviewed and updated
explicitly — never auto-accept changed snapshots without inspecting the diff.

**Pre-existing test failures**: 15 failures in `test-AllQuartets.cpp.R`,
`test-1-tqdist.R`, `test-CompareQuartets.R` — caused by unexported internal
functions unavailable when running via `library()`. Not correctness issues.

## R source file conventions

- `DESCRIPTION` has no explicit `Collate:` field; R sources alphabetically.
- Documentation is generated with `roxygen2`. Always use
  `roxygen2::roxygenise(load_code = roxygen2::load_installed)`.

## Architecture reference

### Package purpose

Quartet calculates tree similarity metrics based on four-taxon subtrees
(quartets). It wraps the **tqDist algorithm** (Sand et al. 2014, O(n log n))
via C++ and provides a suite of R-level similarity/distance metrics.

Supports trees with up to **477 leaves** (32-bit integer constraint in the
underlying C library).

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
| Imports | `ape`, `PlotTools`, `Rdpack`, `Ternary`, `viridisLite` |
| Suggests | `phangorn`, `testthat`, `knitr`, `rmarkdown`, `vdiffr` |

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
