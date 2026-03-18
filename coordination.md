# Quartet — Coordination

## Agent Status

| Agent | Status | Current Task |
|-------|--------|-------------|
| A | IDLE | T-001, T-004 complete |
| B | IDLE | T-002, T-003, T-006 complete |

## Strategic Objectives

| Phase | Objective | Status |
|-------|-----------|--------|
| 1 | **Runtime optimisation** — OpenMP parallelism + profiling-guided micro-opts | **Active** |
| 2 | Maintain CRAN compliance and CI green | Ongoing |
| 3 | Address any open GitHub issues / user-reported bugs | Pending |
| 4 | Improve documentation and vignettes | Pending |
| 5 | Evaluate 32-bit integer limit (max 477 leaves) — possible 64-bit migration | Future |

## Known Issues / Blockers

- **T-005 must not run concurrently with T-004** — both modify
  `src/QuartetDistanceCalculator.cpp`. Assign to the same agent or strictly
  sequence them.
- **`Rcpp::stop()` must not be called inside any OpenMP parallel region** —
  all T-004/T-005 work must replace in-region stops with error flags checked
  after the region closes.
- **VTune installed** at `C:\Program Files (x86)\Intel\oneAPI\vtune\2025.10\bin64` — T-003 and T-006 are unblocked.

## Architecture Decisions

| Date | Decision | Rationale |
|------|----------|-----------|
| — | tqDist C++ code treated as vendored upstream | Minimize divergence from published algorithm |
| — | R-level API supports `phylo`, `multiPhylo`, `Splits`, and lists | Match TreeTools ecosystem conventions |
| 2026-03-18 | Parallelism via OpenMP, per-thread `QuartetDistanceCalculator` instances | Instance member variables make single instance non-re-entrant; per-thread construction is cheap |
| 2026-03-18 | `Rcpp::stop()` replaced with error-flag + post-region stop in parallel loops | `Rcpp::stop()` uses longjmp; unsafe inside OMP worker threads |
| 2026-03-18 | T-001 correctness corpus is a blocking gate for all optimisation | Exact correctness is the primary constraint; optimisations must be verifiable |

## Notes for Agents

- This is a **mature, stable** package. Prefer minimal, targeted changes.
- `src/` contains vendored tqDist code. Avoid modifications there unless
  fixing a confirmed bug; document any changes thoroughly.
- Check `issues.md` before `to-do.md` when picking up work.
- All builds and tests must use a per-agent library (`.agent-X/`).
