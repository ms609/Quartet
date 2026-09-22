# Quartet — Coordination

## Strategic Objectives

| Phase | Objective | Status |
|----|----|----|
| 1 | Maintain CRAN compliance and CI green | Ongoing |
| 2 | Address any open GitHub issues / user-reported bugs | Pending |
| 3 | Improve documentation and vignettes | Pending |
| 4 | Evaluate 32-bit integer limit (max 477 leaves) — possible 64-bit migration | Future |

### ICQ Feature (branch: `icq`, worktree: `../Q-IC`)

| Phase | Objective | Status |
|----|----|----|
| ICQ-1 | R implementation + tests | Complete (on `icq`, unmerged) |
| ICQ-2 | Analytical shortcut investigation (rejected — \#P-hard) | Complete (on `icq`, unmerged) |
| ICQ-3 | Performance: ReduceTrees preprocessing, raise exact limit to 9 | Complete (on `icq`, unmerged) |
| ICQ-4 | Multi-tree API (all-pairs/one-to-many/element-wise) + `future.apply` parallelism + `normalize` | Complete (on `icq`, unmerged) |
| ICQ-5 | Vignette, NEWS.md, CRAN compliance check | Complete (on `icq`, unmerged) |
| ICQ-6 | On-the-fly C++ topology enumeration (drop cache, raise limit 9→11) | **Uncommitted WIP in `../Q-IC`**, untested since 2026-03-23 |

`icq` is 11 commits ahead of, and 44 behind, `main` — none of this has
ever been merged. Next step is to rebase `icq` onto `main`, resolve the
ICQ-6 working-tree diff (commit, rebuild, and test it, or discard it),
and open a PR. See conversation of 2026-09-22 for the full audit.
`main`’s own `to-do.md`/`coordination.md` currently have no ICQ entries.

## Known Issues / Blockers

- **T-005 must not run concurrently with T-004** — both modify
  `src/QuartetDistanceCalculator.cpp`. Assign to the same agent or
  strictly sequence them.
- **`Rcpp::stop()` must not be called inside any OpenMP parallel
  region** — all T-004/T-005 work must replace in-region stops with
  error flags checked after the region closes.
- **VTune installed** at
  `C:\Program Files (x86)\Intel\oneAPI\vtune\2025.10\bin64` — T-003 and
  T-006 are unblocked.

## Architecture Decisions

| Date | Decision | Rationale |
|----|----|----|
| — | tqDist C++ code treated as vendored upstream | Minimize divergence from published algorithm |
| — | R-level API supports `phylo`, `multiPhylo`, `Splits`, and lists | Match TreeTools ecosystem conventions |
| 2026-03-18 | Parallelism via OpenMP, per-thread `QuartetDistanceCalculator` instances | Instance member variables make single instance non-re-entrant; per-thread construction is cheap |
| 2026-03-18 | `Rcpp::stop()` replaced with error-flag + post-region stop in parallel loops | `Rcpp::stop()` uses longjmp; unsafe inside OMP worker threads |
| 2026-03-18 | T-001 correctness corpus is a blocking gate for all optimisation | Exact correctness is the primary constraint; optimisations must be verifiable |

## Notes for Agents

- This is a **mature, stable** package. Prefer minimal, targeted
  changes.
- `src/` contains vendored tqDist code. Avoid modifications there unless
  fixing a confirmed bug; document any changes thoroughly.
- Check `issues.md` before `to-do.md` when picking up work.
- All builds and tests must use a per-agent library (`.agent-X/`).

## Known Staleness (audited 2026-09-22)

- **`to-do.md`’s Open Tasks table lists T-001/T-005/T-008/T-009 as
  `COMPLETE`** while they’re also correctly listed in the Completed
  Tasks table below — these rows should be removed from Open Tasks, not
  just marked complete.
- **Three extra worktrees are parked at the same stale detached commit**
  (`3abb47e`, “Harden CPDT/tqDist memory safety”, 2026-07-03), each
  pinned to its own now-merged-or-abandoned `claude/*` branch:
  `worktrees/Quartet/eloquent-montalcini-db94cd`,
  `worktrees/Quartet/happy-cori-bbaddc`,
  `worktrees/Quartet/quizzical-moore-2e5b84`. Worth checking whether
  that commit’s content ever reached `main` before removing them.
- **`origin/icq2`** is an unrelated, much older (2020) abandoned ICQ
  prototype — not part of the current `icq` branch history, large stale
  diff vs `main`. Candidate for deletion once confirmed nothing in it is
  still needed.
