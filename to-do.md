# Quartet — Task Queue

## Format

| ID | Priority | Status | Agent | Task | Notes |
|----|----------|--------|-------|------|-------|
| T-NNN | P0–P3 | OPEN / IN PROGRESS / BLOCKED | — or X | Short description | Details, blockers |

**Priority:** P0 = blocking / critical bug · P1 = next objective or correctness · P2 = important · P3 = nice-to-have

**Standing tasks** (always available; see `.positai/expertise/`):

| ID | Priority | Status | Agent | Task | Notes |
|----|----------|--------|-------|------|-------|
| S-RED | P1 | OPEN | — | Red-team review | Priority P1: <3 OPEN tasks |
| S-COORD | P1 | OPEN | — | Coordination review | Priority P1: <3 OPEN tasks |

---

## Open Tasks

| ID | Priority | Status | Agent | Task | Notes |
|----|----------|--------|-------|------|-------|
| T-013 | P3 | OPEN | — | Handle unifurcating root in tqDist (GitHub #64) | Upstream bug; CPDT backend may not have this issue — test and potentially route through CPDT. |
| T-014 | P3 | OPEN | — | Allow comparison when tips aren't identical (GitHub #60) | Modify `QuartetStatus()` to handle non-overlapping tip sets via confusion table + `choose()`. |

---

## Completed Tasks

| ID | Priority | Agent | Task | Completed |
|----|----------|-------|------|-----------|
| T-001 | P0 | A | Correctness regression corpus | 2026-03-18 |
| T-002 | P1 | B | Benchmarking infrastructure | 2026-03-18 |
| T-003 | P1 | B | VTune profiling and hotspot documentation | 2026-03-18 |
| T-004 | P1 | A+B | OpenMP all-pairs parallelism | 2026-03-18 (A wrote code; B fixed compile error, added Makevars) |
| T-006 | P2 | B | Profile-guided micro-optimisations (dummyRTFactory) | 2026-03-18 |
| T-005 | P2 | A | OpenMP one-to-many + pairs parallelism | 2026-03-18 |
| T-008 | P2 | A | All-pairs loop: per-thread localCalc + flatten triangle | 2026-03-18 |
| T-009 | P2 | A | Pool size increase + prefetch hints | 2026-03-18 |
| T-007 | P3 | B | VTune expertise file | 2026-03-18 (folded into T-003) |
