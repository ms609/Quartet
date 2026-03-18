# Quartet — Task Queue

## Format

| ID | Priority | Status | Agent | Task | Notes |
|----|----------|--------|-------|------|-------|
| T-NNN | P0–P3 | OPEN / IN PROGRESS / BLOCKED | — or X | Short description | Details, blockers |

**Priority:** P0 = blocking / critical bug · P1 = next objective or correctness · P2 = important · P3 = nice-to-have

**Standing tasks** (always available; see `.positai/expertise/`):

| ID | Priority | Status | Agent | Task | Notes |
|----|----------|--------|-------|------|-------|
| S-RED | P3 | OPEN | — | Red-team review | Priority rises as OPEN task count falls |
| S-COORD | P3 | OPEN | — | Coordination review | Priority rises as OPEN task count falls |

---

## Open Tasks

| ID | Priority | Status | Agent | Task | Notes |
|----|----------|--------|-------|------|-------|
| T-001 | P0 | COMPLETE | A | Correctness regression corpus | Hand-verified known-value tests; **required before any optimisation begins**. See plan. |
| T-005 | P2 | COMPLETE | A | OpenMP parallelism: one-to-many and pairs loops | Parallelised `oneToManyQuartetAgreement` and `pairs_quartet_distance`. C++ speedup: 4.6× at 400 tips. |
| T-008 | P2 | COMPLETE | A | All-pairs loop: per-thread localCalc + flatten triangle | +14–35% across all tree sizes. Post-opt benchmark: `inst/benchmarks/post-opt-T008-2026-03-18.rds`. |
| T-009 | P2 | OPEN | — | CountingLinkedList → flat array | Replace pointer-chasing `CountingLinkedList` / `CountingLinkedListNumOnly` traversals with contiguous arrays in HDT code. VTune: ~11.5% of serial CPU. Invasive to vendored tqDist code — review carefully, document all changes. Depends on T-008 completing first so a clean baseline exists. |

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
| T-007 | P3 | B | VTune expertise file | 2026-03-18 (folded into T-003) |
