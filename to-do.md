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
