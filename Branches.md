# Ladder Diagram Branch Symbols

This document describes the branch connector symbols used in ladder diagram rendering.

## Branch Connectors

### Wire Connectors (Basic)

| Symbol | Name | Description | ASCII |
|--------|------|-------------|-------|
| `hbar` | Horizontal Bar | Horizontal wire segment | `───` |
| `vbar` | Vertical Bar | Vertical wire segment | `│` |

### Corner Connectors

| Symbol | Name | Description | ASCII |
|--------|------|-------------|-------|
| `branchl` | Branch Left (Bottom-Left Corner) | Connects up and right - bottom of left-side branch | `└` |
| `branchr` | Branch Right (Bottom-Right Corner) | Connects up and left - bottom of right-side branch | `┘` |

### T-Junction Connectors

| Symbol | Name | Description | ASCII |
|--------|------|-------------|-------|
| `brancht` | Branch Top | Top of a branch - connects left, right, and down (with junction dot) | `┬●` |
| `branchtl` | Branch T-Left | Middle of left-side branch - connects up, down, and left (with junction dot) | `├●` |
| `branchtr` | Branch T-Right | Middle of right-side branch - connects up, down, and right (with junction dot) | `┤●` |
| `branchtu` | Branch T-Up | T pointing up - connects left, right, and up (with junction dot) | `┴●` |

### Cross Connector

| Symbol | Name | Description | ASCII |
|--------|------|-------------|-------|
| `branchx` | Branch Cross | Cross-over - connects all four directions (with junction dot) | `┼●` |

## Visual Reference

```
Branch Start (brancht):          Branch Bottom-Left (branchl):    Branch Bottom-Right (branchr):
    ───●───                          │                                    │
       │                             └───                              ───┘
       │

Branch T-Left (branchtl):        Branch T-Right (branchtr):       Branch T-Up (branchtu):
       │                              │                                │
    ───●                              ●───                          ───●───
       │                              │

Branch Cross (branchx):          Vertical Bar (vbar):             Horizontal Bar (hbar):
       │                              │
    ───●───                           │                            ─────────
       │                              │
```

## Usage in Ladder Logic

### OR Operations (Parallel Branches)

When an OR instruction creates a parallel path, left-side connectors are used:

```
    ──┬── C10 ─── C11 ──┬──
      │                 │
      └── C12 ─────────┘
```

- Top row: `brancht` at start, `brancht` at end
- Bottom row: `branchl` at start, `branchr` at end

### ANDSTR Operations (Series Merge)

When ANDSTR merges two parallel blocks, left-side merge connectors are added:

```
    ──┬── C10 ──┬──┌── C13 ──┬──
      │        │  │          │
      └── C12 ─┘  └── C15 ──┘
```

- Top of merge: `branchttl` (┌)
- Bottom of merge: `branchl` (└)

### ORSTR Operations (Parallel Merge)

ORSTR stacks matrices vertically. Branch connectors are already present from individual OR operations:

```
    ──┬── C10 ─── C11 ──┬──┌── C13 ─── C14 ──┬──
      │                 │  │                  │
      └── C12 ─────────┘  └── C15 ──────────┘
    ──┬── C16 ─── C18 ──────────────────────────
      │
      └── C17 ──────────────────────────────────
```

## Symbol Mapping

### Lisp Constants (ladder-render.lisp)

| Constant | Value | Description |
|----------|-------|-------------|
| `*branch-ttr*` | `"brancht"` | Top-right corner (mapped to `brancht` in JS) |
| `*branch-tr*` | `"branchtr"` | Middle-right T |
| `*branch-r*` | `"branchr"` | Bottom-right corner |
| `*branch-ttl*` | `"branchttl"` | Top-left corner (mapped to `brancht` in JS) |
| `*branch-tl*` | `"branchtl"` | Middle-left T |
| `*branch-l*` | `"branchl"` | Bottom-left corner |
| `*branch-tu*` | `"branchtu"` | Bottom T (T pointing up) |
| `*hbar*` | `"hbar"` | Horizontal wire |

### JavaScript Symbol Registry (ladsymbols.js)

The SVG symbols are registered in `LADSymbols.symbollist`:

- `brancht` - Top of branch
- `branchl` - Bottom-left corner
- `branchr` - Bottom-right corner
- `branchtl` - T-left junction
- `branchtr` - T-right junction
- `branchtu` - T-up junction
- `branchx` - Cross junction
- `vbar` - Vertical wire
- `hbar` - Horizontal wire
