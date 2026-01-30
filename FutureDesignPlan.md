# Future Design Plan: Ladder Diagram JSON Format

## Overview

This document outlines a planned improvement to the ladder diagram JSON export format to simplify browser-side rendering.

## Problem Statement

The current Common Lisp implementation generates an abstract JSON format where branch information is stored separately from cell data. This requires complex JavaScript logic to compute branch connector positions at render time, leading to edge-case bugs and overly complicated rendering code.

## Comparison: Python MBLogic vs Current Common Lisp Format

### Original Python MBLogic `matrixdata` Format

The Python version treats the ladder diagram as a **fully-populated matrix** where branch connectors are explicit cells:

```json
// From simpleconveyor_subr.txt.json - Network 1 (SimpleConveyor)
{"type": "inp", "row": 0, "col": 0, "addr": ["X1"], "value": "noc", "monitor": ["bool", "X1"]},
{"type": "inp", "row": 0, "col": 1, "addr": ["X2"], "value": "noc", "monitor": ["bool", "X2"]},
{"type": "inp", "row": 0, "col": 2, "addr": [], "value": "branchttr", "monitor": ["none"]},
{"type": "inp", "row": 0, "col": 3, "addr": ["X3"], "value": "noc", "monitor": ["bool", "X3"]},
{"type": "inp", "row": 0, "col": 4, "addr": [], "value": "branchttl", "monitor": ["none"]},
{"type": "inp", "row": 1, "col": 2, "addr": [], "value": "branchr", "monitor": ["none"]},
{"type": "inp", "row": 1, "col": 3, "addr": ["Y1"], "value": "noc", "monitor": ["bool", "Y1"]},
{"type": "inp", "row": 1, "col": 4, "addr": [], "value": "branchl", "monitor": ["none"]},
{"type": "outp", "row": 0, "col": 0, "addr": ["Y1"], "value": "out", "monitor": ["bool", "Y1"]}
```

### Branch Connector Types

| Type | Meaning | Visual Description |
|------|---------|-------------------|
| `branchttr` | Branch Top-To-Right | Where main row drops down to start a parallel branch (top-right corner) |
| `branchttl` | Branch Top-To-Left | Where branches merge back together (top-left corner) |
| `branchr` | Branch Right | Vertical connector on the right side of a branch |
| `branchl` | Branch Left | Vertical connector on the left side of a branch |
| `vbranch` | Vertical Branch | Vertical line segment between rows |

### Visual Representation

For this IL code:
```
STR X1
AND X2
STR X3
OR Y1
ANDSTR
OUT Y1
```

The Python `matrixdata` explicitly places every visual element:
```
Col:   0      1      2          3      4          output
Row 0: [X1]   [X2]   branchttr  [X3]   branchttl  (Y1)
Row 1:               branchr    [Y1]   branchl
```

### Current Common Lisp Format

The current format stores only logical elements and describes branches abstractly:

```json
{
    "cells": [
        {"type": "contact", "symbol": "noc", "addr": "C1", "row": 0, "col": 0},
        {"type": "contact", "symbol": "noc", "addr": "T1", "row": 1, "col": 0}
    ],
    "branches": [
        {"row": 1, "startCol": 0, "mergeCol": 1}
    ]
}
```

## Why Python Approach is Better for Rendering

1. **No computation needed** - The renderer iterates through `matrixdata` and draws each cell based on its `value` field. Branch connectors are first-class citizens.

2. **Clear cell-to-visual mapping** - Each `value` maps directly to a specific graphic:
   - `noc` → normally open contact
   - `ncc` → normally closed contact
   - `branchttr` → corner piece (down-right)
   - `branchl` → T-junction (left)

3. **Gaps are explicit** - Empty cells in the matrix are empty; no need to determine if a wire should pass through.

4. **Simpler JavaScript** - The browser just renders what it receives, no layout algorithm needed.

## Current JavaScript Complexity

With the abstract `branches` format, JavaScript must:
1. Determine where to draw vertical lines between `startCol` and `mergeCol`
2. Calculate which cells need corner connectors vs straight connectors
3. Handle multiple overlapping branches (staircase patterns in complex rungs)
4. Fill in horizontal wire segments between elements
5. Handle output branch columns for parallel coils

This is error-prone, especially with:
- Complex nested branches
- Multiple parallel output coils
- Staircase merge patterns (multiple ORs merging at different columns)

## Proposed Solution: Option A

Modify the Common Lisp compiler to generate Python-style `matrixdata` with explicit branch connector cells.

### Implementation Steps

1. **Analyze Python source** - Study how `PLCCompile.py` generates `matrixdata` with branch connectors

2. **Modify `il-to-ladder.lisp`** - Update the ladder diagram generator to:
   - Calculate the full matrix dimensions
   - Insert branch connector pseudo-cells at appropriate positions
   - Use the same `value` names as Python (`branchttr`, `branchttl`, `branchr`, `branchl`, `vbranch`)

3. **Update JSON export** - Emit cells in Python-compatible format with:
   - `type`: "inp" or "outp"
   - `value`: the symbol/connector type
   - `addr`: address array (empty for connectors)
   - `monitor`: monitoring info (["none"] for connectors)

4. **Simplify JavaScript** - Once the JSON contains explicit connectors, the rendering code can be significantly simplified to just iterate and draw.

### Benefits

- **Single source of truth** - Layout computed once at compile time
- **Simpler JavaScript** - Remove complex branch computation logic
- **Fewer bugs** - No render-time edge cases
- **Better maintainability** - Layout logic in one place (Lisp compiler)
- **Compatibility** - Could potentially reuse existing MBLogic browser code

## Reference Files

- Python source: `D:\Python\pySoftPLC\` (original MBLogic)
- Example Python JSON: `D:\Python\pySoftPLC\simpleconveyor_subr.txt.json`
- Current CL JSON: `D:\common-lisp\mblogic-cl\LadderDemo_v2.json`
- Ladder generator: `D:\common-lisp\mblogic-cl\src\il-to-ladder.lisp`

## Status

**Not Started** - This is a future enhancement to pursue when time permits.
