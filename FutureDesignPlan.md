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

- Python source: `D:\Python\pySoftPLC\mbsoftlogicck\PLCLadder.py` (the actual ladder generator)
- Example Python JSON: `D:\Python\pySoftPLC\simpleconveyor_subr.txt.json`
- Current CL ladder generator: `D:\common-lisp\mblogic-cl\src\web\ladder-render.lisp`

## Status

**IMPLEMENTED** - The Python-compatible matrixdata format is now implemented in `ladder-render.lisp`.

### Verification Test

The following IL program:
```
NETWORK 1
STR X1
AND X2
STR X3
OR Y1
ANDSTR
OUT Y1
```

Now produces this JSON (matching Python format exactly):
```json
{
  "rungtype": "single",
  "matrixdata": [
    {"type": "inp", "row": 0, "col": 0, "addr": ["X1"], "value": "noc", "monitor": ["bool", "X1"]},
    {"type": "inp", "row": 0, "col": 1, "addr": ["X2"], "value": "noc", "monitor": ["bool", "X2"]},
    {"type": "inp", "row": 0, "col": 2, "addr": [], "value": "branchttr", "monitor": ["none"]},
    {"type": "inp", "row": 0, "col": 3, "addr": ["X3"], "value": "noc", "monitor": ["bool", "X3"]},
    {"type": "inp", "row": 0, "col": 4, "addr": [], "value": "branchttl", "monitor": ["none"]},
    {"type": "inp", "row": 1, "col": 2, "addr": [], "value": "branchr", "monitor": ["none"]},
    {"type": "inp", "row": 1, "col": 3, "addr": ["Y1"], "value": "noc", "monitor": ["bool", "Y1"]},
    {"type": "inp", "row": 1, "col": 4, "addr": [], "value": "branchl", "monitor": ["none"]},
    {"type": "outp", "row": 0, "col": 0, "addr": ["Y1"], "value": "out", "monitor": ["bool", "Y1"]}
  ]
}
```

Visual representation:
```
Col:   0      1      2          3      4          output
Row 0: [X1]   [X2]   branchttr  [X3]   branchttl  (Y1)
Row 1:               branchr    [Y1]   branchl
```

---

## Technical Analysis of Python Source (PLCLadder.py)

The matrixdata generation is NOT in `PLCCompile.py` but in **`PLCLadder.py`** (`mbsoftlogicck/PLCLadder.py`). The key class is `RungAssembler`.

### Complete Branch Connector Types

From `PLCLadder.py` lines 79-94:

| Type | Python Name | Side | Visual | Usage |
|------|-------------|------|--------|-------|
| Top-Right | `branchttr` | Right | ┐ | Top of branch fork (ANDSTR right side) |
| Middle-Right | `branchtr` | Right | ┤ | Middle rows of branch fork |
| Bottom-Right | `branchr` | Right | ┘ | Bottom of branch fork |
| Vertical-Right | `vbarr` | Right | │ | Vertical only, no junction (right side) |
| Top-Left | `branchttl` | Left | ┌ | Top of merge (OR/ORSTR right side) |
| Middle-Left | `branchtl` | Left | ├ | Middle rows of merge |
| Bottom-Left | `branchl` | Left | └ | Bottom of merge |
| Vertical-Left | `vbarl` | Left | │ | Vertical only, no junction (left side) |
| Horizontal | `hbar` | — | ─ | Horizontal wire segment |

### Python Matrix Algorithm

The core algorithm in `_InputsToMatrixSingle()` (lines 248-313):

```python
# Matrix is stored as list of rows: [[row0], [row1], [row2], ...]
currentmatrix = [[]]
matrixstack = []  # Stack for nested branch blocks

for instr in self._Inputs:
    if instclass == 'store':     # STR instruction
        matrixstack.append(currentmatrix)
        currentmatrix = [[]]
        currentmatrix = self._AppendInputCell(instr, currentmatrix)

    elif instclass == 'and':      # AND instruction
        currentmatrix = self._AppendInputCell(instr, currentmatrix)

    elif instclass == 'or':       # OR instruction
        newmatrix = [[]]
        newmatrix = self._AppendInputCell(instr, newmatrix)
        currentmatrix = self._MergeBelow(currentmatrix, newmatrix)
        currentmatrix = self._CloseBlock(currentmatrix)  # <-- Adds right-side connectors

    elif instclass == 'orstr':    # ORSTR instruction
        oldmatrix = matrixstack.pop()
        currentmatrix = self._MergeBelow(oldmatrix, currentmatrix)
        currentmatrix = self._CloseBlock(currentmatrix)  # <-- Adds right-side connectors

    elif instclass == 'andstr':   # ANDSTR instruction
        oldmatrix = matrixstack.pop()
        currentmatrix = self._MergeRight(oldmatrix, currentmatrix)  # <-- Adds left-side connectors
```

### Key Methods

#### `_MergeRight()` - For ANDSTR (lines 465-501)

Merges two matrices horizontally (series connection). Inserts **left-side** branch connectors on the new matrix:

```python
# Add branches to left side of new matrix (if multi-row)
if newheight > 1:
    for row in newmatrix:
        row.insert(0, dict(self._BranchTRCell))  # Default: branchtr

    # Fix corners
    newmatrix[0][0]['value'] = 'branchttr'   # Top: ┐
    newmatrix[-1][0]['value'] = 'branchr'    # Bottom: ┘
```

#### `_CloseBlock()` - For OR/ORSTR (lines 506-595)

Adds **right-side** branch connectors after merging rows below:

```python
# Fix up the top row
matrixblock[0].pop()
matrixblock[0].append(dict(self._BranchTTLCell))  # branchttl: ┌

# Fix up middle rows (branchtl: ├) - handled in the loop

# Fix up the bottom row
matrixblock[lastrow].pop()
matrixblock[lastrow].append(dict(self._BranchLCell))  # branchl: └
```

#### `_MergeBelow()` - For OR/ORSTR (lines 418-460)

Merges matrices vertically (parallel connection). Pads narrower matrix with `hbar` cells.

### Standard Cell Templates (lines 98-103)

```python
self._HBarCell = {'value': 'hbar', 'addr': [], 'monitor': 'none'}
self._VBarLCell = {'value': 'vbarl', 'addr': [], 'monitor': 'none'}
self._BranchLCell = {'value': 'branchl', 'addr': [], 'monitor': 'none'}
self._BranchTRCell = {'value': 'branchtr', 'addr': [], 'monitor': 'none'}
self._BranchTLCell = {'value': 'branchtl', 'addr': [], 'monitor': 'none'}
self._BranchTTLCell = {'value': 'branchttl', 'addr': [], 'monitor': 'none'}
```

### Output Format (lines 600-671)

The `GetLadderData()` method converts the internal matrix to JSON format:

```python
matrixdata.append({
    'type': 'inp',          # or 'outp' for outputs
    'row': row,
    'col': col,
    'addr': cell['addr'],   # [] for connectors
    'value': cell['value'], # e.g., 'noc', 'branchttr', etc.
    'monitor': monitor      # ('none',) for connectors
})
```

---

## Implementation Plan for Common Lisp

### Step 1: Add Branch Cell Templates

Add to `ladder-render.lisp`:

```lisp
(defun make-branch-cell (type row col)
  "Create a branch connector cell"
  (make-ladder-cell
   :type :branch
   :symbol type  ; "branchttr", "branchttl", etc.
   :address nil
   :addresses nil
   :row row
   :col col
   :monitor-type nil))
```

### Step 2: Replace Current Algorithm

Replace `network-to-ladder-rung` with a matrix-based approach:

1. Use a 2D list-of-lists structure (like Python)
2. Maintain a stack for STR instructions
3. On AND: append cell to current row
4. On OR: create new row, merge below, close block (add right connectors)
5. On ORSTR: pop stack, merge below, close block
6. On ANDSTR: pop stack, merge right (add left connectors)

### Step 3: Implement Helper Functions

Port these Python methods:
- `_MergeBelow` → `merge-matrix-below`
- `_MergeRight` → `merge-matrix-right`
- `_CloseBlock` → `close-branch-block`
- `_AppendInputCell` → `append-cell-to-matrix`

### Step 4: Update JSON Export

Change `cell-to-plist` to emit Python-compatible format:

```lisp
(defun cell-to-matrixdata (cell)
  (list :type (if (eq (ladder-cell-type cell) :coil) "outp" "inp")
        :row (ladder-cell-row cell)
        :col (ladder-cell-col cell)
        :addr (or (ladder-cell-addresses cell) #())
        :value (ladder-cell-symbol cell)
        :monitor (or (format-monitor cell) '("none"))))
```

### Step 5: Remove JavaScript Complexity

Once CL generates explicit branch cells, remove from JavaScript:
- `computeBranchConnectors()`
- `fillHorizontalWires()`
- Branch metadata handling in `renderLadderRung()`

---

## Reference Files (Updated)

- **Python ladder generator**: `D:\Python\pySoftPLC\mbsoftlogicck\PLCLadder.py`
- **Python compiler**: `D:\Python\pySoftPLC\mbsoftlogicck\PLCCompile.py`
- **Example Python JSON**: `D:\Python\pySoftPLC\simpleconveyor_subr.txt.json`
- **Current CL ladder**: `D:\common-lisp\mblogic-cl\src\web\ladder-render.lisp`
