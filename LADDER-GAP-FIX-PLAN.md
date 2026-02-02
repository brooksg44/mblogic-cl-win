# Ladder Diagram Gap Fix Plan

## Current State (2026-01-31)

### Files Analyzed
- `src/web/ladder-render.lisp` - Lisp code that converts IL to ladder matrix
- `static/js/ladsubrdisplib.js` - JavaScript that renders the matrix to HTML
- `static/js/ladsymbols.js` - SVG symbol definitions
- `static/css/ladder.css` - Styling for ladder diagram
- `test/plcprog.txt` - Test program with LadderDemo subroutine

### Problem Identified

The gap issue is in the **fork/branch connection logic** in `ladder-render.lisp`.

When an OR instruction creates a parallel branch:
1. `merge-matrix-below` merges the new row below existing rows
2. It pads shorter rows with `hbar` or `nil`
3. **BUT it doesn't add fork connectors on the LEFT side of the branch!**

In Python's PLCLadder.py, when OR branches are created:
- Fork column gets `branchttr` (┬) at top row
- Fork column gets `branchr` (┴) at bottom row
- Middle rows get `branchtr` (┼)

The current Lisp code only adds RIGHT-side connectors via `close-branch-block` (for ORSTR merge points).

### Root Cause

In `merge-matrix-below` (lines 392-424):
- Pads with hbar/nil but doesn't insert fork connector cells
- The fork point column needs explicit connector cells

### Fix Required

Modify `merge-matrix-below` to:
1. Identify the fork column (first column where the new branch has a cell)
2. Insert fork connector cells at that column:
   - Top row: `branchttr` (┬) - horizontal through with vertical down
   - Middle rows: `branchtr` (┼) - horizontal through with vertical both ways
   - Bottom row (new branch): `branchr` (┴) - horizontal through with vertical up

### Test Case

LadderDemo Network 5 from `test/plcprog.txt`:
```
STR C10
AND C11
OR C12        <-- Creates parallel branch, needs fork connector at col 0
STRN C13
ANDN C14
ORN C15       <-- Another parallel branch
ANDSTR
STR C16
OR C17
AND C18
ORSTR
OUT C104
...
```

### Implementation Steps

1. **Update `merge-matrix-below`** in `ladder-render.lisp`:
   - Calculate fork column (where OR branch cell is placed)
   - Before merging, add fork connectors:
     - Add `branchttr` cell to original-matrix rows at fork column
     - The new row already has its contact cell at that column

2. **Alternative approach** - Add fork connectors after detecting OR pattern:
   - In the main processing loop, when OR is detected:
     - Before calling `merge-matrix-below`, insert fork cells
   - This may be cleaner as it separates fork logic from merge logic

3. **Test with complex rungs**:
   - Network 5, 6, 7 from LadderDemo
   - Networks with nested OR/ANDSTR/ORSTR

### Code Location References

- `merge-matrix-below`: lines 392-424
- `close-branch-block`: lines 478-506 (RIGHT-side connectors - works correctly)
- OR instruction handling: lines 601-606
- Branch cell constructors: lines 134-176

### Next Session Commands

```lisp
;; Load systems
(ql:quickload :mblogic-cl)
;; Load web package manually since it may have different name
(load "src/web/package.lisp")
(load "src/web/ladder-render.lisp")

;; Test parsing
(defvar *prog* (mblogic-cl:parse-file "test/plcprog.txt"))
(defvar *ladder-demo* (gethash "LadderDemo" (mblogic-cl:program-subroutines *prog*)))

;; Get network 5 (complex branches)
(defvar *net5* (nth 4 (mblogic-cl:subroutine-networks *ladder-demo*)))

;; Convert to ladder and inspect
(defvar *rung5* (mblogic-cl-web:network-to-ladder-rung *net5*))
```
