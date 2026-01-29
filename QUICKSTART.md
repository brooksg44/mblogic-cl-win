# Quick Start Guide

## Project Setup Complete! ✓

The MBLogic-CL project structure has been created at:
```
~/common-lisp/mblogic-cl/
```

## What's Been Created

### Documentation
- **warp.md** - Comprehensive implementation guide with 7 phases
- **README.md** - User-facing documentation
- **QUICKSTART.md** - This file

### Code Structure
- **mblogic-cl.asd** - ASDF system definition
- **src/package.lisp** - Package exports and definitions
- **src/data-table.lisp** - Phase 1 implementation (ready to use!)
- **src/instructions.lisp** - Phase 2 stub
- **src/parser.lisp** - Phase 3 stub
- **src/compiler.lisp** - Phase 4 stub
- **src/interpreter.lisp** - Phase 5 stub
- **src/math-lib.lisp** - Phase 6 stub
- **src/timer-counter.lisp** - Phase 6 stub
- **src/table-ops.lisp** - Phase 6 stub

### Tests
- **test/plcprog.txt** - Sample IL program from original MBLogic
- **test/test-*.lisp** - Test stubs for each component

## Next Steps

### 1. Load the Project
```lisp
;; In your SBCL or other CL REPL
(ql:quickload :mblogic-cl)
```

### 2. Start with Phase 1 (Already Implemented!)
```lisp
(in-package :mblogic-cl)

;; Create and initialize a data table
(defparameter *dt* (make-data-table))
(init-data-table *dt*)

;; Test Boolean operations
(set-bool *dt* "X1" t)
(get-bool *dt* "X1")  ; => T

;; Test Word operations
(set-word *dt* "DS100" 42)
(get-word *dt* "DS100")  ; => 42

;; Test Float operations
(set-float *dt* "DF1" 3.14159)
(get-float *dt* "DF1")  ; => 3.14159

;; Test String operations
(set-string *dt* "TXT1" "Hello PLC")
(get-string *dt* "TXT1")  ; => "Hello PLC"
```

### 3. Read the Implementation Plan
Open **warp.md** for:
- Detailed architecture overview
- Step-by-step implementation guides for each phase
- Code examples and design patterns
- Testing strategies

### 4. Begin Phase 2
Start implementing the instruction set:
```lisp
;; Edit src/instructions.lisp
;; Follow the Phase 2 guide in warp.md
```

## Development Workflow

### Run Tests (when implemented)
```lisp
(asdf:test-system :mblogic-cl)
```

### Interactive Development
```lisp
;; Load system
(ql:quickload :mblogic-cl)

;; Make changes to source files
;; Then reload:
(asdf:load-system :mblogic-cl :force t)

;; Or reload just one file:
(load "src/data-table.lisp")
```

### Debugging Tips
```lisp
;; Set debug mode
(declaim (optimize (debug 3) (speed 0) (safety 3)))

;; Trace function calls
(trace get-bool set-bool)

;; Untrace
(untrace)
```

## Implementation Order (from warp.md)

1. **Phase 1**: Core Data Structures ✅ (Basic implementation done!)
2. **Phase 2**: Instruction Definitions (Week 2-3)
3. **Phase 3**: IL Parser (Week 3-4)
4. **Phase 4**: Compiler (Week 4-6)
5. **Phase 5**: Runtime Interpreter (Week 6-7)
6. **Phase 6**: Standard Libraries (Week 7-9)
7. **Phase 7**: Testing & Validation (Week 9-10)

## Reference the Original Python Code

The original MBLogic Python source is at:
```
/Users/gregorybrooks/Python/mblogic_all/mblogic_2011-04-16/mblogic/mbsoftlogicck/
```

Key files to reference:
- `DLCkDataTable.py` - Data table reference
- `DLCkInstructions.py` - All ~100 instruction definitions
- `PLCCompile.py` - Parser/compiler logic
- `PLCInterp.py` - Runtime execution
- `DLCkMath.py` - Math operations
- `DLCkCounterTimer.py` - Timers and counters

## Testing Against Sample Program

The sample PLC program from MBLogic is in:
```
test/plcprog.txt
```

This demonstrates:
- Boolean logic (STR, AND, OR, OUT)
- Timers (TMR)
- Counters (CNTU, CNTD)
- Math operations (MATHDEC, MATHHEX)
- Subroutine calls (CALL, RT)
- Data operations (COPY, CPYBLK)

## Questions?

Refer to:
- **warp.md** for implementation details
- **README.md** for usage examples
- Original Python source for behavior reference

## Happy Coding! 🚀

Remember: This is a 7-phase project spanning ~10 weeks. Take it one phase at a time!
