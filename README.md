# MBLogic-CL

A Common Lisp port of the PLC (Programmable Logic Controller) compiler and interpreter components from the MBLogic industrial automation system.

## Overview

This project implements a soft PLC system that:
- Parses Instruction List (IL) programs
- Compiles IL to native Common Lisp code
- Executes PLC programs with scan-based real-time behavior
- Supports ~100 IL instructions including logic, timers, counters, math, and data operations

## Original Source

Ported from MBLogic (Python 2.7), a GPL v3 licensed industrial automation system.

- **Original Location**: `/Users/gregorybrooks/Python/mblogic_all/mblogic_2011-04-16/mblogic/mbsoftlogicck/`
- **License**: GPL-3.0
- **Lines of Code**: ~8,400 (Python) → TBD (Common Lisp)

## Installation

### Prerequisites
- Common Lisp implementation (SBCL recommended)
- Quicklisp

### Setup
```bash
cd ~/common-lisp/mblogic-cl

# In your Lisp REPL:
(ql:quickload :mblogic-cl)
```

## Usage

```lisp
(in-package :mblogic-cl)

;; Load and compile a PLC program
(defparameter *program* 
  (with-open-file (in "test/plcprog.txt")
    (loop for line = (read-line in nil)
          while line
          collect line)))

;; Create parser and parse program
(defparameter *parser* (make-il-parser *program*))
(defparameter *parsed* (parse-program *parser*))

;; Compile to executable code
(defparameter *compiler* (make-il-compiler))
(defparameter *compiled* (compile-program *compiler* *parsed*))

;; Create interpreter and run
(defparameter *interp* (make-plc-interpreter *compiled*))

;; Run single scan
(run-scan *interp*)

;; Or run continuously
(run-continuous *interp*)
```

## Testing

```bash
# Run all tests
(asdf:test-system :mblogic-cl)

# Run specific test suite
(fiveam:run! 'mblogic-cl-test:data-table-suite)
```

## Project Structure

```
mblogic-cl/
├── README.md                  # This file
├── warp.md                    # Detailed implementation guide
├── mblogic-cl.asd            # ASDF system definition
├── src/                       # Source code
│   ├── package.lisp          # Package definitions
│   ├── data-table.lisp       # Memory/data structures
│   ├── instructions.lisp     # Instruction set definitions
│   ├── parser.lisp           # IL parser
│   ├── compiler.lisp         # Code generator
│   ├── interpreter.lisp      # Runtime engine
│   ├── math-lib.lisp         # Math operations
│   ├── timer-counter.lisp    # Timers and counters
│   └── table-ops.lisp        # Data table operations
└── test/                      # Test suite
    ├── test-suite.lisp       # Main test framework
    ├── test-data-table.lisp
    ├── test-parser.lisp
    ├── test-compiler.lisp
    ├── test-interpreter.lisp
    └── plcprog.txt           # Sample PLC program
```

## Implementation Status

### Phase 1: Core Data Structures ⏳
- [ ] Data table classes
- [ ] Address initialization
- [ ] Read/write operations

### Phase 2: Instruction Definitions ⏳
- [ ] CLOS instruction classes
- [ ] ~100 instruction definitions
- [ ] Parameter validators

### Phase 3: IL Parser ⏳
- [ ] Tokenizer
- [ ] Network/subroutine parsing
- [ ] Error reporting

### Phase 4: Compiler ⏳
- [ ] Lisp form generation
- [ ] Native code compilation
- [ ] Optimization

### Phase 5: Runtime Interpreter ⏳
- [ ] Scan execution
- [ ] Call stack tracking
- [ ] Condition handling

### Phase 6: Standard Libraries ⏳
- [ ] Math library
- [ ] Timer/Counter library
- [ ] Table operations

### Phase 7: Testing & Validation ⏳
- [ ] Unit tests
- [ ] Integration tests
- [ ] Performance benchmarks

## IL Language Support

### Supported Instructions

#### Boolean Logic
- STR, STRN - Store (NOT) onto logic stack
- AND, ANDN - AND (NOT) with logic stack
- OR, ORN - OR (NOT) with logic stack
- ANDSTR, ORSTR - Combine logic stack levels
- OUT, SET, RST - Output operations

#### Comparisons
- STRE, STRNE - Equal/Not Equal
- STRGT, STRGE - Greater Than/Equal
- STRLT, STRLE - Less Than/Equal
- (AND/OR variants available)

#### Timers & Counters
- TMR - On-delay timer
- TMRA - Accumulating timer
- TMROFF - Off-delay timer
- CNTU, CNTD - Up/Down counters
- UDC - Up/Down counter

#### Data Operations
- COPY - Copy single value
- CPYBLK - Copy block
- FILL - Fill range
- PACK/UNPACK - Bit manipulation
- FINDEQ, FINDGT, etc. - Search operations

#### Math
- MATHDEC - Decimal math expressions
- MATHHEX - Hex/bitwise operations
- Trig functions (SIN, COS, TAN, etc.)
- SQRT, ABS, EXP, LOG, POW

#### Control Flow
- CALL - Call subroutine
- RT, RTC - Return (conditional)
- END, ENDC - End program (conditional)
- FOR/NEXT - Loops

## Contributing

This is a personal project, but suggestions and improvements are welcome.

## License

GPL-3.0 (same as original MBLogic)

Co-Authored-By: Warp <agent@warp.dev>

## See Also

- **warp.md** - Detailed implementation plan and architecture
- **test/plcprog.txt** - Sample PLC program from original MBLogic
