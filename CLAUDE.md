# MBLogic-CL

A Common Lisp port of the PLC compiler/interpreter from MBLogic (Python 2.7 industrial automation system). Creates a standalone soft PLC that compiles and executes Instruction List (IL) programs.

## Quick Reference

- **Language**: Common Lisp (SBCL)
- **Build System**: ASDF
- **License**: GPL v3
- **Original Source**: `/Users/gregorybrooks/Python/mblogic_all/mblogic_2011-04-16/mblogic/mbsoftlogicck/`

## Project Structure

```
mblogic-cl/
├── mblogic-cl.asd            # ASDF system definition
├── src/
│   ├── package.lisp          # Package definitions
│   ├── data-table.lisp       # Data structures (bool/word/float/string tables)
│   ├── instructions.lisp     # IL instruction definitions (~100 instructions)
│   ├── parser.lisp           # IL parser
│   ├── compiler.lisp         # Code generator (IL -> Lisp forms -> native)
│   ├── interpreter.lisp      # Runtime engine (scan-based execution)
│   ├── math-lib.lisp         # MATHDEC/MATHHEX operations
│   ├── timer-counter.lisp    # TMR/TMRA/TMROFF, CNTU/CNTD/UDC
│   └── table-ops.lisp        # COPY/CPYBLK/FILL/PACK/FIND operations
└── test/
    ├── test-suite.lisp       # FiveAM test suite
    └── plcprog.txt           # Sample IL program
```

## Dependencies

- `cl-ppcre` - Regular expressions for parsing
- `alexandria` - Common utilities
- `parse-number` - Number parsing
- `local-time` - Timestamp handling (optional)
- `fiveam` - Testing framework

## Architecture

1. **IL Parser** - Tokenizes text IL programs using cl-ppcre
2. **Compiler** - Generates Lisp forms, compiles to native code via CL `compile`
3. **Interpreter** - Executes compiled programs in scan cycles
4. **Data Tables** - Hash-table based memory spaces

## Data Table Address Spaces

| Type | Addresses | Description |
|------|-----------|-------------|
| Boolean | X1-X2000 | Inputs |
| Boolean | Y1-Y2000 | Outputs |
| Boolean | C1-C2000 | Control relays |
| Boolean | SC1-SC1000 | System control |
| Boolean | T1-T500 | Timer bits |
| Boolean | CT1-CT250 | Counter bits |
| Word | DS1-DS10000 | Signed integer |
| Word | DD1-DD2000 | Double integer |
| Word | DH1-DH2000 | Hex/unsigned |
| Float | DF1-DF2000 | Floating point |
| String | TXT1-TXT10000 | Text |

## Instruction Categories

- **Boolean Logic**: STR, STRN, AND, ANDN, OR, ORN, ANDSTR, ORSTR
- **Outputs**: OUT, SET, RST, PD
- **Comparisons**: STRE, STRGT, STRLT, STRGE, STRLE, STRNE
- **Edges**: STRPD, STRND, ANDPD, ANDND, ORPD, ORND
- **Timers**: TMR, TMRA, TMROFF
- **Counters**: CNTU, CNTD, UDC
- **Data Movement**: COPY, CPYBLK, FILL, PACK, UNPACK
- **Math**: MATHDEC, MATHHEX
- **Search**: FINDEQ, FINDGT, FINDLT, etc.
- **Control**: CALL, RT, RTC, END, ENDC, FOR, NEXT, NETWORK, SBR

## Development

```bash
# Load in REPL
(ql:quickload :mblogic-cl)

# Run tests
(asdf:test-system :mblogic-cl)
```

## Key Python Source Files (for reference)

| File | Purpose |
|------|---------|
| DLCkDataTable.py | Data structure definitions |
| DLCkInstructions.py | Complete instruction set |
| PLCCompile.py | Parser/compiler logic |
| PLCInterp.py | Runtime behavior |
| DLCkMath.py | Math library |
| DLCkCounterTimer.py | Timer/counter implementations |
| DLCkTableInstr.py | Table operations |

## Compilation Strategy

IL source -> Parse to instruction objects -> Generate Lisp S-expressions -> CL `compile` to native code -> Execute in scan loop

## Conventions

- Use strings (not symbols) for PLC addresses
- Hash tables for address lookup
- CLOS classes for instructions and runtime objects
- CL condition system for PLC-specific errors
- Declare types in performance-critical paths
