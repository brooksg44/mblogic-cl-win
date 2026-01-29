# MBLogic-CL: PLC Compiler/Interpreter Port to Common Lisp

## Project Overview
This project ports the PLC (Programmable Logic Controller) compiler and interpreter components from the MBLogic Python 2.7 industrial automation system to Common Lisp. The goal is to create a standalone soft PLC system that compiles and executes Instruction List (IL) programs.

## Source Material
- **Original Project**: MBLogic (Python 2.7, ~8,400 lines for PLC components)
- **Location**: `/Users/gregorybrooks/Python/mblogic_all/mblogic_2011-04-16/mblogic/mbsoftlogicck/`
- **License**: GPL v3
- **Key Files**:
  - `PLCCompile.py` - IL parser and compiler
  - `PLCInterp.py` - Runtime interpreter
  - `DLCkDataTable.py` - Memory/data table definitions
  - `DLCkInstructions.py` - Instruction set (~100 instructions)
  - `PLClad2il.py` - Ladder diagram to IL converter
  - `DLCkMath.py`, `DLCkCounterTimer.py` - Standard libraries

## Architecture
The system consists of:
1. **IL Parser** - Tokenizes and validates text-based IL programs
2. **Compiler** - Converts parsed IL to executable Common Lisp code
3. **Interpreter** - Executes compiled programs with scan-based execution
4. **Data Tables** - Boolean, Word, Float, String memory spaces
5. **Standard Libraries** - Math, timers, counters, data manipulation

## Technology Stack
- **Language**: Common Lisp (SBCL recommended)
- **Build System**: ASDF
- **Dependencies**:
  - `cl-ppcre` - Regular expressions for parsing
  - `alexandria` - Common utilities
  - `parse-number` - Number parsing
  - `local-time` - Timestamp handling (optional)
  - `fiveam` - Testing framework

## Project Structure
```
mblogic-cl/
├── warp.md                    # This file
├── mblogic-cl.asd            # ASDF system definition
├── README.md                  # User documentation
├── src/
│   ├── package.lisp          # Package definitions
│   ├── data-table.lisp       # Phase 1: Data structures
│   ├── instructions.lisp     # Phase 2: Instruction definitions
│   ├── parser.lisp           # Phase 3: IL parser
│   ├── compiler.lisp         # Phase 4: Code generator
│   ├── interpreter.lisp      # Phase 5: Runtime engine
│   ├── math-lib.lisp         # Phase 6: Math operations
│   ├── timer-counter.lisp    # Phase 6: Timers and counters
│   └── table-ops.lisp        # Phase 6: Data table operations
└── test/
    ├── test-suite.lisp       # Main test suite
    ├── test-data-table.lisp
    ├── test-parser.lisp
    ├── test-compiler.lisp
    ├── test-interpreter.lisp
    └── plcprog.txt           # Sample IL program from MBLogic
```

## Implementation Plan

### Phase 1: Core Data Structures (Week 1-2)
**Goal**: Create memory spaces for PLC data (Boolean, Word, Float, String)

**Files**: `src/data-table.lisp`

**Key Components**:
- Hash tables for address spaces:
  - Boolean: X (inputs), Y (outputs), C (control relays), SC (system), T (timers), CT (counters)
  - Word: DS, DD, DH (data registers), XD/YD/XS/YS (I/O registers), TD/CTD (timer/counter data)
  - Float: DF (floating point registers)
  - String: TXT (text registers)
- Address validation and indexing
- Initialization functions

**CLOS Classes**:
```lisp
(defclass data-table ()
  ((bool-table :initform (make-hash-table :test 'equal))
   (word-table :initform (make-hash-table :test 'equal))
   (float-table :initform (make-hash-table :test 'equal))
   (string-table :initform (make-hash-table :test 'equal))
   (instr-table :initform (make-hash-table :test 'equal))))
```

**Address Ranges** (from DLCkDataTable.py):
- X1-X2000 (inputs)
- Y1-Y2000 (outputs)
- C1-C2000 (control relays)
- SC1-SC1000 (system control)
- T1-T500 (timers)
- CT1-CT250 (counters)
- DS1-DS10000 (signed integer data)
- DD1-DD2000 (double integer data)
- DH1-DH2000 (hex/unsigned integer data)
- DF1-DF2000 (floating point data)
- TXT1-TXT10000 (text/string data)

**Testing**: Verify initialization, address lookup, data read/write

### Phase 2: Instruction Definitions (Week 2-3)
**Goal**: Define the complete IL instruction set using CLOS

**Files**: `src/instructions.lisp`

**CLOS Class Structure**:
```lisp
(defclass plc-instruction ()
  ((opcode :initarg :opcode :accessor instruction-opcode)
   (description :initarg :description :accessor instruction-description)
   (function :initarg :function :accessor instruction-function)
   (type :initarg :type :accessor instruction-type)
   (class :initarg :class :accessor instruction-class)
   (params :initarg :params :accessor instruction-params)
   (validator :initarg :validator :accessor instruction-validator)
   (ladsymb :initarg :ladsymb :accessor instruction-ladsymb)
   (monitor :initarg :monitor :accessor instruction-monitor)))

;; Generic function for typed data handling
(defgeneric validate-parameter (instruction param-index value))
(defgeneric execute-instruction (instruction params logic-stack data-table))
```

**Instruction Categories**:
1. **Boolean Logic**: STR, STRN, AND, ANDN, OR, ORN, ANDSTR, ORSTR
2. **Outputs**: OUT, SET, RST, PD
3. **Comparisons**: STRE, STRGT, STRLT, STRGE, STRLE, STRNE (and AND/OR variants)
4. **Edges**: STRPD, STRND, ANDPD, ANDND, ORPD, ORND
5. **Timers**: TMR, TMRA, TMROFF
6. **Counters**: CNTU, CNTD, UDC
7. **Data Movement**: COPY, CPYBLK, FILL, PACK, UNPACK
8. **Math**: MATHDEC, MATHHEX
9. **Search**: FINDEQ, FINDGT, FINDLT, etc.
10. **Control**: CALL, RT, RTC, END, ENDC, FOR, NEXT
11. **Special**: NETWORK, SBR (subroutine), SHFRG (shift register)

**Total**: ~100 instruction definitions to port from DLCkInstructions.py

**Testing**: Verify instruction creation, validation logic, parameter counts

### Phase 3: IL Parser (Week 3-4)
**Goal**: Parse text IL programs into structured instruction lists

**Files**: `src/parser.lisp`

**Key Components**:
- Tokenizer using `cl-ppcre` for regex pattern matching
- Network/subroutine structure recognition
- Comment handling
- Parameter extraction and validation
- Error reporting with line numbers
- Symbol table for subroutines

**Parser Structure**:
```lisp
(defclass il-parser ()
  ((source-lines :initarg :source)
   (instruction-set :initarg :instructions)
   (networks :initform '())
   (subroutines :initform (make-hash-table :test 'equal))
   (errors :initform '())))

(defmethod parse-program ((parser il-parser))
  ;; Main parsing loop
  ;; Returns: list of networks and subroutines
  )

(defmethod parse-line ((parser il-parser) line-text line-number)
  ;; Parse single line
  ;; Returns: instruction object or nil
  )
```

**Parsing Rules** (from PLCCompile.py):
- Comments: Lines starting with `//`
- Networks: `NETWORK <number>`
- Subroutines: `SBR <name>`
- Instructions: `OPCODE [param1] [param2] ...`
- Parameter types: Boolean addresses, word addresses, constants, expressions

**Testing**: Parse `plcprog.txt`, verify network structure, check error detection

### Phase 4: Compiler (Week 4-6)
**Goal**: Generate executable Lisp code from parsed IL (Option A: Form generation and compilation)

**Files**: `src/compiler.lisp`

**Compilation Strategy**:
The Python version generates Python source code strings and compiles to bytecode. Our approach:
1. Generate Lisp forms (S-expressions) from parsed IL
2. Use Common Lisp's `compile` to create native machine code
3. Store compiled functions for execution

**Compiler Structure**:
```lisp
(defclass il-compiler ()
  ((instruction-set :initarg :instructions)
   (data-table :initarg :data-table)
   (compiled-networks :initform (make-hash-table))
   (subroutines :initform (make-hash-table))
   (errors :initform '())))

(defmethod compile-network ((compiler il-compiler) instructions)
  "Compile IL instruction list to Lisp lambda"
  `(lambda (data-table)
     (let ((logic-stack '())
           (stacktop nil)
           (rung-number 0))
       ,@(mapcar (lambda (instr) 
                   (compile-instruction compiler instr))
                 instructions))))

(defmethod compile-instruction ((compiler il-compiler) instruction)
  "Generate Lisp form for single instruction"
  ;; Pattern match on instruction class and generate appropriate code
  )
```

**Code Generation Examples**:
```lisp
;; STR X1 -> Store input X1 on logic stack
(push (gethash "X1" (bool-table data-table)) logic-stack)
(setf stacktop (first logic-stack))

;; AND C10 -> AND contact C10 with top of stack
(setf stacktop (and stacktop (gethash "C10" (bool-table data-table))))
(setf (first logic-stack) stacktop)

;; OUT Y1 -> Output top of stack to Y1
(setf (gethash "Y1" (bool-table data-table)) stacktop)
```

**Compilation Process**:
1. Parse IL to instruction objects (Phase 3)
2. Generate Lisp forms for each instruction
3. Wrap in lambda with local variables
4. Call `compile` to create native function
5. Store in compiler's function table

**Testing**: Compile simple networks, verify generated code, test execution

### Phase 5: Runtime Interpreter (Week 6-7)
**Goal**: Execute compiled programs with scan-based real-time behavior

**Files**: `src/interpreter.lisp`

**Interpreter Structure**:
```lisp
(defclass plc-interpreter ()
  ((program :initarg :program :accessor interpreter-program)
   (data-table :initarg :data-table :accessor interpreter-data-table)
   (main-network :accessor interpreter-main)
   (subroutines :initform (make-hash-table) :accessor interpreter-subroutines)
   (call-stack :initform '() :accessor interpreter-call-stack)
   (scan-time :initform 0 :accessor interpreter-scan-time)
   (running :initform nil :accessor interpreter-running)
   (exit-code :initform nil :accessor interpreter-exit-code)))

(defmethod run-scan ((interp plc-interpreter))
  "Execute one complete PLC scan cycle"
  (let ((start-time (get-internal-real-time)))
    (handler-case
        (funcall (interpreter-main interp) 
                 (interpreter-data-table interp))
      (plc-end-condition (c) 
        (setf (interpreter-exit-code interp) :normal-end))
      (error (e) 
        (handle-runtime-error interp e)))
    (setf (interpreter-scan-time interp)
          (/ (- (get-internal-real-time) start-time)
             internal-time-units-per-second))))

(defmethod run-continuous ((interp plc-interpreter) &optional (max-scans nil))
  "Run PLC in continuous scan mode"
  (setf (interpreter-running interp) t)
  (loop for scan-count from 0
        while (interpreter-running interp)
        do (run-scan interp)
           (when (and max-scans (>= scan-count max-scans))
             (return))))
```

**Condition System**:
```lisp
(define-condition plc-end-condition (condition)
  ((type :initarg :type :reader end-type)))  ; :normal, :conditional

(define-condition plc-runtime-error (error)
  ((message :initarg :message)
   (subroutine :initarg :subroutine)
   (network :initarg :network)))
```

**Call Stack Tracking**:
```lisp
(defclass call-frame ()
  ((subroutine-name :initarg :name)
   (network-number :initarg :network)
   (timestamp :initform (get-internal-real-time))))

(defmethod push-call ((interp plc-interpreter) subname network)
  (push (make-instance 'call-frame 
                       :name subname 
                       :network network)
        (interpreter-call-stack interp)))
```

**Testing**: Execute compiled programs, verify scan timing, test error handling

### Phase 6: Standard Libraries (Week 7-9)
**Goal**: Implement PLC standard function libraries

**Files**: `src/math-lib.lisp`, `src/timer-counter.lisp`, `src/table-ops.lisp`

#### Math Library (`math-lib.lisp`)
From `DLCkMath.py`:
- MATHDEC: Decimal math expressions (e.g., `(1 + DS10) * 2`)
- MATHHEX: Hexadecimal math with bitwise operations
- BCD math operations
- Trigonometric functions: SIN, COS, TAN, ASIN, ACOS, ATAN
- Other: SQRT, ABS, EXP, LN, LOG, POW
- Bitwise: AND, OR, XOR, NOT, LSH (left shift), RSH (right shift)

```lisp
(defun mathdec (result-addr flags expression data-table)
  "Execute decimal math expression"
  ;; Parse and evaluate mathematical expression
  ;; Store result in result-addr
  )

(defun mathhex (result-addr flags expression data-table)
  "Execute hexadecimal math with bitwise operations"
  ;; Similar to mathdec but for hex/bitwise
  )
```

#### Timer/Counter Library (`timer-counter.lisp`)
From `DLCkCounterTimer.py`:

**Timer Types**:
1. TMR - On-delay timer
2. TMRA - Accumulating on-delay timer
3. TMROFF - Off-delay timer

**Counter Types**:
1. CNTU - Up counter
2. CNTD - Down counter
3. UDC - Up/down counter

```lisp
(defclass plc-timer ()
  ((address :initarg :address)
   (status-bit :initform nil)
   (preset :initform 0)
   (accumulated :initform 0)
   (last-enable :initform nil)
   (last-reset :initform nil)
   (last-update-time :initform 0)))

(defmethod tmr-execute ((timer plc-timer) enable reset preset scan-time)
  "On-delay timer execution"
  (cond
    (reset 
     (setf (slot-value timer 'accumulated) 0
           (slot-value timer 'status-bit) nil))
    (enable
     (incf (slot-value timer 'accumulated) scan-time)
     (when (>= (slot-value timer 'accumulated) preset)
       (setf (slot-value timer 'status-bit) t)))
    (t
     (setf (slot-value timer 'accumulated) 0
           (slot-value timer 'status-bit) nil))))
```

**Counter Implementation**:
```lisp
(defclass plc-counter ()
  ((address :initarg :address)
   (status-bit :initform nil)
   (preset :initform 0)
   (accumulated :initform 0)
   (last-count :initform nil)
   (last-reset :initform nil)))

(defmethod cntu-execute ((counter plc-counter) count reset preset)
  "Up counter execution"
  ;; Count on rising edge
  ;; Set status when accumulated >= preset
  )
```

#### Table Operations (`table-ops.lisp`)
From `DLCkTableInstr.py`:

- COPY: Copy single value
- CPYBLK: Copy block of values
- FILL: Fill range with value
- PACK: Pack bits into word
- UNPACK: Unpack word into bits
- FINDEQ, FINDNE, FINDGT, FINDLT, FINDGE, FINDLE: Search operations
- FINDIEQ, etc.: Incremental search operations
- SHFRG: Shift register
- SUM: Sum array of values

```lisp
(defun copy-value (source dest data-table)
  "Copy single value from source to destination"
  (setf (gethash dest (word-table data-table))
        (gethash source (word-table data-table))))

(defun copy-block (source-start source-end dest-start data-table)
  "Copy block of values"
  ;; Copy range of addresses
  )

(defun find-equal (search-value table-start table-end result-addr found-bit data-table)
  "Search for value in table"
  ;; Linear search through address range
  )
```

**Testing**: 
- Math: Test expressions, operator precedence, functions
- Timers: Verify timing accuracy, edge detection
- Counters: Test counting, presets, resets
- Tables: Test all data manipulation operations

### Phase 7: Testing & Validation (Week 9-10)
**Goal**: Comprehensive testing with real PLC program

**Files**: `test/test-suite.lisp`, individual test files

**Test Strategy**:
1. Unit tests for each phase
2. Integration tests with `plcprog.txt`
3. Performance benchmarks
4. Edge case testing

```lisp
(in-package :mblogic-cl-test)

(def-suite data-table-suite :description "Data table tests")
(def-suite parser-suite :description "Parser tests")
(def-suite compiler-suite :description "Compiler tests")
(def-suite interpreter-suite :description "Interpreter tests")
(def-suite integration-suite :description "Full system tests")

(in-suite integration-suite)

(test compile-and-run-sample-program
  "Load, compile, and execute plcprog.txt"
  (let* ((source (read-il-file "test/plcprog.txt"))
         (parser (make-instance 'il-parser :source source))
         (parsed (parse-program parser))
         (compiler (make-instance 'il-compiler))
         (compiled (compile-program compiler parsed))
         (interp (make-instance 'plc-interpreter :program compiled)))
    (is (null (parser-errors parser)))
    (is (null (compiler-errors compiler)))
    ;; Run 10 scan cycles
    (run-continuous interp 10)
    ;; Verify outputs
    (is (= (get-bool interp "Y1") (get-bool interp "X1")))
    ;; Test tank simulation
    (test-tank-sim interp)))
```

**Test Coverage Goals**:
- All instruction types
- All data types (bool, word, float, string)
- Error conditions
- Edge cases (overflow, division by zero, etc.)
- Call stack depth
- Network branching

**Validation Against Python**:
- Compare outputs for same inputs
- Verify timing behavior
- Check edge conditions

## Key Design Decisions

### Data Structure Choices
- **Hash tables** for address lookup (flexibility over raw performance)
- **CLOS classes** for typed data and instruction definitions
- **Symbols vs strings** for addresses: Use strings for consistency with IL syntax

### Compilation Strategy
- **Option A selected**: Generate Lisp forms and compile to native code
- Advantages: Native performance, debuggable, leverages CL compiler optimizations
- Alternative considered: Direct interpretation (simpler but slower)

### Error Handling
- Use CL condition system for PLC-specific conditions
- Separate compilation errors from runtime errors
- Maintain call stack for debugging

### Performance Considerations
- Declare types where performance-critical
- Use `(optimize (speed 3) (safety 1))` for production
- Pre-compile all networks before execution
- Cache address lookups where possible

## Development Workflow

### Initial Setup
```bash
cd ~/common-lisp/mblogic-cl
# Load in REPL
(ql:quickload :mblogic-cl)
```

### Development Cycle
1. Implement phase components
2. Write unit tests
3. Run tests: `(asdf:test-system :mblogic-cl)`
4. Iterate

### Testing Individual Components
```lisp
;; In REPL
(in-package :mblogic-cl)

;; Test data table
(defparameter *dt* (make-instance 'data-table))
(init-data-table *dt*)
(set-bool *dt* "X1" t)
(get-bool *dt* "X1")  ; => T

;; Test parser
(defparameter *parser* (make-instance 'il-parser))
(parse-line *parser* "STR X1" 1)

;; Test compiler
(defparameter *compiler* (make-instance 'il-compiler))
(compile-network *compiler* <instructions>)
```

## Reference Materials

### Python Source Files (Priority Order)
1. `DLCkDataTable.py` - Data structure reference
2. `DLCkInstructions.py` - Complete instruction set
3. `PLCCompile.py` - Parser/compiler logic
4. `PLCInterp.py` - Runtime behavior
5. `DLCkMath.py` - Math library
6. `DLCkCounterTimer.py` - Timer/counter implementations
7. `DLCkTableInstr.py` - Table operations

### Sample IL Program
`test/plcprog.txt` - Working PLC program demonstrating:
- Boolean logic
- Timers and counters
- Math operations
- Subroutine calls
- Data manipulation

## Implementation Status

### Completed Phases
- ✅ **Phase 1**: Core Data Structures
- ✅ **Phase 2**: Instruction Definitions
- ✅ **Phase 3**: IL Parser
- ✅ **Phase 4**: Compiler
- ✅ **Phase 5**: Runtime Interpreter
- ✅ **Phase 6**: Standard Libraries
- ✅ **Phase 7**: Testing & Validation

### Phase 7 Completion Summary
- Implemented FOR/NEXT loop support in compiler with proper network-level handling
- Fixed critical performance issue: precompiled regex patterns (1800x speedup)
- Optimized timer/counter key generation
- Enabled all previously skipped tests for plcprog.txt
- All tests pass: 240/241 (1 skip for TIMER-BASIC which requires real-time)
- Performance: ~0.05ms per scan for plcprog.txt (target was <1ms)

## Success Criteria
- ✅ Parse `plcprog.txt` without errors
- ✅ Compile all networks to executable code (including FOR/NEXT loops)
- ✅ Execute scan cycles with correct outputs
- ✅ Pass all unit and integration tests (240/241 pass, 1 expected skip)
- ✅ Performance: <1ms scan time for sample program (achieved: 0.05ms)
- ✅ Code quality: Clean, documented, idiomatic Common Lisp

## Phase 7+: Ladder Diagram Visualization (In Progress)
**Goal**: Web-based ladder diagram visualization and live monitoring

**Status**: ✅ Started

**Files Implemented**:
- `src/web/package.lisp` - Package definition for mblogic-cl-web
- `src/web/ladder-render.lisp` - IL to ladder cell matrix conversion
- `src/web/json-api.lisp` - JSON API response generation
- `src/web/server.lisp` - Hunchentoot web server with REST API
- `static/laddermonitor.html` - Main HTML page for ladder monitor
- `static/css/ladder.css` - Ladder diagram styling

**Additional Dependencies** (mblogic-cl/web system):
- `hunchentoot` - HTTP server
- `cl-json` - JSON serialization
- `bordeaux-threads` - Multi-threading for PLC execution

**Components Completed**:
1. ✅ **Ladder Symbol Mapping** - Mapping from IL instruction :ladsymb to SVG symbols
2. ✅ **Instruction Classification** - Functions to categorize instructions (contact, coil, block, etc.)
3. ✅ **Address Extraction** - Extract monitorable addresses from instructions
4. ✅ **Cell Conversion** - Convert parsed instructions to ladder cells
5. ✅ **Network to Rung** - Convert parsed networks to ladder rung structures
6. ✅ **Program Conversion** - Convert parsed programs to ladder diagram format
7. ✅ **JSON Serialization** - Convert ladder structures to JSON for web API
8. ✅ **Web Server** - Hunchentoot server with API endpoints
9. ✅ **Static Files** - HTML, CSS for ladder monitor interface

**API Endpoints**:
- `GET /api/statistics` - PLC runtime statistics
- `GET /api/data?addr=X1,Y1,...` - Read address values
- `GET /api/program?subrname=main` - Get ladder diagram structure
- `GET /api/subroutines` - List available subroutines
- `POST /api/control/start` - Start continuous PLC execution
- `POST /api/control/stop` - Stop PLC execution
- `POST /api/control/step` - Single scan step

**Testing**:
- `test/test-ld-visualization.lisp` - Ladder visualization unit tests

**Usage**:
```lisp
;; Quick start with IL program
(ql:quickload :mblogic-cl/web)
(mblogic-cl-web:quick-start "test/plcprog.txt" :port 8080)
;; Open http://localhost:8080/laddermonitor.html
```

**JavaScript Files** (static/js/):
- `servercomm.js` - API communication module with fetch wrappers
- `ladsymbols.js` - SVG symbol definitions for contacts, coils, blocks
- `ladsubrdisplib.js` - Ladder rung rendering and cell state updates
- `ladmonitor.js` - Main monitor with polling, button handlers, stats

**Components Remaining**:
- [ ] Branch visualization (parallel contacts with vertical lines)
- [ ] Enhanced block instruction rendering (parameter display in blocks)
- [ ] Real-time value overlay on cells

## Future Enhancements (Post Phase 7+)
- Live debugging/monitoring completion
- Persistent data table (save/restore state)
- Network editor
- Integration with I/O hardware
- Web-based HMI interface
- Multi-threaded execution for large programs

## Notes
- Python source uses tabs for indentation
- Original system targeted Python 2.5-2.7
- GPL v3 licensed - derivative must maintain license
- Co-author attribution: `Co-Authored-By: Warp <agent@warp.dev>`
