# How to Generate Ladder JSON Output

This guide shows you how to generate JSON output for ladder diagrams from PLC programs.

## Quick Start

### Option 1: Use the Script (Easiest)

Simply run the included script:

```powershell
sbcl --load generate-ladder-json.lisp --quit
```

This will:
- Load the mblogic-cl system
- Parse `test/plcprog.txt`
- Generate JSON for the **LadderDemo** subroutine
- Save it to `LadderDemo-output.json`
- Display the JSON on screen

### Option 2: Interactive REPL

For more control, use the REPL:

```powershell
sbcl
```

Then in the SBCL REPL:

```lisp
;; Load the systems
(ql:quickload :mblogic-cl)
(ql:quickload :mblogic-cl/web)

;; Parse your PLC program
(defparameter *source* 
  (mblogic-cl:parse-il-file "test/plcprog.txt"))

;; Generate ladder structure for any subroutine
(defparameter *ladder* 
  (mblogic-cl-web::program-to-ladder *source* "LadderDemo"))
  
;; Convert to plist
(defparameter *plist* 
  (mblogic-cl-web::ladder-program-to-plist *ladder*))

;; Convert to JSON
(defparameter *json* 
  (mblogic-cl-web::plist-to-json *plist*))

;; View the JSON
(format t "~A~%" *json*)

;; Save to a file
(with-open-file (out "my-output.json"
                     :direction :output
                     :if-exists :supersede)
  (write-string *json* out))
```

## Available Subroutines

To see all available subroutines in the program:

```lisp
(mblogic-cl-web::list-subroutine-names *source*)
```

The `plcprog.txt` file contains:
- **main** - Main program logic
- **LadderDemo** - Demonstrates various ladder instructions
- **TankSim** - Tank simulation
- **StripChart** - Strip chart demo
- **ExtData** - Extended data types demo
- **PickAndPlace** - Pick and place control
- **Events** - Event handling
- **Alarms** - Alarm checking

## JSON Output Format

The generated JSON contains:

```json
{
  "subrname": "LadderDemo",
  "addresses": ["C1", "T1", "CT5", "C100", ...],
  "subrdata": [
    {
      "rungnum": 1,
      "rows": 2,
      "cols": 5,
      "comment": "Demonstrate different ladder instructions...",
      "addrs": ["C1", "T1", "CT5", "C100"],
      "cells": [
        {
          "type": "contact",
          "symbol": "noc",
          "addr": "C1",
          "opcode": "STR",
          "params": ["C1"],
          "row": 0,
          "col": 0,
          "monitor": "bool"
        },
        ...
      ]
    },
    ...
  ]
}
```

### Cell Types

- **contact** - Input contact (normally open/closed)
- **coil** - Output coil (OUT, SET, RST, PD)
- **block** - Functional blocks (timers, counters, math, compare, etc.)
- **branch** - Branch connectors for parallel logic paths
- **control** - Control flow instructions (RT, NEXT, etc.)

### Symbol Names

Common symbols include:
- `noc` - Normally open contact
- `ncc` - Normally closed contact
- `nocpd` - Positive edge (rising edge) contact
- `nocnd` - Negative edge (falling edge) contact
- `out` - Output coil
- `set` - Set/latch coil
- `rst` - Reset/unlatch coil
- `pd` - Pulse coil
- `tmr`, `tmra`, `tmroff` - Timer blocks
- `cntu`, `cntd`, `udc` - Counter blocks
- `compare` - Comparison block
- `copy`, `cpyblk`, `fill` - Data operation blocks

## Example: Generate JSON for a Different Subroutine

```lisp
;; Generate JSON for the TankSim subroutine
(defparameter *tank-ladder* 
  (mblogic-cl-web::program-to-ladder *source* "TankSim"))
  
(defparameter *tank-json* 
  (mblogic-cl-web::plist-to-json 
    (mblogic-cl-web::ladder-program-to-plist *tank-ladder*)))

;; Save it
(with-open-file (out "TankSim.json"
                     :direction :output
                     :if-exists :supersede)
  (write-string *tank-json* out))
```

## Files Generated

- `LadderDemo-output.json` - Full JSON output for LadderDemo subroutine
- `LadderDemo.json` - Pre-existing reference (may be different format)
- `generate-ladder-json.lisp` - The generator script

## Web Visualization

The JSON output is compatible with web-based ladder diagram visualization. 
The `mblogic-cl/web` system includes a web server that can:
- Display ladder diagrams in a browser
- Monitor PLC values in real-time
- Interact with the running interpreter

See `src/web/` for more details on the web visualization system.

## Troubleshooting

### "Package not found" error
Make sure Quicklisp is loaded and the system is in the right location:
```lisp
(ql:quickload :mblogic-cl)
```

### "File not found" when parsing
Ensure you're in the project root directory:
```powershell
cd D:\common-lisp\mblogic-cl
```

### Empty or incorrect JSON
Verify the subroutine name is correct:
```lisp
(mblogic-cl-web::list-subroutine-names *source*)
```

## Next Steps

- Check out the existing JSON in `LadderDemo.json` for reference
- Explore the web visualization in `static/laddermonitor.html`
- Read `src/web/ladder-render.lisp` for implementation details
- Try parsing your own PLC programs!
