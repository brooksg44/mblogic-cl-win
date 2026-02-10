;;;; src/web/ladder-render.lisp
;;;;
;;;; IL to Ladder Diagram Conversion
;;;; Converts parsed IL networks to ladder cell matrix format for visualization

(in-package #:mblogic-cl-web)

;;; ============================================================
;;; Ladder Symbol Mapping
;;; ============================================================

(defparameter *ladsymb-to-svg*
  '((:contact-no . "noc")       ; Normally open contact
    (:contact-nc . "ncc")       ; Normally closed contact
    (:contact-pd . "nocpd")     ; Positive differential (rising edge) contact
    (:contact-nd . "nocnd")     ; Negative differential (falling edge) contact
    (:coil . "out")             ; Standard output coil
    (:coil-set . "set")         ; Set (latch) coil
    (:coil-reset . "rst")       ; Reset (unlatch) coil
    (:coil-pd . "pd")           ; Pulse coil
    (:branch-end . nil)         ; Branch end - no symbol, handled structurally
    (:compare . "compare")      ; Comparison block
    (:timer . "tmr")            ; Timer block
    (:counter . "cntu")         ; Counter block
    (:math . "mathdec")         ; Math block
    (:copy . "copy")            ; Copy instruction
    (:cpyblk . "cpyblk")        ; Block copy
    (:fill . "fill")            ; Fill instruction
    (:pack . "pack")            ; Pack bits
    (:unpack . "unpack")        ; Unpack bits
    (:shfrg . "shfrg")          ; Shift register
    (:find . "findeq")          ; Search instruction
    (:sum . "sum")              ; Sum instruction
    (:call . "call")            ; Subroutine call
    (:return . "rt")            ; Return
    (:return-cond . "rtc")      ; Conditional return
    (:end . "end")              ; End
    (:end-cond . "endc")        ; Conditional end
    (:for . "for")              ; For loop start
    (:next . "next"))           ; For loop end
  "Mapping from instruction :ladsymb to SVG symbol names")

(defun ladsymb-to-svg-symbol (ladsymb opcode)
  "Convert instruction ladsymb keyword to SVG symbol name.
   Uses opcode for more specific mapping when needed."
  (let ((svg-sym (cdr (assoc ladsymb *ladsymb-to-svg*))))
    ;; Handle special cases where opcode matters
    (cond
      ;; Comparisons - map to specific JS symbols
      ((eq ladsymb :compare)
       (cond
         ((member opcode '("STRE" "ANDE" "ORE") :test #'string-equal) "compeq")
         ((member opcode '("STRNE" "ANDNE" "ORNE") :test #'string-equal) "compneq")
         ((member opcode '("STRGT" "ANDGT" "ORGT") :test #'string-equal) "compgt")
         ((member opcode '("STRLT" "ANDLT" "ORLT") :test #'string-equal) "complt")
         ((member opcode '("STRGE" "ANDGE" "ORGE") :test #'string-equal) "compge")
         ((member opcode '("STRLE" "ANDLE" "ORLE") :test #'string-equal) "comple")
         (t "compeq")))  ; Default comparison
      ;; Timers
      ((and (eq ladsymb :timer) (string-equal opcode "TMR")) "tmr")
      ((and (eq ladsymb :timer) (string-equal opcode "TMRA")) "tmra")
      ((and (eq ladsymb :timer) (string-equal opcode "TMROFF")) "tmroff")
      ;; Counters
      ((and (eq ladsymb :counter) (string-equal opcode "CNTU")) "cntu")
      ((and (eq ladsymb :counter) (string-equal opcode "CNTD")) "cntd")
      ((and (eq ladsymb :counter) (string-equal opcode "UDC")) "udc")
      ;; Find instructions
      ((eq ladsymb :find)
       (cond
         ((member opcode '("FINDEQ" "FINDIEQ") :test #'string-equal) "findeq")
         ((member opcode '("FINDNE" "FINDINE") :test #'string-equal) "findne")
         ((member opcode '("FINDGT" "FINDIGT") :test #'string-equal) "findgt")
         ((member opcode '("FINDLT" "FINDILT") :test #'string-equal) "findlt")
         ((member opcode '("FINDGE" "FINDIGE") :test #'string-equal) "findge")
         ((member opcode '("FINDLE" "FINDILE") :test #'string-equal) "findle")
         (t "findeq")))
      ;; Default
      (t (or svg-sym "il")))))  ; "il" = raw IL display fallback

;;; ============================================================
;;; Ladder Cell Structure
;;; ============================================================

(defstruct ladder-cell
  "A single cell in the ladder diagram matrix"
  (type nil)          ; :contact, :coil, :block, :hline, :vline, :branch, :empty
  (symbol nil)        ; SVG symbol name
  (address nil)       ; PLC address (string) or nil
  (addresses nil)     ; List of addresses for multi-address instructions
  (value nil)         ; Display value or preset
  (opcode nil)        ; Original opcode for reference
  (params nil)        ; Original parameters
  (row 0)             ; Row position in matrix
  (col 0)             ; Column position in matrix
  (monitor-type nil)) ; :bool, :word, :timer, :counter for live monitoring

(defstruct ladder-rung
  "A complete ladder rung (network)"
  (number 0)            ; Network number
  (cells nil)           ; 2D array or list of ladder-cell
  (rows 1)              ; Number of rows (for branches)
  (cols 0)              ; Number of columns
  (addresses nil)       ; All addresses for monitoring
  (comment nil)         ; Associated comment
  (il-fallback nil)     ; If t, display raw IL instead
  (branches nil)        ; List of (row start-col merge-col) for input branch rendering
  (output-branches nil)); List of (col rows...) for parallel output coils

(defstruct ladder-program
  "Complete ladder program structure"
  (name "main")         ; Subroutine name
  (rungs nil)           ; List of ladder-rung
  (addresses nil))      ; All unique addresses

;;; ============================================================
;;; Branch Connector Types (Python-compatible)
;;; ============================================================
;;; These match the Python MBLogic matrixdata format exactly.
;;; Branch connectors are explicit cells in the matrix, not computed at render time.

;; Branch connector symbol names
(defparameter *branch-ttr* "branchttr"   "Top-right corner: ┐ - top of branch fork")
(defparameter *branch-tr*  "branchtr"    "Middle-right T: ┤ - middle rows of branch fork")
(defparameter *branch-r*   "branchr"     "Bottom-right corner: ┘ - bottom of branch fork")
(defparameter *vbar-r*     "vbarr"       "Vertical bar right: │ - no junction (right side)")
(defparameter *branch-ttl* "branchttl"   "Top-left corner: ┌ - top of merge")
(defparameter *branch-tl*  "branchtl"    "Middle-left T: ├ - middle rows of merge")
(defparameter *branch-l*   "branchl"     "Bottom-left corner: └ - bottom of merge")
(defparameter *vbar-l*     "vbarl"       "Vertical bar left: │ - no junction (left side)")
(defparameter *branch-tu*  "branchtu"    "Bottom T: ┴ - connects left, right, up")
(defparameter *branch-x*   "branchx"     "Cross: ┼ - connects all four directions")
(defparameter *hbar*       "hbar"        "Horizontal bar: ─ - wire segment")

;; List of all branch/connector symbols for identification
(defparameter *branch-symbols*
  (list *branch-ttr* *branch-tr* *branch-r* *vbar-r*
        *branch-ttl* *branch-tl* *branch-l* *vbar-l* *branch-tu* *branch-x* *hbar*)
  "All branch connector symbol names")

;; Vertical branch symbols (excludes horizontal)
(defparameter *vertical-branch-symbols*
  (list *branch-ttr* *branch-tr* *branch-r* *vbar-r*
        *branch-ttl* *branch-tl* *branch-l* *vbar-l* *branch-tu* *branch-x*)
  "Vertical branch connector symbols")

(defun branch-symbol-p (symbol)
  "Check if SYMBOL is a branch connector symbol"
  (member symbol *branch-symbols* :test #'string-equal))

(defun vertical-branch-symbol-p (symbol)
  "Check if SYMBOL is a vertical branch connector symbol"
  (member symbol *vertical-branch-symbols* :test #'string-equal))

;;; ============================================================
;;; Branch Cell Constructors
;;; ============================================================

(defun make-branch-cell (symbol &key (row 0) (col 0))
  "Create a branch connector cell with the given SYMBOL at ROW, COL.
   Branch cells have no address and monitor type 'none'."
  (make-ladder-cell
   :type :branch
   :symbol symbol
   :address nil
   :addresses nil
   :row row
   :col col
   :monitor-type nil))

(defun make-hbar-cell (&key (row 0) (col 0))
  "Create a horizontal bar cell"
  (make-branch-cell *hbar* :row row :col col))

(defun make-vbar-l-cell (&key (row 0) (col 0))
  "Create a vertical bar (left side) cell"
  (make-branch-cell *vbar-l* :row row :col col))

(defun make-branch-l-cell (&key (row 0) (col 0))
  "Create a bottom-left corner cell: └"
  (make-branch-cell *branch-l* :row row :col col))

(defun make-branch-tl-cell (&key (row 0) (col 0))
  "Create a middle-left T cell: ├"
  (make-branch-cell *branch-tl* :row row :col col))

(defun make-branch-ttl-cell (&key (row 0) (col 0))
  "Create a top-left corner cell: ┌"
  (make-branch-cell *branch-ttl* :row row :col col))

(defun make-branch-tr-cell (&key (row 0) (col 0))
  "Create a middle-right T cell: ┤"
  (make-branch-cell *branch-tr* :row row :col col))

(defun make-branch-ttr-cell (&key (row 0) (col 0))
  "Create a top-right corner cell: ┐"
  (make-branch-cell *branch-ttr* :row row :col col))

(defun make-branch-r-cell (&key (row 0) (col 0))
  "Create a bottom-right corner cell: ┘"
  (make-branch-cell *branch-r* :row row :col col))

(defun make-branch-tu-cell (&key (row 0) (col 0))
  "Create a bottom T cell: ┴ (connects left, right, up)"
  (make-branch-cell *branch-tu* :row row :col col))

(defun make-branch-x-cell (&key (row 0) (col 0))
  "Create a cross cell: ┼ (connects all four directions)"
  (make-branch-cell *branch-x* :row row :col col))

;;; ============================================================
;;; Instruction Classification
;;; ============================================================

(defun contact-instruction-p (opcode)
  "Check if opcode is a contact (input) instruction"
  (member opcode '("STR" "STRN" "AND" "ANDN" "OR" "ORN"
                   "STRPD" "STRND" "ANDPD" "ANDND" "ORPD" "ORND")
          :test #'string-equal))

(defun store-instruction-p (opcode)
  "Check if opcode is a STR (store/start new logic block) instruction"
  (member opcode '("STR" "STRN" "STRPD" "STRND"
                   "STRE" "STRNE" "STRGT" "STRLT" "STRGE" "STRLE")
          :test #'string-equal))

(defun and-instruction-p (opcode)
  "Check if opcode is an AND instruction (continues current row)"
  (member opcode '("AND" "ANDN" "ANDPD" "ANDND"
                   "ANDE" "ANDNE" "ANDGT" "ANDLT" "ANDGE" "ANDLE")
          :test #'string-equal))

(defun or-instruction-p (opcode)
  "Check if opcode is an OR instruction (creates parallel branch)"
  (member opcode '("OR" "ORN" "ORPD" "ORND"
                   "ORE" "ORNE" "ORGT" "ORLT" "ORGE" "ORLE")
          :test #'string-equal))

(defun orstr-instruction-p (opcode)
  "Check if opcode is ORSTR (merge parallel blocks)"
  (string-equal opcode "ORSTR"))

(defun continuation-instruction-p (opcode)
  "Check if opcode is a continuation instruction (comparison/pulse that continues current row).
   These instructions should be appended to the continuation row after nested branches.
   Includes: STRE, STRNE, STRGT, STRLT, STRGE, STRLE, ORPD, ORND, ANDGT, ANDLT, ANDGE, ANDLE"
  (member opcode '("STRE" "STRNE" "STRGT" "STRLT" "STRGE" "STRLE"
                  "ORPD" "ORND"
                  "ANDGT" "ANDLT" "ANDGE" "ANDLE")
          :test #'string-equal))

(defun andstr-instruction-p (opcode)
  "Check if opcode is ANDSTR (merge series blocks)"
  (string-equal opcode "ANDSTR"))

(defun coil-instruction-p (opcode)
  "Check if opcode is a coil (output) instruction"
  (member opcode '("OUT" "SET" "RST" "PD") :test #'string-equal))

(defun branch-start-p (opcode)
  "Check if instruction starts a parallel branch"
  (member opcode '("OR" "ORN" "ORPD" "ORND"
                   "ORE" "ORNE" "ORGT" "ORLT" "ORGE" "ORLE")
          :test #'string-equal))

(defun branch-end-p (opcode)
  "Check if instruction ends branches"
  (member opcode '("ANDSTR" "ORSTR") :test #'string-equal))

(defun block-instruction-p (opcode)
  "Check if instruction renders as a block"
  (member opcode '("TMR" "TMRA" "TMROFF" "CNTU" "CNTD" "UDC"
                   "COPY" "CPYBLK" "FILL" "PACK" "UNPACK" "SHFRG"
                   "MATHDEC" "MATHHEX" "SUM"
                   "FINDEQ" "FINDNE" "FINDGT" "FINDLT" "FINDGE" "FINDLE"
                   "FINDIEQ" "FINDINE" "FINDIGT" "FINDILT" "FINDIGE" "FINDILE"
                   "STRE" "STRNE" "STRGT" "STRLT" "STRGE" "STRLE"
                   "ANDE" "ANDNE" "ANDGT" "ANDLT" "ANDGE" "ANDLE"
                   "ORE" "ORNE" "ORGT" "ORLT" "ORGE" "ORLE"
                   "CALL" "FOR")
          :test #'string-equal))

(defun control-instruction-p (opcode)
  "Check if instruction is a control flow instruction"
  (member opcode '("END" "ENDC" "RT" "RTC" "NEXT") :test #'string-equal))

(defun output-block-instruction-p (opcode)
  "Check if instruction is a block that belongs in the output column.
   These are NOT comparison instructions (which modify the logic stack like contacts)."
  (member opcode '("TMR" "TMRA" "TMROFF" "CNTU" "CNTD" "UDC"
                   "COPY" "CPYBLK" "FILL" "PACK" "UNPACK" "SHFRG"
                   "MATHDEC" "MATHHEX" "SUM"
                   "FINDEQ" "FINDNE" "FINDGT" "FINDLT" "FINDGE" "FINDLE"
                   "FINDIEQ" "FINDINE" "FINDIGT" "FINDILT" "FINDIGE" "FINDILE"
                   "CALL" "FOR")
          :test #'string-equal))

(defun rungtype-for-output (opcode)
  "Return the rungtype string for an output block instruction.
   Double rungs have 2 input rows, triple have 3."
  (cond
    ((member opcode '("CNTU" "CNTD" "TMRA") :test #'string-equal) "double")
    ((member opcode '("UDC" "SHFRG") :test #'string-equal) "triple")
    (t "single")))

;;; ============================================================
;;; Address Extraction
;;; ============================================================

(defun extract-addresses (instruction)
  "Extract all monitorable addresses from a parsed instruction"
  (let ((opcode (mblogic-cl:parsed-opcode instruction))
        (params (mblogic-cl:parsed-params instruction))
        (addresses nil))
    (cond
      ;; Contact instructions - single boolean address
      ((contact-instruction-p opcode)
       (when (and params (mblogic-cl:bool-addr-p (first params)))
         (push (first params) addresses)))

      ;; Coil instructions - multiple boolean addresses
      ((coil-instruction-p opcode)
       (dolist (p params)
         (when (mblogic-cl:bool-addr-p p)
           (push p addresses))))

      ;; Timer - timer address and preset
      ((member opcode '("TMR" "TMRA" "TMROFF") :test #'string-equal)
       (when params
         (let ((timer-addr (first params)))
           ;; Timer bit: T1, Timer data: TD1
           (when (cl-ppcre:scan "^T[0-9]+$" timer-addr)
             (push timer-addr addresses)
             (push (format nil "TD~A"
                          (subseq timer-addr 1))
                   addresses)))))

      ;; Counter - counter address
      ((member opcode '("CNTU" "CNTD" "UDC") :test #'string-equal)
       (when params
         (let ((ctr-addr (first params)))
           (when (cl-ppcre:scan "^CT[0-9]+$" ctr-addr)
             (push ctr-addr addresses)
             (push (format nil "CTD~A"
                          (subseq ctr-addr 2))
                   addresses)))))

      ;; Compare instructions - all params (addresses AND literals for display)
      ((member opcode '("STRE" "STRNE" "STRGT" "STRLT" "STRGE" "STRLE"
                        "ANDE" "ANDNE" "ANDGT" "ANDLT" "ANDGE" "ANDLE"
                        "ORE" "ORNE" "ORGT" "ORLT" "ORGE" "ORLE")
               :test #'string-equal)
       (dolist (p params)
         (push p addresses)))

      ;; COPY - source and dest
      ((string-equal opcode "COPY")
       (dolist (p params)
         (when (mblogic-cl:any-addr-p p)
           (push p addresses))))

      ;; Math - destination
      ((member opcode '("MATHDEC" "MATHHEX") :test #'string-equal)
       (when (and params (mblogic-cl:any-addr-p (first params)))
         (push (first params) addresses))))

    (nreverse addresses)))

(defun get-monitor-type (instruction)
  "Determine the monitor type for an instruction"
  (let ((instr-def (mblogic-cl:parsed-instruction-def instruction)))
    (when instr-def
      (mblogic-cl:instruction-monitor instr-def))))

;;; ============================================================
;;; Instruction to Cell Conversion
;;; ============================================================

(defun instruction-to-cell (instruction col)
  "Convert a parsed instruction to a ladder cell"
  (let* ((opcode (mblogic-cl:parsed-opcode instruction))
         (params (mblogic-cl:parsed-params instruction))
         (instr-def (mblogic-cl:parsed-instruction-def instruction))
         (ladsymb (when instr-def (mblogic-cl:instruction-ladsymb instr-def)))
         (svg-symbol (ladsymb-to-svg-symbol ladsymb opcode))
         (addresses (extract-addresses instruction)))

    (make-ladder-cell
     :type (cond
             ((contact-instruction-p opcode) :contact)
             ((coil-instruction-p opcode) :coil)
             ((block-instruction-p opcode) :block)
             ((control-instruction-p opcode) :control)
             ((branch-end-p opcode) :branch-end)
             (t :unknown))
     :symbol svg-symbol
     :address (first addresses)
     :addresses addresses
     :opcode opcode
     :params params
     :col col
     :monitor-type (get-monitor-type instruction))))

;;; ============================================================
;;; Matrix Operations (Python-compatible algorithm)
;;; ============================================================
;;; The ladder matrix is stored as a list of rows: ((row0-cells) (row1-cells) ...)
;;; Each row is a list of cells (or nil for empty positions).
;;; This matches the Python PLCLadder.py implementation.

(defun matrix-width (matrix)
  "Return the width (max column count) of the matrix"
  (if (null matrix)
      0
      (reduce #'max matrix :key #'length :initial-value 0)))

(defun matrix-height (matrix)
  "Return the height (row count) of the matrix"
  (length matrix))

(defun copy-cell (cell)
  "Create a shallow copy of a ladder cell"
  (when cell
    (make-ladder-cell
     :type (ladder-cell-type cell)
     :symbol (ladder-cell-symbol cell)
     :address (ladder-cell-address cell)
     :addresses (copy-list (ladder-cell-addresses cell))
     :value (ladder-cell-value cell)
     :opcode (ladder-cell-opcode cell)
     :params (copy-list (ladder-cell-params cell))
     :row (ladder-cell-row cell)
     :col (ladder-cell-col cell)
     :monitor-type (ladder-cell-monitor-type cell))))

(defun append-cell-to-matrix (cell matrix)
  "Append CELL to the first row of MATRIX, padding other rows with nil.
   Returns the modified matrix."
  (let ((cell-copy (copy-cell cell)))
    ;; Add to first row
    (setf (first matrix) (append (first matrix) (list cell-copy)))
    ;; Pad other rows with nil to keep rectangular
    (dolist (row (rest matrix))
      (nconc row (list nil)))
    matrix))

(defun append-cell-to-row (cell matrix target-row)
  "Append CELL to a specific TARGET-ROW of MATRIX, padding other rows with hbar.
   Returns the modified matrix."
  (let* ((cell-copy (copy-cell cell))
         (height (matrix-height matrix))
         (target-row (min target-row (1- height))))
    ;; Add to target row
    (setf (nth target-row matrix) (append (nth target-row matrix) (list cell-copy)))
    ;; Pad other rows with hbar to keep rectangular
    (dotimes (i height)
      (unless (= i target-row)
        (let ((row (nth i matrix)))
          (nconc row (list (make-hbar-cell))))))
    matrix))

(defun append-cell-to-last-row (cell matrix)
  "Append CELL to the LAST row of MATRIX, padding other rows with hbar.
   Returns the modified matrix."
  (let* ((cell-copy (copy-cell cell))
         (height (matrix-height matrix))
         (last-row-idx (1- height))
         (last-row (nth last-row-idx matrix)))
    ;; Add to last row
    (setf last-row (append last-row (list cell-copy)))
    (setf (nth last-row-idx matrix) last-row)
    ;; Pad other rows with hbar to keep rectangular
    (dotimes (i last-row-idx)
      (let ((row (nth i matrix)))
        (nconc row (list (make-hbar-cell)))))
    matrix))

(defun has-fork-column-p (matrix)
  "Check if matrix already has a fork connector column at column 0.
   Returns T if the first cell of the first row is a vertical branch connector."
  (and matrix
       (first matrix)
       (first (first matrix))
       (vertical-branch-symbol-p (ladder-cell-symbol (first (first matrix))))))

(defun merge-matrix-below (original-matrix new-matrix)
   "Merge NEW-MATRIX below ORIGINAL-MATRIX (for OR/ORSTR parallel connections).
    Only pads matrices to same width with hbar cells - does NOT add branch connectors.
    Branch connectors should be added separately by close-branch-block.
    Returns the merged matrix."
   (let* ((original-width (matrix-width original-matrix))
          (new-width (matrix-width new-matrix))
          (max-width (max original-width new-width)))

     ;; Pad original rows with hbar cells (only rows with content)
     (dolist (row original-matrix)
       (let ((row-width (length row)))
         (dotimes (i (- max-width row-width))
           (nconc row (list (make-hbar-cell))))))

     ;; Pad new rows with hbar cells (only rows with content)
     (dolist (row new-matrix)
       (let ((row-width (length row)))
         (dotimes (i (- max-width row-width))
           (nconc row (list (make-hbar-cell))))))

     ;; Merge: append new-matrix rows to original-matrix
     (nconc original-matrix new-matrix)
     original-matrix))

(defun merge-matrix-right (original-matrix new-matrix)
   "Merge NEW-MATRIX to the right of ORIGINAL-MATRIX (for ANDSTR series connection).
    When heights are equal, does horizontal merge with hbar (no vertical connectors).
    When heights differ, pads matrices and merges horizontally.
    Adds fork connector to the ORIGINAL matrix where the fork occurs.
    Returns the merged matrix."
   (let ((original-height (matrix-height original-matrix))
         (new-height (matrix-height new-matrix))
         (original-width (matrix-width original-matrix))
         (new-width (matrix-width new-matrix))
         (fork-col 0))

      ;; Handle empty matrices
      (when (zerop original-width)
        (return-from merge-matrix-right original-matrix))

      ;; If heights differ, pad the shorter matrix with nil rows to match
      (cond
        ;; Original is taller - pad new-matrix with nil rows
        ((> original-height new-height)
         (dotimes (i (- original-height new-height))
           (let ((empty-row (make-list (matrix-width new-matrix) :initial-element nil)))
             (nconc new-matrix (list empty-row)))))
        ;; New is taller - pad original-matrix with nil rows
        ((> new-height original-height)
         (dotimes (i (- new-height original-height))
           (let ((empty-row (make-list (matrix-width original-matrix) :initial-element nil)))
             (nconc original-matrix (list empty-row))))))

      ;; Set fork-col to original width for later use
      (setf fork-col original-width)

      ;; Pad all rows with hbar to match max width
      (let ((max-width (max (matrix-width original-matrix) (matrix-width new-matrix))))
        (dolist (row original-matrix)
          (let ((row-width (length row)))
            (dotimes (i (- max-width row-width))
              (nconc row (list (make-hbar-cell))))))
        (dolist (row new-matrix)
          (let ((row-width (length row)))
            (dotimes (i (- max-width row-width))
              (nconc row (list (make-hbar-cell)))))))

      ;; Now add fork connector at fork-col for each row of original
      (dotimes (row-idx original-height)
        (let ((row (nth row-idx original-matrix)))
          ;; Replace the hbar at fork-col with a branch connector
          (when (and (< fork-col (length row)) (nth fork-col row))
            (setf (nth fork-col row) (make-branch-tr-cell :row row-idx :col fork-col)))))

      ;; Horizontal merge: extend each original row with corresponding new row
      (loop for orig-row in original-matrix
            for new-row in new-matrix
            do (nconc orig-row new-row))

      original-matrix))

(defun close-branch-block (matrix)
  "Add or update RIGHT-side branch connectors after merging rows (for OR/ORSTR).
   Uses branchttr at top (normalizes to brancht), branchtr in middle, branchr at bottom.
   This closes the branch on the right side connecting back to the main rail.
   Returns the modified matrix. (Matches demodata.js conventions)"
  (let ((height (matrix-height matrix))
        (width (matrix-width matrix)))
    (when (> height 1)
      ;; Check if any cell in the last column is a vertical branch connector
      (let ((has-existing-branch nil))
        (dolist (row matrix)
          (let ((last-cell (when (>= (length row) width)
                             (nth (1- width) row))))
            (when (and last-cell
                       (vertical-branch-symbol-p (ladder-cell-symbol last-cell)))
              (setf has-existing-branch t))))

        (if has-existing-branch
            ;; UPDATE existing branch column - only update hbar cells, leave existing branches alone
            (loop for row in matrix
                  for i from 0
                  for target-symbol = (cond
                                        ((= i 0) *branch-ttr*)
                                        (t *branch-r*))  ; All non-top rows get branchr
                  do (let ((cell (when (>= (length row) width)
                                  (nth (1- width) row))))
                       (cond
                         ;; Cell is hbar - replace with branch connector
                         ((and cell (string-equal (ladder-cell-symbol cell) *hbar*))
                          (setf (ladder-cell-symbol cell) target-symbol))
                         ;; Cell is already a vertical branch - leave it alone
                         ((and cell (vertical-branch-symbol-p (ladder-cell-symbol cell)))
                          nil)  ; do nothing, preserve existing
                         ;; Cell exists but is something else - replace
                         (cell
                          (setf (nth (1- width) row)
                                (make-branch-cell target-symbol
                                                  :row i :col (1- width))))
                         ;; Row is shorter than width - pad and add
                         (t
                          (dotimes (j (- width (length row) 1))
                            (nconc row (list nil)))
                          (nconc row (list (make-branch-cell target-symbol
                                                            :row i :col (1- width))))))))
            ;; No existing branch column - ADD new column
            (loop for row in matrix
                  for i from 0
                  do (let ((new-cell (cond
                                       ((= i 0) (make-branch-ttr-cell))
                                       ((= i (1- height)) (make-branch-r-cell))
                                       (t (make-branch-tr-cell)))))
                       (nconc row (list new-cell))))))))
  matrix)

(defun add-orstr-junction (matrix top-height)
  "Add junction connectors at the ORSTR merge point.
   TOP-HEIGHT is the number of rows from the top matrix (before merge).
   This adds vertical connections between the bottom of the top section
   and the top of the bottom section at the rightmost column.
   Result: top-bottom-row gets branchtl (├), bottom-top-row gets branchr (┘)
   Returns the modified matrix."
  (let ((width (matrix-width matrix))
        (height (matrix-height matrix)))
    (when (and (> width 0) (> height top-height) (> top-height 0))
      (let* ((junction-col (1- width))
             (top-bottom-row (1- top-height))
             (bottom-top-row top-height)
             (top-row (nth top-bottom-row matrix))
             (bottom-row (nth bottom-top-row matrix)))
        ;; Top section bottom row: convert to branchtl (├) - connects up, down, left
        (when (and top-row (>= (length top-row) width))
          (let ((top-cell (nth junction-col top-row)))
            (when top-cell
              (setf (ladder-cell-symbol top-cell) *branch-tl*))))
        ;; Bottom section top row: convert to branchr (┘) - connects up, left
        (when (and bottom-row (>= (length bottom-row) width))
          (let ((bottom-cell (nth junction-col bottom-row)))
            (when bottom-cell
              (setf (ladder-cell-symbol bottom-cell) *branch-r*))))))
    matrix))

;;; ============================================================
;;; Network to Ladder Rung Conversion
;;; ============================================================

(defun multi-address-coil-p (opcode params)
  "Check if this is a coil instruction with multiple output addresses"
  (and (coil-instruction-p opcode)
       (> (length params) 1)))

(defun make-coil-cell (opcode addr col row &optional is-range)
  "Create a single coil cell for one address.
   IS-RANGE indicates if this is part of a range address output."
  (let ((symbol (cond
                  ((string-equal opcode "OUT") (if is-range "out2" "out"))
                  ((string-equal opcode "SET") (if is-range "set2" "set"))
                  ((string-equal opcode "RST") (if is-range "rst2" "rst"))
                  ((string-equal opcode "PD") (if is-range "pd2" "pd"))
                  (t "out"))))
    (make-ladder-cell
     :type :coil
     :symbol symbol
     :address addr
     :addresses (list addr)
     :opcode opcode
     :params (list addr)
     :row row
     :col col
     :monitor-type :bool)))

(defun network-to-ladder-rung (network)
  "Convert a parsed network to a ladder rung structure using matrix-based algorithm.
   This produces Python-compatible matrixdata with explicit branch connector cells.

   Algorithm (matches Python PLCLadder.py):
   - Matrix is a list of rows: ((row0-cells) (row1-cells) ...)
   - matrixstack holds matrices for nested blocks
   - STR: push current matrix, start new
   - AND: append cell to current row
   - OR: create new row matrix, merge below, close block (add right connectors)
   - ORSTR: pop stack, merge below, close block
   - ANDSTR: pop stack, merge right (add left connectors)
   - Outputs are handled separately after inputs"
  (let ((instructions (mblogic-cl:network-instructions network))
        (all-addresses nil)
        ;; Input matrix processing
        (current-matrix (list (list)))  ; Start with one empty row
        (matrix-stack (list))           ; Stack for STR blocks
        ;; Output collection
        (output-cells nil)
        ;; First STR flag - don't push empty matrix on first STR
        (first-store t))

    ;; Separate inputs from outputs
    (let ((inputs nil)
          (outputs nil))
      (dolist (instr instructions)
        (let ((opcode (mblogic-cl:parsed-opcode instr)))
          (cond
            ((coil-instruction-p opcode)
             (push instr outputs))
            ((control-instruction-p opcode)
             (push instr outputs))
            ((output-block-instruction-p opcode)
             (push instr outputs))
            (t
             (push instr inputs)))))
      (setf inputs (nreverse inputs))
      (setf outputs (nreverse outputs))

      ;; Process input instructions using matrix algorithm
      (dolist (instr inputs)
        (let* ((opcode (mblogic-cl:parsed-opcode instr))
               (cell (instruction-to-cell instr 0)))

          ;; Collect addresses
          (dolist (addr (ladder-cell-addresses cell))
            (pushnew addr all-addresses :test #'string-equal))

          (cond
            ;; STR instruction - start new logic block
            ((store-instruction-p opcode)
             (if first-store
                 ;; First STR - just add to current matrix
                 (progn
                   (setf first-store nil)
                   (setf current-matrix (append-cell-to-matrix cell current-matrix)))
                 ;; Subsequent STR - push current and start new
                 (progn
                   (push current-matrix matrix-stack)
                   (setf current-matrix (list (list)))
                   (setf current-matrix (append-cell-to-matrix cell current-matrix)))))

            ;; AND instruction - append to current row
            ((and-instruction-p opcode)
             (setf current-matrix (append-cell-to-matrix cell current-matrix)))

            ;; OR instruction - create parallel branch below, then close block
            ;; This matches Python algorithm: merge below, then add right-side closing connectors
            ((or-instruction-p opcode)
             (let ((new-matrix (list (list))))
               (setf new-matrix (append-cell-to-matrix cell new-matrix))
               (setf current-matrix (merge-matrix-below current-matrix new-matrix))
               (setf current-matrix (close-branch-block current-matrix))))

            ;; ORSTR - pop and merge below, then add right-side closing connectors
            ;; This matches Python algorithm: merge below, then close-branch-block
            ((orstr-instruction-p opcode)
             (when matrix-stack
               (let ((old-matrix (pop matrix-stack)))
                 (setf current-matrix (merge-matrix-below old-matrix current-matrix))
                 (setf current-matrix (close-branch-block current-matrix)))))

            ;; ANDSTR - pop and merge right with left-side connectors
            ((andstr-instruction-p opcode)
             (when matrix-stack
               (let ((old-matrix (pop matrix-stack)))
                 (setf current-matrix (merge-matrix-right old-matrix current-matrix)))))

            ;; Other instructions (comparisons, etc.) - treat as AND
            (t
             (setf current-matrix (append-cell-to-matrix cell current-matrix))))))

      ;; Unwind any remaining matrices on the stack.
      ;; This handles double/triple rungs where multiple STR instructions push
      ;; matrices that are never consumed by ANDSTR/ORSTR (because the
      ;; counter/timer is an output block, not an input merge point).
      (loop while matrix-stack
            do (let ((old-matrix (pop matrix-stack)))
                 (setf current-matrix (merge-matrix-below old-matrix current-matrix))))

      ;; Process output instructions
      ;; First pass: collect all output cells with sequential row numbers
      (let ((output-row 0))
        (dolist (instr outputs)
          (let* ((opcode (mblogic-cl:parsed-opcode instr))
                 (params (mblogic-cl:parsed-params instr)))
            (cond
              ;; Coil with potentially multiple addresses
              ((coil-instruction-p opcode)
               ;; Check if this is a range address (2 params for coil = range)
               (let ((is-range (> (length params) 1)))
                 (dolist (addr params)
                   (when (mblogic-cl:bool-addr-p addr)
                     ;; Column 1 to leave room for branch connector at column 0
                     (let ((cell (make-coil-cell opcode addr 1 output-row is-range)))
                       (push cell output-cells)
                       (pushnew addr all-addresses :test #'string-equal)
                       (incf output-row))))))
              ;; Output block instructions (TMR, CNTU, COPY, MATHDEC, etc.)
              ((output-block-instruction-p opcode)
               (let* ((instr-def (mblogic-cl:parsed-instruction-def instr))
                      (ladsymb (when instr-def (mblogic-cl:instruction-ladsymb instr-def)))
                      (svg-symbol (ladsymb-to-svg-symbol ladsymb opcode))
                      ;; All params go into addresses (matches demodata.js format)
                      (addr-list (if params (copy-list params) (list "")))
                      (cell (make-ladder-cell
                             :type :coil
                             :symbol svg-symbol
                             :address (first params)
                             :addresses addr-list
                             :opcode opcode
                             :params params
                             :row output-row
                             :col 1
                             :monitor-type (when instr-def
                                             (mblogic-cl:instruction-monitor instr-def)))))
                 (push cell output-cells)
                 ;; Track PLC addresses for monitoring
                 (dolist (p params)
                   (when (mblogic-cl:any-addr-p p)
                     (pushnew p all-addresses :test #'string-equal)))
                 (incf output-row)))
              ;; Control instructions (END, RT, etc.)
              (t
               (let ((cell (instruction-to-cell instr 0)))
                 (setf (ladder-cell-row cell) output-row)
                 (setf (ladder-cell-col cell) 1)
                 (push cell output-cells)
                 (dolist (addr (ladder-cell-addresses cell))
                   (pushnew addr all-addresses :test #'string-equal))
                 (incf output-row)))))

        ;; Note: JS handles parallel output rendering automatically based on
        ;; having multiple outputeditN entries - no branch connectors needed
        )

      ;; Post-process: convert branchr to branchtu when there's an explicit hbar to the right
      ;; This creates the ┴ junction for branches that continue horizontally
      ;; Only convert when the next cell is explicitly hbar, not when the row ends (nil)
      ;; This preserves branchr as the closing connector for shorter branch rows
      (dolist (row current-matrix)
        (loop for col-idx from 0 below (1- (length row))
              for cell = (nth col-idx row)
              for next-cell = (nth (1+ col-idx) row)
              when (and cell
                        (string-equal (ladder-cell-symbol cell) *branch-r*)
                        next-cell  ; Only if there IS a next cell
                        (string-equal (ladder-cell-symbol next-cell) *hbar*))
              do (setf (ladder-cell-symbol cell) *branch-tu*)))

      ;; Convert matrix to flat cell list with correct row/col positions
      ;; Fill nil cells with hbar for horizontal wire connections
      (let ((input-cells nil)
            (matrix-height (matrix-height current-matrix))
            (matrix-width (matrix-width current-matrix)))
        (loop for row in current-matrix
              for row-idx from 0
              do (loop for cell in row
                       for col-idx from 0
                       do (cond
                            ;; Real cell - set position
                            (cell
                             (setf (ladder-cell-row cell) row-idx)
                             (setf (ladder-cell-col cell) col-idx)
                             (push cell input-cells))
                            ;; Nil in row 0 - fill with hbar for wire continuity
                            ((= row-idx 0)
                             (let ((hbar (make-hbar-cell :row row-idx :col col-idx)))
                               (push hbar input-cells)))
                            ;; Nil in branch row before first non-nil in that row
                            ;; Check if there's any cell in this row at a later column
                            ((let ((has-later-cell nil))
                               (loop for later-col from (1+ col-idx) below (length row)
                                     when (nth later-col row)
                                     do (setf has-later-cell t) (return))
                               has-later-cell)
                             (let ((hbar (make-hbar-cell :row row-idx :col col-idx)))
                               (push hbar input-cells))))))

        ;; Build the rung (no longer needs branch metadata - it's in the cells)
        (make-ladder-rung
         :number (mblogic-cl:network-number network)
         :cells (append (nreverse input-cells) (nreverse output-cells))
         :rows (max matrix-height (length output-cells) 1)
         :cols matrix-width
         :addresses (nreverse all-addresses)
         :comment (first (mblogic-cl:network-comments network))
         :branches nil           ; No longer needed - explicit cells
         :output-branches nil)))))) ; No longer needed - explicit cells

;;; ============================================================
;;; Program/Subroutine to Ladder Conversion
;;; ============================================================

(defun networks-to-ladder (networks name)
  "Convert a list of networks to a ladder program structure"
  (let ((rungs nil)
        (all-addresses nil))

    (dolist (net networks)
      (let ((rung (network-to-ladder-rung net)))
        (push rung rungs)
        (dolist (addr (ladder-rung-addresses rung))
          (pushnew addr all-addresses :test #'string-equal))))

    (make-ladder-program
     :name name
     :rungs (nreverse rungs)
     :addresses (sort (copy-list all-addresses) #'string<))))

(defun program-to-ladder (parsed-program &optional (name "main"))
  "Convert a parsed program to ladder diagram structure.
   NAME specifies which subroutine (or 'main' for main program)."
  (if (string-equal name "main")
      (networks-to-ladder (mblogic-cl:program-main-networks parsed-program) "main")
      (let ((sbr (gethash name (mblogic-cl:program-subroutines parsed-program))))
        (when sbr
          (networks-to-ladder (mblogic-cl:subroutine-networks sbr) name)))))

(defun list-subroutine-names (parsed-program)
  "Return list of all subroutine names in a parsed program"
  (let ((names '("main")))
    (maphash (lambda (name sbr)
               (declare (ignore sbr))
               (push name names))
             (mblogic-cl:program-subroutines parsed-program))
    (sort names #'string<)))

;;; ============================================================
;;; Cell to JSON-ready Plist Conversion (Python-compatible matrixdata format)
;;; ============================================================

(defun format-monitor-info (cell)
  "Format monitor info for a cell in Python-compatible format.
   Returns a list like (\"bool\" \"X1\") or (\"none\")."
  (let ((monitor-type (ladder-cell-monitor-type cell))
        (addr (ladder-cell-address cell)))
    (cond
      ;; Boolean monitoring
      ((and (eq monitor-type :bool) addr)
       (list "bool" addr))
      ;; No monitoring (for branch connectors, etc.)
      (t
       (list "none")))))

(defun cell-to-matrixdata (cell)
  "Convert a ladder cell to Python-compatible matrixdata format.
   Format: {type, row, col, addr, value, monitor}"
  (list :type (if (member (ladder-cell-type cell) '(:coil :output-branch :control))
                  "outp"
                  "inp")
        :row (ladder-cell-row cell)
        :col (ladder-cell-col cell)
        :addr (let ((addrs (ladder-cell-addresses cell)))
                (if addrs addrs (list "")))  ; Empty string in list for no addresses
        :value (ladder-cell-symbol cell)
        :monitor (format-monitor-info cell)))

;; Keep old function for backwards compatibility during transition
(defun cell-to-plist (cell)
  "Convert a ladder cell to a plist for JSON serialization (legacy format)"
  (list :type (string-downcase (symbol-name (ladder-cell-type cell)))
        :symbol (ladder-cell-symbol cell)
        :addr (ladder-cell-address cell)
        :addrs (ladder-cell-addresses cell)
        :opcode (ladder-cell-opcode cell)
        :params (ladder-cell-params cell)
        :row (ladder-cell-row cell)
        :col (ladder-cell-col cell)
        :monitor (when (ladder-cell-monitor-type cell)
                   (string-downcase (symbol-name (ladder-cell-monitor-type cell))))))

(defun rung-to-matrixdata (rung)
  "Convert a ladder rung to Python-compatible format.
   Format: {rungnum, rungtype, comment, ildata, matrixdata}"
  (list :rungnum (ladder-rung-number rung)
        :rungtype (determine-rungtype rung)
        :comment (or (ladder-rung-comment rung) "")
        :ildata #()  ; TODO: capture original IL if needed
        :matrixdata (mapcar #'cell-to-matrixdata (ladder-rung-cells rung))))

;; Keep old function for backwards compatibility
(defun rung-to-plist (rung)
  "Convert a ladder rung to a plist for JSON serialization (legacy format)"
  (list :rungnum (ladder-rung-number rung)
        :rows (ladder-rung-rows rung)
        :cols (ladder-rung-cols rung)
        :comment (ladder-rung-comment rung)
        :addrs (ladder-rung-addresses rung)
        :cells (mapcar #'cell-to-plist (ladder-rung-cells rung))
        :branches nil    ; No longer used
        :output-branches nil))  ; No longer used

(defun ladder-program-to-matrixdata (ladder-prog)
  "Convert a ladder program to Python-compatible format.
   Format: {subrname: {subrdata, subrcomments, signature}}"
  (list :subrdata (mapcar #'rung-to-matrixdata (ladder-program-rungs ladder-prog))
        :subrcomments ""
        :signature ""))  ; TODO: compute MD5 hash if needed

;; Keep old function for backwards compatibility
(defun ladder-program-to-plist (ladder-prog)
  "Convert a ladder program to a plist for JSON serialization (legacy format)"
  (list :subrname (ladder-program-name ladder-prog)
        :addresses (ladder-program-addresses ladder-prog)
        :subrdata (mapcar #'rung-to-plist (ladder-program-rungs ladder-prog))))

;;; ============================================================
;;; JavaScript-Compatible Format Conversion (demodata.js format)
;;; ============================================================
;;; These functions produce output compatible with the MBLogic ladtest
;;; JavaScript files (ladsubrdisplib.js, ladeditlib.js, etc.)

(defun cl-symbol-to-js-symbol (symbol)
  "Map CL branch symbols to JavaScript-compatible symbol names.
   JS only has: brancht, branchl, branchr, branchtl, branchtr, branchtu, branchx, vbar, hbar"
  (cond
    ((null symbol) "")
    ;; Top corners (fork start or merge end)
    ((string-equal symbol "branchttr") "brancht")  ; Top-right fork start: ┐
    ((string-equal symbol "branchttl") "brancht")  ; Top-left merge end: ┌
    ;; Middle T-junctions
    ((string-equal symbol "branchtr") "branchtr")  ; Middle-right fork: ┤ (pass through)
    ((string-equal symbol "branchtl") "branchtl")  ; Middle-left merge: ├ (pass through)
    ;; Bottom corners (fork end or merge start)
    ((string-equal symbol "branchr") "branchr")    ; Bottom-right fork end: ┘ (pass through)
    ((string-equal symbol "branchl") "branchl")    ; Bottom-left merge start: └ (pass through)
    ;; Special junctions
    ((string-equal symbol "branchtu") "branchtu")  ; Top continuation from right: ┴
    ((string-equal symbol "branchx") "branchx")    ; Cross junction: ╳
    ;; Vertical bars (no junction)
    ((string-equal symbol "vbarr") "vbar")
    ((string-equal symbol "vbarl") "vbar")
    (t symbol)))  ; All others pass through unchanged

(defun cell-to-js-format (cell)
  "Convert ladder cell to JS format as alist: ((value . \"noc\") (addr . (\"X1\")))"
  (let ((symbol (cl-symbol-to-js-symbol (ladder-cell-symbol cell)))
        (addrs (ladder-cell-addresses cell)))
    (list (cons :value symbol)
          (cons :addr (if addrs addrs (list ""))))))  ; Branch symbols get [""]

(defun determine-rungtype (rung)
  "Determine rungtype based on output block instruction type.
   The rungtype is driven by the output block instruction (if any):
   - CNTU/CNTD/TMRA → double (2 input rows)
   - UDC/SHFRG → triple (3 input rows)
   - Everything else → single
   Verifies the actual input row count fits the constraint."
  (let ((cells (ladder-rung-cells rung)))
    (if (null cells)
        "empty"
        ;; Find the first output block instruction's opcode
        (let ((block-opcode nil)
              (max-input-row 0))
          (dolist (cell cells)
            (let ((opcode (ladder-cell-opcode cell)))
              (cond
                ;; Track input rows (non-output cells)
                ((not (member (ladder-cell-type cell) '(:coil :control :output-branch)))
                 (setf max-input-row (max max-input-row (ladder-cell-row cell))))
                ;; Find first output block instruction
                ((and (null block-opcode)
                      opcode
                      (output-block-instruction-p opcode))
                 (setf block-opcode opcode)))))
          (if block-opcode
              ;; Get base rungtype from the output block instruction
              (let ((base-type (rungtype-for-output block-opcode)))
                ;; Verify input row count fits the constraint
                (cond
                  ((and (string= base-type "triple") (<= max-input-row 2)) "triple")
                  ((and (string= base-type "double") (<= max-input-row 1)) "double")
                  ((string= base-type "single") "single")
                  ;; Fall back to single if rows overflow
                  (t "single")))
              ;; No output block instruction → always single
              "single")))))

(defun rung-to-js-format (rung rung-index)
  "Convert rung to JS format with matrixdata as object (not array).
   Keys are 'inputeditRC' for input cells and 'outputeditN' for output cells.
   Returns an alist for proper JSON encoding.
   Note: Filters cells to match JavaScript constraints (max 3 input rows, 8 output rows)."
  (let ((matrixdata-alist nil))
    ;; First determine rungtype to know the constraints
    (let* ((rungtype (determine-rungtype rung))
           ;; JavaScript constraints based on MatrixParams in ladsymbols.js
           (max-input-row (cond ((string= rungtype "single") 7)
                                ((string= rungtype "double") 1)
                                ((string= rungtype "triple") 2)
                                (t 7)))
           (max-output-row (cond ((string= rungtype "single") 7)
                                 ((string= rungtype "double") 0)
                                 ((string= rungtype "triple") 0)
                                 (t 7)))
           (max-input-col 7))  ; JS key format only supports single-digit columns (0-7)
      
      ;; Build matrixdata as alist with inputeditRC/outputeditN keys
      ;; Filter cells to only those within JavaScript constraints
      (dolist (cell (ladder-rung-cells rung))
        (let* ((type (ladder-cell-type cell))
               (row (ladder-cell-row cell))
               (col (ladder-cell-col cell)))
          ;; Only include cells within JavaScript's supported matrix dimensions
          (when (if (member type '(:coil :control :output-branch))
                    (<= row max-output-row)
                    (and (<= row max-input-row) (<= col max-input-col)))
            (let ((key (if (member type '(:coil :control :output-branch))
                           (format nil "outputedit~D" row)
                           (format nil "inputedit~D~D" row col))))
              (push (cons key (cell-to-js-format cell)) matrixdata-alist)))))

      ;; Return rung structure as alist for proper JSON encoding
      ;; Include reference field for JavaScript rung tracking (required by ladsubrdata.js)
      (list (cons :matrixdata (nreverse matrixdata-alist))
            (cons :rungtype rungtype)
            (cons :ildata ())  ; Empty IL data array (required by JavaScript)
            (cons :comment (or (ladder-rung-comment rung) ""))
            (cons :reference rung-index)))))

(defun ladder-program-to-js-format (ladder-prog)
  "Convert ladder program to full JS-compatible format for demodata.js.
   Returns an alist for proper JSON encoding."
  (let ((rungs-js nil)
        (idx 0))
    (dolist (rung (ladder-program-rungs ladder-prog))
      (push (rung-to-js-format rung idx) rungs-js)
      (incf idx))
    (list (cons :subroutinename (ladder-program-name ladder-prog))
          (cons :subrcomments "")
          (cons :signature 0)
          (cons :rungdata (nreverse rungs-js)))))

;;; End of ladder-render.lisp
