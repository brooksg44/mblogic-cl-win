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
    (:end . "end")              ; End
    (:for . "for")              ; For loop start
    (:next . "next"))           ; For loop end
  "Mapping from instruction :ladsymb to SVG symbol names")

(defun ladsymb-to-svg-symbol (ladsymb opcode)
  "Convert instruction ladsymb keyword to SVG symbol name.
   Uses opcode for more specific mapping when needed."
  (let ((svg-sym (cdr (assoc ladsymb *ladsymb-to-svg*))))
    ;; Handle special cases where opcode matters
    (cond
      ;; Timers
      ((and (eq ladsymb :timer) (string-equal opcode "TMR")) "tmr")
      ((and (eq ladsymb :timer) (string-equal opcode "TMRA")) "tmra")
      ((and (eq ladsymb :timer) (string-equal opcode "TMROFF")) "tmroff")
      ;; Counters
      ((and (eq ladsymb :counter) (string-equal opcode "CNTU")) "cntu")
      ((and (eq ladsymb :counter) (string-equal opcode "CNTD")) "cntd")
      ((and (eq ladsymb :counter) (string-equal opcode "UDC")) "udc")
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
;;; Instruction Classification
;;; ============================================================

(defun contact-instruction-p (opcode)
  "Check if opcode is a contact (input) instruction"
  (member opcode '("STR" "STRN" "AND" "ANDN" "OR" "ORN"
                   "STRPD" "STRND" "ANDPD" "ANDND" "ORPD" "ORND")
          :test #'string-equal))

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

      ;; Compare instructions - word addresses
      ((member opcode '("STRE" "STRNE" "STRGT" "STRLT" "STRGE" "STRLE"
                        "ANDE" "ANDNE" "ANDGT" "ANDLT" "ANDGE" "ANDLE"
                        "ORE" "ORNE" "ORGT" "ORLT" "ORGE" "ORLE")
               :test #'string-equal)
       (dolist (p params)
         (when (mblogic-cl:word-addr-p p)
           (push p addresses))))

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
;;; Network to Ladder Rung Conversion
;;; ============================================================

(defun multi-address-coil-p (opcode params)
  "Check if this is a coil instruction with multiple output addresses"
  (and (coil-instruction-p opcode)
       (> (length params) 1)))

(defun make-coil-cell (opcode addr col row)
  "Create a single coil cell for one address"
  (let ((symbol (cond
                  ((string-equal opcode "OUT") "out")
                  ((string-equal opcode "SET") "set")
                  ((string-equal opcode "RST") "rst")
                  ((string-equal opcode "PD") "pd")
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
  "Convert a parsed network to a ladder rung structure.
   Handles simple linear rungs, OR branches, and parallel output coils.

   Branch logic: When OR/ORN is encountered:
   - A new row is created starting from column 0
   - The OR contact is placed at column 0 on the branch row
   - A merge column is reserved for the vertical connector
   - After processing OR contact, we return to main row
   - ORSTR explicitly merges all branches (handled structurally)

   Parallel coils: Consecutive coil instructions are all parallel:
   - All coils at the end of a rung share the same logic
   - Each coil address gets its own row at the same column
   - Vertical connectors link them for parallel output"
  (let ((instructions (mblogic-cl:network-instructions network))
        (cells nil)
        (all-addresses nil)
        (col 0)
        (current-row 0)
        (max-row 0)
        (branch-info nil)        ; List of (row start-col merge-col) for input branches
        (output-branch-info nil) ; List of (col row1 row2 ...) for parallel coils
        (coil-col nil)           ; Column where parallel coils are placed
        (coil-rows nil))         ; List of rows with coils at coil-col

    ;; Process each instruction
    (dolist (instr instructions)
      (let* ((opcode (mblogic-cl:parsed-opcode instr))
             (params (mblogic-cl:parsed-params instr)))

        (cond
          ;; OR starts a new branch row - the contact goes at col 0 on new row
          ;; Reserve current column for the vertical merge connector
          ((and (branch-start-p opcode) (> col 0))
           ;; Finalize any pending coil group
           (when (and coil-col coil-rows (> (length coil-rows) 1))
             (push (cons coil-col (nreverse coil-rows)) output-branch-info))
           (setf coil-col nil coil-rows nil)

           (incf max-row)
           (let ((branch-row max-row)
                 (merge-col col))  ; This column is reserved for vertical connector
             ;; Record branch info for rendering vertical connectors
             (push (list branch-row 0 merge-col) branch-info)
             ;; Create cell at column 0 on the branch row
             (let ((cell (instruction-to-cell instr 0)))
               (setf (ladder-cell-row cell) branch-row)
               (dolist (addr (ladder-cell-addresses cell))
                 (pushnew addr all-addresses :test #'string-equal))
               (push cell cells))
             ;; Advance past the merge column so next instruction doesn't overlap
             (incf col))
           ;; Return to main row for subsequent instructions
           (setf current-row 0))

          ;; ANDSTR/ORSTR merges branches - structural, no cell needed
          ((branch-end-p opcode)
           ;; Finalize any pending coil group
           (when (and coil-col coil-rows (> (length coil-rows) 1))
             (push (cons coil-col (nreverse coil-rows)) output-branch-info))
           (setf coil-col nil coil-rows nil)
           (setf current-row 0))

          ;; Coil instruction - group consecutive coils as parallel
          ((coil-instruction-p opcode)
           ;; Start a new coil group if not already in one
           (unless coil-col
             (setf coil-col col)
             (setf coil-rows nil))
           ;; Add each address as a separate coil on its own row
           (dolist (addr params)
             (when (mblogic-cl:bool-addr-p addr)
               (let ((cell (make-coil-cell opcode addr coil-col
                                           (if (null coil-rows)
                                               current-row
                                               (incf max-row)))))
                 (setf (ladder-cell-row cell) (if (null coil-rows) current-row max-row))
                 (push cell cells)
                 (pushnew addr all-addresses :test #'string-equal)
                 (push (ladder-cell-row cell) coil-rows)))))

          ;; Regular instruction - place on current row
          (t
           ;; Finalize any pending coil group before placing non-coil
           (when (and coil-col coil-rows (> (length coil-rows) 1))
             (push (cons coil-col (nreverse coil-rows)) output-branch-info))
           (when coil-col
             (incf col))  ; Move past the coil column
           (setf coil-col nil coil-rows nil)

           (let ((cell (instruction-to-cell instr col)))
             (setf (ladder-cell-row cell) current-row)
             (dolist (addr (ladder-cell-addresses cell))
               (pushnew addr all-addresses :test #'string-equal))
             (push cell cells)
             (incf col))))))

    ;; Finalize any pending coil group at end
    (when (and coil-col coil-rows)
      (when (> (length coil-rows) 1)
        (push (cons coil-col (nreverse coil-rows)) output-branch-info))
      (incf col))

    ;; Build the rung with branch metadata
    (make-ladder-rung
     :number (mblogic-cl:network-number network)
     :cells (nreverse cells)
     :rows (1+ max-row)
     :cols col
     :addresses (nreverse all-addresses)
     :comment (first (mblogic-cl:network-comments network))
     :branches (nreverse branch-info)
     :output-branches (nreverse output-branch-info))))

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
;;; Cell to JSON-ready Plist Conversion
;;; ============================================================

(defun cell-to-plist (cell)
  "Convert a ladder cell to a plist for JSON serialization"
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

(defun rung-to-plist (rung)
  "Convert a ladder rung to a plist for JSON serialization"
  (list :rungnum (ladder-rung-number rung)
        :rows (ladder-rung-rows rung)
        :cols (ladder-rung-cols rung)
        :comment (ladder-rung-comment rung)
        :addrs (ladder-rung-addresses rung)
        :cells (mapcar #'cell-to-plist (ladder-rung-cells rung))
        :branches (mapcar (lambda (b)
                           (list :row (first b)
                                 :start-col (second b)
                                 :merge-col (third b)))
                         (ladder-rung-branches rung))
        :output-branches (mapcar (lambda (ob)
                                  (list :col (car ob)
                                        :rows (cdr ob)))
                                (ladder-rung-output-branches rung))))

(defun ladder-program-to-plist (ladder-prog)
  "Convert a ladder program to a plist for JSON serialization"
  (list :subrname (ladder-program-name ladder-prog)
        :addresses (ladder-program-addresses ladder-prog)
        :subrdata (mapcar #'rung-to-plist (ladder-program-rungs ladder-prog))))

;;; End of ladder-render.lisp
