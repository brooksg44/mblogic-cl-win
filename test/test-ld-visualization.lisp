;;;; test-ld-visualization.lisp
;;;; Phase 7+: Ladder Diagram Visualization Tests

(in-package #:mblogic-cl-test)

;;; ============================================================
;;; Test Suite Definition
;;; ============================================================

(def-suite ld-visualization-tests
  :description "Ladder diagram visualization tests"
  :in all-tests)

(in-suite ld-visualization-tests)

;;; ============================================================
;;; Helper Functions for Tests
;;; ============================================================

(defun parse-test-il (source)
  "Parse IL source string and return the parsed program"
  (mblogic-cl:parse-il-string source))

(defun make-test-instruction (opcode &rest params)
  "Create a parsed instruction for testing"
  (let ((instr-def (mblogic-cl:find-instruction opcode)))
    (make-instance 'mblogic-cl:parsed-instruction
                   :opcode opcode
                   :params params
                   :line-number 1
                   :instruction-def instr-def)))

;;; ============================================================
;;; Instruction Classification Tests
;;; ============================================================

(test contact-instruction-classification
  "Test that contact instructions are correctly identified"
  (is (mblogic-cl-web::contact-instruction-p "STR"))
  (is (mblogic-cl-web::contact-instruction-p "STRN"))
  (is (mblogic-cl-web::contact-instruction-p "AND"))
  (is (mblogic-cl-web::contact-instruction-p "ANDN"))
  (is (mblogic-cl-web::contact-instruction-p "OR"))
  (is (mblogic-cl-web::contact-instruction-p "ORN"))
  (is (mblogic-cl-web::contact-instruction-p "STRPD"))
  (is (mblogic-cl-web::contact-instruction-p "STRND"))
  (is (mblogic-cl-web::contact-instruction-p "ANDPD"))
  (is (mblogic-cl-web::contact-instruction-p "ANDND"))
  (is (mblogic-cl-web::contact-instruction-p "ORPD"))
  (is (mblogic-cl-web::contact-instruction-p "ORND"))
  ;; Non-contacts
  (is (not (mblogic-cl-web::contact-instruction-p "OUT")))
  (is (not (mblogic-cl-web::contact-instruction-p "TMR")))
  (is (not (mblogic-cl-web::contact-instruction-p "COPY"))))

(test coil-instruction-classification
  "Test that coil instructions are correctly identified"
  (is (mblogic-cl-web::coil-instruction-p "OUT"))
  (is (mblogic-cl-web::coil-instruction-p "SET"))
  (is (mblogic-cl-web::coil-instruction-p "RST"))
  (is (mblogic-cl-web::coil-instruction-p "PD"))
  ;; Non-coils
  (is (not (mblogic-cl-web::coil-instruction-p "STR")))
  (is (not (mblogic-cl-web::coil-instruction-p "AND")))
  (is (not (mblogic-cl-web::coil-instruction-p "TMR"))))

(test branch-instruction-classification
  "Test that branch instructions are correctly identified"
  ;; Branch start (parallel)
  (is (mblogic-cl-web::branch-start-p "OR"))
  (is (mblogic-cl-web::branch-start-p "ORN"))
  (is (mblogic-cl-web::branch-start-p "ORPD"))
  (is (mblogic-cl-web::branch-start-p "ORND"))
  ;; Branch end
  (is (mblogic-cl-web::branch-end-p "ANDSTR"))
  (is (mblogic-cl-web::branch-end-p "ORSTR"))
  ;; Non-branch
  (is (not (mblogic-cl-web::branch-start-p "STR")))
  (is (not (mblogic-cl-web::branch-end-p "AND"))))

(test block-instruction-classification
  "Test that block instructions are correctly identified"
  ;; Timers
  (is (mblogic-cl-web::block-instruction-p "TMR"))
  (is (mblogic-cl-web::block-instruction-p "TMRA"))
  (is (mblogic-cl-web::block-instruction-p "TMROFF"))
  ;; Counters
  (is (mblogic-cl-web::block-instruction-p "CNTU"))
  (is (mblogic-cl-web::block-instruction-p "CNTD"))
  (is (mblogic-cl-web::block-instruction-p "UDC"))
  ;; Data operations
  (is (mblogic-cl-web::block-instruction-p "COPY"))
  (is (mblogic-cl-web::block-instruction-p "CPYBLK"))
  (is (mblogic-cl-web::block-instruction-p "FILL"))
  (is (mblogic-cl-web::block-instruction-p "PACK"))
  (is (mblogic-cl-web::block-instruction-p "UNPACK"))
  ;; Math
  (is (mblogic-cl-web::block-instruction-p "MATHDEC"))
  (is (mblogic-cl-web::block-instruction-p "MATHHEX"))
  ;; Comparisons
  (is (mblogic-cl-web::block-instruction-p "STRE"))
  (is (mblogic-cl-web::block-instruction-p "STRGT"))
  ;; Non-blocks
  (is (not (mblogic-cl-web::block-instruction-p "STR")))
  (is (not (mblogic-cl-web::block-instruction-p "OUT"))))

(test control-instruction-classification
  "Test that control instructions are correctly identified"
  (is (mblogic-cl-web::control-instruction-p "END"))
  (is (mblogic-cl-web::control-instruction-p "ENDC"))
  (is (mblogic-cl-web::control-instruction-p "RT"))
  (is (mblogic-cl-web::control-instruction-p "RTC"))
  (is (mblogic-cl-web::control-instruction-p "NEXT"))
  ;; Non-control
  (is (not (mblogic-cl-web::control-instruction-p "STR")))
  (is (not (mblogic-cl-web::control-instruction-p "CALL"))))

;;; ============================================================
;;; Ladder Symbol Mapping Tests
;;; ============================================================

(test ladsymb-to-svg-mapping
  "Test ladder symbol to SVG mapping"
  ;; Contacts
  (is (string= "noc" (mblogic-cl-web::ladsymb-to-svg-symbol :contact-no "STR")))
  (is (string= "ncc" (mblogic-cl-web::ladsymb-to-svg-symbol :contact-nc "STRN")))
  (is (string= "nocpd" (mblogic-cl-web::ladsymb-to-svg-symbol :contact-pd "STRPD")))
  (is (string= "nocnd" (mblogic-cl-web::ladsymb-to-svg-symbol :contact-nd "STRND")))
  ;; Coils
  (is (string= "out" (mblogic-cl-web::ladsymb-to-svg-symbol :coil "OUT")))
  (is (string= "set" (mblogic-cl-web::ladsymb-to-svg-symbol :coil-set "SET")))
  (is (string= "rst" (mblogic-cl-web::ladsymb-to-svg-symbol :coil-reset "RST")))
  (is (string= "pd" (mblogic-cl-web::ladsymb-to-svg-symbol :coil-pd "PD")))
  ;; Timers (opcode-specific)
  (is (string= "tmr" (mblogic-cl-web::ladsymb-to-svg-symbol :timer "TMR")))
  (is (string= "tmra" (mblogic-cl-web::ladsymb-to-svg-symbol :timer "TMRA")))
  (is (string= "tmroff" (mblogic-cl-web::ladsymb-to-svg-symbol :timer "TMROFF")))
  ;; Counters (opcode-specific)
  (is (string= "cntu" (mblogic-cl-web::ladsymb-to-svg-symbol :counter "CNTU")))
  (is (string= "cntd" (mblogic-cl-web::ladsymb-to-svg-symbol :counter "CNTD")))
  (is (string= "udc" (mblogic-cl-web::ladsymb-to-svg-symbol :counter "UDC")))
  ;; Unknown falls back to "il"
  (is (string= "il" (mblogic-cl-web::ladsymb-to-svg-symbol nil "UNKNOWN"))))

;;; ============================================================
;;; Address Extraction Tests
;;; ============================================================

(test extract-addresses-contact
  "Test address extraction from contact instructions"
  (let ((instr (make-test-instruction "STR" "X1")))
    (let ((addrs (mblogic-cl-web::extract-addresses instr)))
      (is (= 1 (length addrs)))
      (is (string= "X1" (first addrs))))))

(test extract-addresses-coil
  "Test address extraction from coil instructions"
  (let ((instr (make-test-instruction "OUT" "Y1" "Y2" "Y3")))
    (let ((addrs (mblogic-cl-web::extract-addresses instr)))
      (is (= 3 (length addrs)))
      (is (member "Y1" addrs :test #'string=))
      (is (member "Y2" addrs :test #'string=))
      (is (member "Y3" addrs :test #'string=)))))

(test extract-addresses-timer
  "Test address extraction from timer instructions"
  (let ((instr (make-test-instruction "TMR" "T1" "100")))
    (let ((addrs (mblogic-cl-web::extract-addresses instr)))
      ;; Should get T1 (timer bit) and TD1 (timer data)
      (is (= 2 (length addrs)))
      (is (member "T1" addrs :test #'string=))
      (is (member "TD1" addrs :test #'string=)))))

(test extract-addresses-counter
  "Test address extraction from counter instructions"
  (let ((instr (make-test-instruction "CNTU" "CT5" "100")))
    (let ((addrs (mblogic-cl-web::extract-addresses instr)))
      ;; Should get CT5 (counter bit) and CTD5 (counter data)
      (is (= 2 (length addrs)))
      (is (member "CT5" addrs :test #'string=))
      (is (member "CTD5" addrs :test #'string=)))))

;;; ============================================================
;;; Ladder Cell Tests
;;; ============================================================

(test instruction-to-cell-contact
  "Test converting contact instruction to ladder cell"
  (let* ((instr (make-test-instruction "STR" "X1"))
         (cell (mblogic-cl-web::instruction-to-cell instr 0)))
    (is (eq :contact (mblogic-cl-web::ladder-cell-type cell)))
    (is (string= "X1" (mblogic-cl-web::ladder-cell-address cell)))
    (is (string= "STR" (mblogic-cl-web::ladder-cell-opcode cell)))
    (is (= 0 (mblogic-cl-web::ladder-cell-col cell)))))

(test instruction-to-cell-coil
  "Test converting coil instruction to ladder cell"
  (let* ((instr (make-test-instruction "OUT" "Y1"))
         (cell (mblogic-cl-web::instruction-to-cell instr 5)))
    (is (eq :coil (mblogic-cl-web::ladder-cell-type cell)))
    (is (string= "Y1" (mblogic-cl-web::ladder-cell-address cell)))
    (is (string= "OUT" (mblogic-cl-web::ladder-cell-opcode cell)))
    (is (= 5 (mblogic-cl-web::ladder-cell-col cell)))))

(test instruction-to-cell-block
  "Test converting block instruction to ladder cell"
  (let* ((instr (make-test-instruction "TMR" "T1" "100"))
         (cell (mblogic-cl-web::instruction-to-cell instr 2)))
    (is (eq :block (mblogic-cl-web::ladder-cell-type cell)))
    (is (string= "TMR" (mblogic-cl-web::ladder-cell-opcode cell)))
    (is (= 2 (mblogic-cl-web::ladder-cell-col cell)))))

;;; ============================================================
;;; Network to Rung Conversion Tests
;;; ============================================================

(test simple-rung-conversion
  "Test converting a simple network to a ladder rung"
  (let* ((source "NETWORK 1
STR X1
AND X2
OUT Y1
")
         (program (parse-test-il source))
         (network (first (mblogic-cl:program-main-networks program)))
         (rung (mblogic-cl-web::network-to-ladder-rung network)))
    ;; Check rung structure
    (is (= 1 (mblogic-cl-web::ladder-rung-number rung)))
    (is (= 1 (mblogic-cl-web::ladder-rung-rows rung)))  ; No branches = 1 row
    (is (= 3 (mblogic-cl-web::ladder-rung-cols rung)))  ; 3 instructions
    ;; Check cells
    (let ((cells (mblogic-cl-web::ladder-rung-cells rung)))
      (is (= 3 (length cells)))
      (is (string= "STR" (mblogic-cl-web::ladder-cell-opcode (first cells))))
      (is (string= "AND" (mblogic-cl-web::ladder-cell-opcode (second cells))))
      (is (string= "OUT" (mblogic-cl-web::ladder-cell-opcode (third cells)))))))

(test rung-with-addresses
  "Test that rung tracks all addresses"
  (let* ((source "NETWORK 1
STR X1
AND X2
OUT Y1 Y2
")
         (program (parse-test-il source))
         (network (first (mblogic-cl:program-main-networks program)))
         (rung (mblogic-cl-web::network-to-ladder-rung network)))
    (let ((addrs (mblogic-cl-web::ladder-rung-addresses rung)))
      ;; Should have X1, X2, Y1, Y2
      (is (>= (length addrs) 4))
      (is (member "X1" addrs :test #'string=))
      (is (member "X2" addrs :test #'string=))
      (is (member "Y1" addrs :test #'string=))
      (is (member "Y2" addrs :test #'string=)))))

;;; ============================================================
;;; Program to Ladder Conversion Tests
;;; ============================================================

(test program-to-ladder-main
  "Test converting main program to ladder structure"
  (let* ((source "NETWORK 1
STR X1
OUT Y1

NETWORK 2
STR X2
OUT Y2
")
         (program (parse-test-il source))
         (ladder (mblogic-cl-web::program-to-ladder program "main")))
    (is (string= "main" (mblogic-cl-web::ladder-program-name ladder)))
    (is (= 2 (length (mblogic-cl-web::ladder-program-rungs ladder))))
    ;; Check addresses collected from all rungs
    (let ((addrs (mblogic-cl-web::ladder-program-addresses ladder)))
      (is (member "X1" addrs :test #'string=))
      (is (member "Y1" addrs :test #'string=))
      (is (member "X2" addrs :test #'string=))
      (is (member "Y2" addrs :test #'string=)))))

(test program-to-ladder-subroutine
  "Test converting subroutine to ladder structure"
  (let* ((source "NETWORK 1
STR X1
OUT Y1

SBR TestSub
NETWORK 1
STR C1
OUT C2
")
         (program (parse-test-il source))
         (ladder (mblogic-cl-web::program-to-ladder program "TestSub")))
    (is (string= "TestSub" (mblogic-cl-web::ladder-program-name ladder)))
    (is (= 1 (length (mblogic-cl-web::ladder-program-rungs ladder))))))

(test list-subroutine-names
  "Test listing all subroutine names"
  (let* ((source "NETWORK 1
STR X1
OUT Y1

SBR Sub1
NETWORK 1
STR C1
OUT C2

SBR Sub2
NETWORK 1
STR C3
OUT C4
")
         (program (parse-test-il source))
         (names (mblogic-cl-web::list-subroutine-names program)))
    (is (member "main" names :test #'string=))
    (is (member "Sub1" names :test #'string=))
    (is (member "Sub2" names :test #'string=))))

;;; ============================================================
;;; JSON Serialization Tests
;;; ============================================================

(test cell-to-plist-conversion
  "Test converting ladder cell to plist"
  (let* ((instr (make-test-instruction "STR" "X1"))
         (cell (mblogic-cl-web::instruction-to-cell instr 0))
         (plist (mblogic-cl-web::cell-to-plist cell)))
    (is (string= "contact" (getf plist :type)))
    (is (string= "X1" (getf plist :addr)))
    (is (string= "STR" (getf plist :opcode)))
    (is (= 0 (getf plist :col)))))

(test rung-to-plist-conversion
  "Test converting ladder rung to plist"
  (let* ((source "NETWORK 5
STR X1
OUT Y1
")
         (program (parse-test-il source))
         (network (first (mblogic-cl:program-main-networks program)))
         (rung (mblogic-cl-web::network-to-ladder-rung network))
         (plist (mblogic-cl-web::rung-to-plist rung)))
    (is (= 5 (getf plist :rungnum)))
    (is (= 1 (getf plist :rows)))
    (is (= 2 (getf plist :cols)))
    (is (listp (getf plist :cells)))
    (is (= 2 (length (getf plist :cells))))))

(test ladder-program-to-plist-conversion
  "Test converting ladder program to plist"
  (let* ((source "NETWORK 1
STR X1
OUT Y1
")
         (program (parse-test-il source))
         (ladder (mblogic-cl-web::program-to-ladder program "main"))
         (plist (mblogic-cl-web::ladder-program-to-plist ladder)))
    (is (string= "main" (getf plist :subrname)))
    (is (listp (getf plist :addresses)))
    (is (listp (getf plist :subrdata)))
    (is (= 1 (length (getf plist :subrdata))))))

;;; ============================================================
;;; JSON API Tests
;;; ============================================================

(test plist-to-alist-conversion
  "Test plist to alist conversion"
  (let ((plist '(:name "test" :value 42 :active t)))
    (let ((alist (mblogic-cl-web::plist-to-alist plist)))
      (is (equal "test" (cdr (assoc :name alist))))
      (is (= 42 (cdr (assoc :value alist))))
      (is (eq t (cdr (assoc :active alist)))))))

(test plist-to-alist-nested
  "Test nested plist to alist conversion"
  (let ((plist '(:outer (:inner "value"))))
    (let ((alist (mblogic-cl-web::plist-to-alist plist)))
      (is (listp (cdr (assoc :outer alist)))))))

(test parse-address-list
  "Test parsing comma-separated address list"
  (is (equal '("X1" "Y1" "DS1")
             (mblogic-cl-web::parse-address-list "X1,Y1,DS1")))
  (is (equal '("X1" "Y1")
             (mblogic-cl-web::parse-address-list "X1, Y1")))
  (is (null (mblogic-cl-web::parse-address-list "")))
  (is (null (mblogic-cl-web::parse-address-list nil))))

;;; ============================================================
;;; Integration Tests
;;; ============================================================

(test full-ladder-conversion-integration
  "Test full IL program to ladder conversion pipeline"
  (let* ((source "// Simple motor control
NETWORK 1
STR X1
AND X2
OUT Y1

NETWORK 2
STR X3
TMR T1 100
OUT Y2
")
         (program (parse-test-il source))
         (ladder (mblogic-cl-web::program-to-ladder program "main")))
    ;; Verify structure
    (is (not (null ladder)))
    (is (= 2 (length (mblogic-cl-web::ladder-program-rungs ladder))))
    ;; Verify first rung
    (let ((rung1 (first (mblogic-cl-web::ladder-program-rungs ladder))))
      (is (= 1 (mblogic-cl-web::ladder-rung-number rung1)))
      (is (= 3 (length (mblogic-cl-web::ladder-rung-cells rung1)))))
    ;; Verify second rung has timer
    (let* ((rung2 (second (mblogic-cl-web::ladder-program-rungs ladder)))
           (cells (mblogic-cl-web::ladder-rung-cells rung2))
           (timer-cell (find "TMR" cells
                            :key #'mblogic-cl-web::ladder-cell-opcode
                            :test #'string=)))
      (is (not (null timer-cell)))
      (is (eq :block (mblogic-cl-web::ladder-cell-type timer-cell))))))

(test ladder-to-json-integration
  "Test complete ladder to JSON conversion"
  (let* ((source "NETWORK 1
STR X1
OUT Y1
")
         (program (parse-test-il source))
         (ladder (mblogic-cl-web::program-to-ladder program "main"))
         (plist (mblogic-cl-web::ladder-program-to-plist ladder))
         (json (mblogic-cl-web::plist-to-json plist)))
    ;; JSON should be a non-empty string
    (is (stringp json))
    (is (> (length json) 0))
    ;; Should contain expected keys
    (is (search "subrname" json))
    (is (search "subrdata" json))
    (is (search "main" json))))

;;; ============================================================
;;; Edge Case Tests
;;; ============================================================

(test empty-network
  "Test handling of minimal network"
  (let* ((source "NETWORK 1
")
         (program (parse-test-il source))
         (networks (mblogic-cl:program-main-networks program)))
    ;; Should handle gracefully even if empty
    (is (listp networks))))

(test multiple-outputs-per-rung
  "Test rung with multiple coil outputs"
  (let* ((source "NETWORK 1
STR X1
OUT Y1 Y2 Y3 Y4
")
         (program (parse-test-il source))
         (network (first (mblogic-cl:program-main-networks program)))
         (rung (mblogic-cl-web::network-to-ladder-rung network)))
    (let ((addrs (mblogic-cl-web::ladder-rung-addresses rung)))
      (is (member "Y1" addrs :test #'string=))
      (is (member "Y2" addrs :test #'string=))
      (is (member "Y3" addrs :test #'string=))
      (is (member "Y4" addrs :test #'string=)))))

(test comparison-instruction-addresses
  "Test address extraction from comparison instructions"
  (let ((instr (make-test-instruction "STRE" "DS1" "DS2")))
    (let ((addrs (mblogic-cl-web::extract-addresses instr)))
      (is (member "DS1" addrs :test #'string=))
      (is (member "DS2" addrs :test #'string=)))))

;;; ============================================================
;;; Ladder Structure Validation Tests
;;; ============================================================

(test ladder-cell-structure
  "Test ladder-cell struct has all required fields"
  (let ((cell (mblogic-cl-web::make-ladder-cell
               :type :contact
               :symbol "noc"
               :address "X1"
               :opcode "STR"
               :row 0
               :col 0)))
    (is (eq :contact (mblogic-cl-web::ladder-cell-type cell)))
    (is (string= "noc" (mblogic-cl-web::ladder-cell-symbol cell)))
    (is (string= "X1" (mblogic-cl-web::ladder-cell-address cell)))
    (is (string= "STR" (mblogic-cl-web::ladder-cell-opcode cell)))
    (is (= 0 (mblogic-cl-web::ladder-cell-row cell)))
    (is (= 0 (mblogic-cl-web::ladder-cell-col cell)))))

(test ladder-rung-structure
  "Test ladder-rung struct has all required fields"
  (let ((rung (mblogic-cl-web::make-ladder-rung
               :number 1
               :cells nil
               :rows 1
               :cols 0)))
    (is (= 1 (mblogic-cl-web::ladder-rung-number rung)))
    (is (null (mblogic-cl-web::ladder-rung-cells rung)))
    (is (= 1 (mblogic-cl-web::ladder-rung-rows rung)))
    (is (= 0 (mblogic-cl-web::ladder-rung-cols rung)))))

(test ladder-program-structure
  "Test ladder-program struct has all required fields"
  (let ((prog (mblogic-cl-web::make-ladder-program
               :name "test"
               :rungs nil
               :addresses nil)))
    (is (string= "test" (mblogic-cl-web::ladder-program-name prog)))
    (is (null (mblogic-cl-web::ladder-program-rungs prog)))
    (is (null (mblogic-cl-web::ladder-program-addresses prog)))))

;;; End of test-ld-visualization.lisp
