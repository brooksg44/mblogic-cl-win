;;;; test-phase2.lisp - Phase 2 Instruction Definitions testing

(ql:quickload :mblogic-cl)
(in-package :mblogic-cl)

(format t "~%=== Phase 2 Testing: Instruction Definitions ===~%~%")

;; Test instruction count
(format t "Total instructions registered: ~D~%~%" (instruction-count))

;; Print instruction summary by class
(print-instruction-summary)

;; Test finding specific instructions
(format t "~%=== Individual Instruction Tests ===~%")

(let ((str-instr (find-instruction "STR")))
  (format t "~%STR instruction:~%")
  (format t "  Opcode: ~A~%" (instruction-opcode str-instr))
  (format t "  Description: ~A~%" (instruction-description str-instr))
  (format t "  Type: ~A~%" (instruction-type str-instr))
  (format t "  Class: ~A~%" (instruction-class str-instr))
  (format t "  Min params: ~D~%" (instruction-min-params str-instr))
  (format t "  Max params: ~D~%" (instruction-max-params str-instr)))

(let ((tmr-instr (find-instruction "TMR")))
  (format t "~%TMR instruction:~%")
  (format t "  Opcode: ~A~%" (instruction-opcode tmr-instr))
  (format t "  Description: ~A~%" (instruction-description tmr-instr))
  (format t "  Type: ~A~%" (instruction-type tmr-instr))
  (format t "  Class: ~A~%" (instruction-class tmr-instr))
  (format t "  Min params: ~D~%" (instruction-min-params tmr-instr))
  (format t "  Max params: ~D~%" (instruction-max-params tmr-instr)))

(let ((mathdec-instr (find-instruction "MATHDEC")))
  (format t "~%MATHDEC instruction:~%")
  (format t "  Opcode: ~A~%" (instruction-opcode mathdec-instr))
  (format t "  Description: ~A~%" (instruction-description mathdec-instr))
  (format t "  Type: ~A~%" (instruction-type mathdec-instr))
  (format t "  Class: ~A~%" (instruction-class mathdec-instr)))

;; Test validation
(format t "~%=== Validation Tests ===~%")

(let ((str-instr (find-instruction "STR")))
  (multiple-value-bind (valid error-msg)
      (validate-instruction str-instr '("X1"))
    (format t "STR X1 validation: ~A~%" (if valid "PASS" error-msg)))

  (multiple-value-bind (valid error-msg)
      (validate-instruction str-instr '())
    (format t "STR (no params) validation: ~A~%" (if valid "PASS" error-msg)))

  (multiple-value-bind (valid error-msg)
      (validate-instruction str-instr '("X1" "X2"))
    (format t "STR X1 X2 validation: ~A~%" (if valid "PASS" error-msg))))

(let ((out-instr (find-instruction "OUT")))
  (multiple-value-bind (valid error-msg)
      (validate-instruction out-instr '("Y1" "Y2" "Y3"))
    (format t "OUT Y1 Y2 Y3 validation: ~A~%" (if valid "PASS" error-msg))))

;; Test address validators
(format t "~%=== Address Validator Tests ===~%")
(format t "bool-addr-p \"X1\": ~A~%" (bool-addr-p "X1"))
(format t "bool-addr-p \"Y2000\": ~A~%" (bool-addr-p "Y2000"))
(format t "bool-addr-p \"SC100\": ~A~%" (bool-addr-p "SC100"))
(format t "bool-addr-p \"DS100\": ~A~%" (bool-addr-p "DS100"))

(format t "word-addr-p \"DS100\": ~A~%" (word-addr-p "DS100"))
(format t "word-addr-p \"DD1\": ~A~%" (word-addr-p "DD1"))
(format t "word-addr-p \"CTD5\": ~A~%" (word-addr-p "CTD5"))
(format t "word-addr-p \"X1\": ~A~%" (word-addr-p "X1"))

(format t "float-addr-p \"DF100\": ~A~%" (float-addr-p "DF100"))
(format t "string-addr-p \"TXT1\": ~A~%" (string-addr-p "TXT1"))
(format t "any-addr-p \"X1\": ~A~%" (any-addr-p "X1"))
(format t "any-addr-p \"DS100\": ~A~%" (any-addr-p "DS100"))
(format t "any-addr-p \"INVALID\": ~A~%" (any-addr-p "INVALID"))

(format t "numeric-p \"123\": ~A~%" (numeric-p "123"))
(format t "numeric-p \"-456\": ~A~%" (numeric-p "-456"))
(format t "numeric-p \"3.14\": ~A~%" (numeric-p "3.14"))
(format t "hex-constant-p \"FFh\": ~A~%" (hex-constant-p "FFh"))
(format t "time-unit-p \"ms\": ~A~%" (time-unit-p "ms"))
(format t "time-unit-p \"sec\": ~A~%" (time-unit-p "sec"))

;; Verify all instructions from sample program exist
(format t "~%=== Sample Program Instruction Coverage ===~%")
(let ((sample-opcodes '("STR" "STRN" "AND" "ANDN" "OR" "ORN" "ANDSTR" "ORSTR"
                        "OUT" "SET" "RST" "PD"
                        "STRPD" "STRND" "ANDPD" "ANDND" "ORPD" "ORND"
                        "STRE" "STRNE" "STRGT" "STRLT" "STRGE" "STRLE"
                        "ANDE" "ANDNE" "ANDGT" "ANDLT" "ANDGE" "ANDLE"
                        "ORE" "ORNE" "ORGT" "ORLT" "ORGE" "ORLE"
                        "TMR" "TMRA" "TMROFF"
                        "CNTU" "CNTD" "UDC"
                        "COPY" "CPYBLK" "FILL" "PACK" "UNPACK"
                        "MATHDEC" "MATHHEX" "SUM"
                        "FINDEQ" "FINDNE" "FINDGT" "FINDLT" "FINDGE" "FINDLE"
                        "FINDIEQ" "FINDINE" "FINDIGT" "FINDILT" "FINDIGE" "FINDILE"
                        "CALL" "RT" "RTC" "END" "ENDC" "FOR" "NEXT"
                        "NETWORK" "SBR" "SHFRG"))
      (missing nil))
  (dolist (op sample-opcodes)
    (unless (find-instruction op)
      (push op missing)))
  (if missing
      (format t "MISSING instructions: ~{~A~^, ~}~%" missing)
      (format t "All ~D sample program instructions found!~%" (length sample-opcodes))))

(format t "~%=== Phase 2 Testing Complete ===~%")
