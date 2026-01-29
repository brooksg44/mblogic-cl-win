;;;; test-phase3.lisp - Phase 3 Parser testing

(ql:quickload :mblogic-cl)
(in-package :mblogic-cl)

(format t "~%=== Phase 3 Testing: IL Parser ===~%~%")

;; Test tokenization
(format t "=== Tokenization Tests ===~%")
(format t "tokenize \"STR X1\": ~S~%" (tokenize-line "STR X1"))
(format t "tokenize \"OUT Y1 Y2 Y3\": ~S~%" (tokenize-line "OUT Y1 Y2 Y3"))
(format t "tokenize \"TMR T5 5000 ms\": ~S~%" (tokenize-line "TMR T5 5000 ms"))
(format t "tokenize \"MATHDEC DS11 0 100 - DS10\": ~S~%" (tokenize-line "MATHDEC DS11 0 100 - DS10"))
(format t "tokenize with parens: ~S~%" (tokenize-line "MATHDEC DF1 0 (1 + DS2125) ^ 2"))
(format t "tokenize with quotes: ~S~%" (tokenize-line "FINDGT \"c\" TXT100 TXT110 DS2115 C113"))

;; Test helper functions
(format t "~%=== Helper Function Tests ===~%")
(format t "comment-line-p \"// test\": ~A~%" (comment-line-p "// test"))
(format t "comment-line-p \"STR X1\": ~A~%" (comment-line-p "STR X1"))
(format t "blank-line-p \"\": ~A~%" (blank-line-p ""))
(format t "blank-line-p \"   \": ~A~%" (blank-line-p "   "))
(format t "extract-comment \"// hello world\": ~S~%" (extract-comment "// hello world"))

;; Test simple parsing
(format t "~%=== Simple Parse Test ===~%")
(let ((simple-program "NETWORK 1
STR X1
OUT Y1

NETWORK 2
STR X2
AND X3
OUT Y2
"))
  (let ((result (parse-il-string simple-program)))
    (format t "Parsed ~D main networks~%" (length (program-main-networks result)))
    (format t "Errors: ~D~%" (length (program-errors result)))
    (format t "Warnings: ~D~%" (length (program-warnings result)))
    (dolist (net (program-main-networks result))
      (format t "  Network ~D: ~D instructions~%"
              (network-number net)
              (length (network-instructions net)))
      (dolist (instr (network-instructions net))
        (format t "    ~A~{ ~A~}~%" (parsed-opcode instr) (parsed-params instr))))))

;; Test parsing the full sample program
(format t "~%=== Full Sample Program Parse Test ===~%")
;;(let* ((filepath "/home/brooksg44/quicklisp/local-projects/mblogic-cl/test/plcprog.txt")
(let* ((filepath "d:/common-lisp/mblogic-cl/test/plcprog.txt")
       (result (parse-il-file filepath)))

  ;; Print summary
  (print-parsed-program result)

  ;; Count instructions
  (format t "~%Total instructions: ~D~%" (count-instructions result))

  ;; Check for specific content
  (format t "~%=== Content Verification ===~%")

  ;; Check main program networks
  (format t "Main networks: ~D~%" (length (program-main-networks result)))

  ;; Check subroutines
  (let ((sbr-table (program-subroutines result)))
    (format t "Subroutines found: ")
    (maphash (lambda (name sbr)
               (declare (ignore sbr))
               (format t "~A " name))
             sbr-table)
    (format t "~%")

    ;; Check specific subroutine
    (let ((tank-sim (gethash "TankSim" sbr-table)))
      (when tank-sim
        (format t "~%TankSim subroutine has ~D networks~%"
                (length (subroutine-networks tank-sim))))))

  ;; Show first few instructions of main program
  (format t "~%=== First Network Detail ===~%")
  (let ((first-net (first (program-main-networks result))))
    (when first-net
      (print-network-detail first-net)))

  ;; Show a network with MATHDEC
  (format t "~%=== Network with MATHDEC ===~%")
  (dolist (net (program-main-networks result))
    (dolist (instr (network-instructions net))
      (when (string= (parsed-opcode instr) "MATHDEC")
        (format t "Found MATHDEC at line ~D:~%" (parsed-line-number instr))
        (format t "  Params: ~S~%" (parsed-params instr))
        (return-from nil)))))

;; Test error handling
(format t "~%=== Error Handling Test ===~%")
(let ((bad-program "NETWORK 1
STR X1
BADINSTRUCTION Y1
OUT Y2
"))
  (let ((result (parse-il-string bad-program)))
    (format t "Errors found: ~D~%" (length (program-errors result)))
    (dolist (err (program-errors result))
      (format t "  Line ~D: ~A~%"
              (parse-error-line err)
              (parse-error-message err)))))

;; Test with timer instruction
(format t "~%=== Timer Instruction Parse ===~%")
(let ((timer-program "NETWORK 1
STR C1
TMR T5 5000 ms

NETWORK 2
STR C2
TMRA T6 100 sec
"))
  (let ((result (parse-il-string timer-program)))
    (dolist (net (program-main-networks result))
      (dolist (instr (network-instructions net))
        (when (member (parsed-opcode instr) '("TMR" "TMRA") :test #'string=)
          (format t "~A params: ~S~%" (parsed-opcode instr) (parsed-params instr)))))))

(format t "~%=== Phase 3 Testing Complete ===~%")
