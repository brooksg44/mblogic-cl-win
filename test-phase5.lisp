;;;; test-phase5.lisp - Phase 5 Interpreter testing

(ql:quickload :mblogic-cl)
(in-package :mblogic-cl)

(format t "~%=== Phase 5 Testing: Runtime Interpreter ===~%~%")

;; Test basic interpreter creation and execution
(format t "=== Basic Interpreter Test ===~%")
(let* ((source "NETWORK 1
STR X1
OUT Y1

NETWORK 2
STR X2
AND X3
OUT Y2
")
       (interp (test-program source
                             :scans 1
                             :inputs '(("X1" . t) ("X2" . t) ("X3" . t)))))

  (format t "After 1 scan:~%")
  (format t "  Y1=~A (expected T)~%" (get-bool-value interp "Y1"))
  (format t "  Y2=~A (expected T)~%" (get-bool-value interp "Y2"))
  (print-interpreter-status interp))

;; Test multiple scans
(format t "~%=== Multiple Scans Test ===~%")
(let* ((source "NETWORK 1
STR SC1
COPY DS100 DS101

NETWORK 2
STR SC1
STRE DS101 0
STRN DS101 0
ANDSTR
OUT C1
")
       (compiled (compile-il-string source))
       (interp (make-plc-interpreter :program compiled)))

  ;; Set initial value
  (set-word-value interp "DS100" 42)

  ;; Run 5 scans
  (run-continuous interp :max-scans 5)

  (format t "After 5 scans:~%")
  (format t "  DS101=~A (expected 42, copied from DS100)~%"
          (get-word-value interp "DS101"))
  (format t "  Scan count: ~D~%" (interpreter-scan-count interp))
  (print-interpreter-status interp))

;; Test system control bits
(format t "~%=== System Control Bits Test ===~%")
(let* ((source "NETWORK 1
// SC1 should always be true
STR SC1
OUT Y1

NETWORK 2
// SC5 is first scan only
STR SC5
OUT Y5
")
       (compiled (compile-il-string source))
       (interp (make-plc-interpreter :program compiled)))

  ;; Run first scan
  (run-continuous interp :max-scans 1)
  (format t "After scan 1:~%")
  (format t "  SC1=~A (expected T - always on)~%" (get-bool-value interp "SC1"))
  (format t "  SC5=~A (expected T - first scan)~%" (get-bool-value interp "SC5"))
  (format t "  Y1=~A Y5=~A~%" (get-bool-value interp "Y1") (get-bool-value interp "Y5"))

  ;; Reset and run second scan
  (set-bool-value interp "Y5" nil)
  (step-scan interp)
  (format t "After scan 2:~%")
  (format t "  SC5=~A (expected NIL - not first scan)~%" (get-bool-value interp "SC5"))
  (format t "  Y5=~A (expected NIL)~%" (get-bool-value interp "Y5")))

;; Test quick-test utility
(format t "~%=== Quick Test Utility ===~%")
(let ((result (quick-test
               "NETWORK 1
STR X1
OUT Y1

NETWORK 2
STRE DS100 100
OUT Y2
"
               '(("X1" . t) ("DS100" . 100))  ; inputs
               '(("Y1" . t) ("Y2" . t)))))     ; expected outputs
  (format t "quick-test result: ~A (expected T)~%" result))

;; Test with different inputs
(let ((result (quick-test
               "NETWORK 1
STR X1
OUT Y1
"
               '(("X1" . nil))
               '(("Y1" . nil)))))
  (format t "quick-test with X1=NIL: ~A (expected T)~%" result))

;; Test statistics tracking
(format t "~%=== Statistics Tracking Test ===~%")
(let* ((source "NETWORK 1
STR SC1
COPY DS100 DS101
")
       (compiled (compile-il-string source))
       (interp (make-plc-interpreter :program compiled)))

  (run-continuous interp :max-scans 100)

  (let ((stats (interpreter-statistics interp)))
    (format t "After 100 scans:~%")
    (format t "  Total scans: ~D~%" (stats-total-scans stats))
    (format t "  Average scan time: ~,3F ms~%" (average-scan-time stats))
    (format t "  Min scan time: ~,3F ms~%" (stats-min-scan-time stats))
    (format t "  Max scan time: ~,3F ms~%" (stats-max-scan-time stats))))

;; Test run-il-string convenience function
(format t "~%=== Convenience Functions Test ===~%")
(let ((interp (run-il-string "NETWORK 1
STR SC1
OUT Y1
"
                             :max-scans 1)))
  (format t "run-il-string result:~%")
  (format t "  Y1=~A (expected T)~%" (get-bool-value interp "Y1"))
  (format t "  Exit code: ~A~%" (interpreter-exit-code interp)))

;; Test END instruction
(format t "~%=== END Instruction Test ===~%")
(let* ((source "NETWORK 1
STR SC1
OUT Y1

NETWORK 2
END

NETWORK 3
STR SC1
OUT Y2
")
       (interp (run-il-string source :max-scans 1)))
  (format t "Program with END:~%")
  (format t "  Y1=~A (expected T - before END)~%" (get-bool-value interp "Y1"))
  (format t "  Y2=~A (expected NIL - after END, not executed)~%" (get-bool-value interp "Y2"))
  (format t "  Exit code: ~A~%" (interpreter-exit-code interp)))

;; Test step-scan for debugging
(format t "~%=== Step Scan (Debug) Test ===~%")
(let* ((source "NETWORK 1
STR SC3
OUT Y1
")
       (compiled (compile-il-string source))
       (interp (make-plc-interpreter :program compiled)))

  (format t "Stepping through scans:~%")
  (dotimes (i 4)
    (step-scan interp)
    (format t "  Scan ~D: SC3=~A Y1=~A~%"
            (interpreter-scan-count interp)
            (get-bool-value interp "SC3")
            (get-bool-value interp "Y1"))))

;; Test edge detection across scans
(format t "~%=== Edge Detection Across Scans ===~%")
(let* ((source "NETWORK 1
STRPD X1
OUT Y1
")
       (compiled (compile-il-string source))
       (interp (make-plc-interpreter :program compiled)))

  ;; Scan 1: X1 starts NIL
  (set-bool-value interp "X1" nil)
  (step-scan interp)
  (format t "Scan 1 (X1=NIL): Y1=~A (expected NIL)~%" (get-bool-value interp "Y1"))

  ;; Scan 2: X1 goes to T (rising edge!)
  (set-bool-value interp "X1" t)
  (step-scan interp)
  (format t "Scan 2 (X1=T, rising edge): Y1=~A (expected T)~%" (get-bool-value interp "Y1"))

  ;; Scan 3: X1 stays T (no edge)
  (step-scan interp)
  (format t "Scan 3 (X1=T, no edge): Y1=~A (expected NIL)~%" (get-bool-value interp "Y1"))

  ;; Scan 4: X1 goes to NIL
  (set-bool-value interp "X1" nil)
  (step-scan interp)
  (format t "Scan 4 (X1=NIL): Y1=~A (expected NIL)~%" (get-bool-value interp "Y1"))

  ;; Scan 5: X1 goes to T again (another rising edge!)
  (set-bool-value interp "X1" t)
  (step-scan interp)
  (format t "Scan 5 (X1=T, rising edge): Y1=~A (expected T)~%" (get-bool-value interp "Y1")))

;; Test subroutine execution
(format t "~%=== Subroutine Execution Test ===~%")
(let* ((source "NETWORK 1
STR SC1
CALL DoWork

NETWORK 2
END

SBR DoWork
NETWORK 1
STR SC1
COPY 12345 DS100
RT
")
       (interp (run-il-string source :max-scans 1)))
  (format t "After calling subroutine:~%")
  (format t "  DS100=~A (expected 12345)~%" (get-word-value interp "DS100")))

;; Performance test
(format t "~%=== Performance Test (1000 scans) ===~%")
(let* ((source "NETWORK 1
STR SC1
AND SC1
OR SC2
OUT C1

NETWORK 2
STR C1
OUT Y1
")
       (compiled (compile-il-string source))
       (interp (make-plc-interpreter :program compiled))
       (start-time (get-internal-real-time)))

  (run-continuous interp :max-scans 1000)

  (let* ((end-time (get-internal-real-time))
         (total-ms (* 1000.0 (/ (- end-time start-time) internal-time-units-per-second))))
    (format t "1000 scans completed in ~,1F ms~%" total-ms)
    (format t "Average: ~,3F ms/scan~%" (/ total-ms 1000))
    (print-interpreter-status interp)))

(format t "~%=== Phase 5 Testing Complete ===~%")
