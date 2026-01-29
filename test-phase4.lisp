;;;; test-phase4.lisp - Phase 4 Compiler testing

(ql:quickload :mblogic-cl)
(in-package :mblogic-cl)

(format t "~%=== Phase 4 Testing: IL Compiler ===~%~%")

;; Test code generation for simple instructions
(format t "=== Code Generation Tests ===~%")

(format t "~%--- Simple Boolean Logic ---~%")
(show-generated-code "NETWORK 1
STR X1
OUT Y1

NETWORK 2
STR X2
AND X3
OUT Y2
")

(format t "~%--- Comparison Instructions ---~%")
(show-generated-code "NETWORK 1
STRE DS100 50
OUT Y1

NETWORK 2
STRGT DS100 100
OUT Y2
")

;; Test compilation of a simple program
(format t "~%=== Simple Program Compilation Test ===~%")
(let* ((simple-program "NETWORK 1
STR X1
OUT Y1

NETWORK 2
STR X2
AND X3
OUT Y2
")
       (compiled (compile-il-string simple-program)))
  (format t "Compilation successful!~%")
  (format t "Main function: ~A~%" (program-main-function compiled))
  (format t "Subroutines: ~D~%" (hash-table-count (program-compiled-subroutines compiled))))

;; Test execution of simple program
(format t "~%=== Simple Program Execution Test ===~%")
(let* ((simple-program "NETWORK 1
STR X1
OUT Y1

NETWORK 2
STR X2
AND X3
OUT Y2
")
       (compiled (compile-il-string simple-program))
       (dt (make-data-table)))

  ;; Initialize data table
  (init-data-table dt)

  ;; Set input values
  (set-bool dt "X1" t)
  (set-bool dt "X2" t)
  (set-bool dt "X3" nil)

  (format t "Before execution:~%")
  (format t "  X1=~A X2=~A X3=~A~%" (get-bool dt "X1") (get-bool dt "X2") (get-bool dt "X3"))
  (format t "  Y1=~A Y2=~A~%" (get-bool dt "Y1") (get-bool dt "Y2"))

  ;; Execute the compiled program
  (funcall (program-main-function compiled)
           dt 100 (program-compiled-subroutines compiled))

  (format t "After execution:~%")
  (format t "  Y1=~A (expected T)~%" (get-bool dt "Y1"))
  (format t "  Y2=~A (expected NIL because X3=NIL)~%" (get-bool dt "Y2"))

  ;; Test with different inputs
  (set-bool dt "X3" t)
  (funcall (program-main-function compiled)
           dt 100 (program-compiled-subroutines compiled))
  (format t "After X3=T:~%")
  (format t "  Y2=~A (expected T)~%" (get-bool dt "Y2")))

;; Test SET/RST instructions
(format t "~%=== SET/RST Test ===~%")
(let* ((setrst-program "NETWORK 1
STR X1
SET C1

NETWORK 2
STR X2
RST C1
")
       (compiled (compile-il-string setrst-program))
       (dt (make-data-table)))

  (init-data-table dt)

  ;; Test SET
  (set-bool dt "X1" t)
  (set-bool dt "X2" nil)
  (funcall (program-main-function compiled)
           dt 100 (program-compiled-subroutines compiled))
  (format t "After X1=T, X2=NIL: C1=~A (expected T - SET)~%" (get-bool dt "C1"))

  ;; Test RST
  (set-bool dt "X1" nil)
  (set-bool dt "X2" t)
  (funcall (program-main-function compiled)
           dt 100 (program-compiled-subroutines compiled))
  (format t "After X1=NIL, X2=T: C1=~A (expected NIL - RST)~%" (get-bool dt "C1")))

;; Test comparison instructions
(format t "~%=== Comparison Instructions Test ===~%")
(let* ((compare-program "NETWORK 1
STRE DS100 50
OUT Y1

NETWORK 2
STRGT DS100 50
OUT Y2

NETWORK 3
STRLT DS100 50
OUT Y3
")
       (compiled (compile-il-string compare-program))
       (dt (make-data-table)))

  (init-data-table dt)
  (set-word dt "DS100" 50)

  (funcall (program-main-function compiled)
           dt 100 (program-compiled-subroutines compiled))

  (format t "DS100=50:~%")
  (format t "  STRE 50 => Y1=~A (expected T)~%" (get-bool dt "Y1"))
  (format t "  STRGT 50 => Y2=~A (expected NIL)~%" (get-bool dt "Y2"))
  (format t "  STRLT 50 => Y3=~A (expected NIL)~%" (get-bool dt "Y3"))

  ;; Test with DS100=75
  (set-word dt "DS100" 75)
  (funcall (program-main-function compiled)
           dt 100 (program-compiled-subroutines compiled))

  (format t "DS100=75:~%")
  (format t "  STRE 50 => Y1=~A (expected NIL)~%" (get-bool dt "Y1"))
  (format t "  STRGT 50 => Y2=~A (expected T)~%" (get-bool dt "Y2"))
  (format t "  STRLT 50 => Y3=~A (expected NIL)~%" (get-bool dt "Y3")))

;; Test COPY instruction
(format t "~%=== COPY Instruction Test ===~%")
(let* ((copy-program "NETWORK 1
STR SC1
COPY DS100 DS200
")
       (compiled (compile-il-string copy-program))
       (dt (make-data-table)))

  (init-data-table dt)
  (set-bool dt "SC1" t)  ; Enable the copy
  (set-word dt "DS100" 12345)
  (set-word dt "DS200" 0)

  (format t "Before: DS100=~A DS200=~A~%" (get-word dt "DS100") (get-word dt "DS200"))
  (funcall (program-main-function compiled)
           dt 100 (program-compiled-subroutines compiled))
  (format t "After COPY: DS200=~A (expected 12345)~%" (get-word dt "DS200")))

;; Test edge detection (STRPD)
(format t "~%=== Edge Detection Test ===~%")
(let* ((edge-program "NETWORK 1
STRPD X1
OUT Y1
")
       (compiled (compile-il-string edge-program))
       (dt (make-data-table)))

  (init-data-table dt)

  ;; First scan - X1 goes from nil to nil (no edge)
  (set-bool dt "X1" nil)
  (funcall (program-main-function compiled)
           dt 100 (program-compiled-subroutines compiled))
  (format t "Scan 1 (X1=NIL): Y1=~A (expected NIL)~%" (get-bool dt "Y1"))

  ;; Second scan - X1 goes from nil to t (rising edge!)
  (set-bool dt "X1" t)
  (funcall (program-main-function compiled)
           dt 100 (program-compiled-subroutines compiled))
  (format t "Scan 2 (X1=NIL->T): Y1=~A (expected T - rising edge)~%" (get-bool dt "Y1"))

  ;; Third scan - X1 stays t (no edge)
  (funcall (program-main-function compiled)
           dt 100 (program-compiled-subroutines compiled))
  (format t "Scan 3 (X1=T->T): Y1=~A (expected NIL - no edge)~%" (get-bool dt "Y1")))

;; Test subroutine call
(format t "~%=== Subroutine Call Test ===~%")
(let* ((sbr-program "NETWORK 1
STR SC1
CALL MySub

SBR MySub
NETWORK 1
STR X1
OUT Y1
NETWORK 2
RT
")
       (compiled (compile-il-string sbr-program))
       (dt (make-data-table)))

  (init-data-table dt)
  (set-bool dt "SC1" t)  ; Enable call
  (set-bool dt "X1" t)

  (format t "Before: Y1=~A~%" (get-bool dt "Y1"))
  (funcall (program-main-function compiled)
           dt 100 (program-compiled-subroutines compiled))
  (format t "After CALL MySub: Y1=~A (expected T)~%" (get-bool dt "Y1")))

;; Test END instruction
(format t "~%=== END/ENDC Test ===~%")
(let* ((end-program "NETWORK 1
STR X1
OUT Y1

NETWORK 2
STRN SC1
ENDC

NETWORK 3
STR X2
OUT Y2
")
       (compiled (compile-il-string end-program))
       (dt (make-data-table)))

  (init-data-table dt)
  (set-bool dt "X1" t)
  (set-bool dt "X2" t)
  (set-bool dt "SC1" t)  ; ENDC condition false

  (funcall (program-main-function compiled)
           dt 100 (program-compiled-subroutines compiled))
  (format t "SC1=T (ENDC false): Y1=~A Y2=~A (both T)~%"
          (get-bool dt "Y1") (get-bool dt "Y2"))

  ;; Reset outputs
  (set-bool dt "Y1" nil)
  (set-bool dt "Y2" nil)
  (set-bool dt "SC1" nil)  ; ENDC condition true

  (funcall (program-main-function compiled)
           dt 100 (program-compiled-subroutines compiled))
  (format t "SC1=NIL (ENDC true): Y1=~A Y2=~A (Y1=T, Y2=NIL - stopped early)~%"
          (get-bool dt "Y1") (get-bool dt "Y2")))

(format t "~%=== Phase 4 Testing Complete ===~%")
