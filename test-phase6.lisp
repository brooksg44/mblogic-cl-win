;;;; test-phase6.lisp - Phase 6 Standard Libraries Testing

(ql:quickload :mblogic-cl)
(in-package :mblogic-cl)

(format t "~%=== Phase 6 Testing: Standard Libraries ===~%~%")

;;; ============================================================
;;; Helper Functions
;;; ============================================================

(defun make-test-dt ()
  "Create and initialize a test data table"
  (let ((dt (make-data-table)))
    (init-data-table dt)
    dt))

;;; ============================================================
;;; Math Library Tests
;;; ============================================================

(format t "=== Math Library Tests ===~%")

;; Test basic MATHDEC operations
(let ((dt (make-test-dt)))
  (format t "~%-- MATHDEC Basic Operations --~%")

  ;; Test simple addition
  (set-word dt "DS10" 100)
  (set-word dt "DS11" 50)
  (mathdec dt "DS100" nil "DS10 + DS11")
  (format t "DS10 + DS11 = ~A (expected 150)~%" (get-word dt "DS100"))

  ;; Test multiplication
  (mathdec dt "DS101" nil "DS10 * 2")
  (format t "DS10 * 2 = ~A (expected 200)~%" (get-word dt "DS101"))

  ;; Test complex expression
  (mathdec dt "DS102" nil "(DS10 + DS11) * 2")
  (format t "(DS10 + DS11) * 2 = ~A (expected 300)~%" (get-word dt "DS102"))

  ;; Test division
  (mathdec dt "DS103" nil "DS10 / 4")
  (format t "DS10 / 4 = ~A (expected 25)~%" (get-word dt "DS103"))

  ;; Test modulo
  (mathdec dt "DS104" nil "DS10 % 30")
  (format t "DS10 %% 30 = ~A (expected 10)~%" (get-word dt "DS104")))

;; Test MATHDEC functions
(let ((dt (make-test-dt)))
  (format t "~%-- MATHDEC Functions --~%")

  ;; Test SQRT
  (mathdec dt "DS100" nil "SQRT(144)")
  (format t "SQRT(144) = ~A (expected 12)~%" (get-word dt "DS100"))

  ;; Test ABS
  (set-word dt "DS10" -50)
  (mathdec dt "DS101" nil "ABS(DS10)")
  (format t "ABS(-50) = ~A (expected 50)~%" (get-word dt "DS101"))

  ;; Test POW
  (mathdec dt "DS102" nil "POW(2, 10)")
  (format t "POW(2, 10) = ~A (expected 1024)~%" (get-word dt "DS102"))

  ;; Test MIN/MAX
  (set-word dt "DS10" 30)
  (set-word dt "DS11" 50)
  (mathdec dt "DS103" nil "MIN(DS10, DS11)")
  (format t "MIN(30, 50) = ~A (expected 30)~%" (get-word dt "DS103"))
  (mathdec dt "DS104" nil "MAX(DS10, DS11)")
  (format t "MAX(30, 50) = ~A (expected 50)~%" (get-word dt "DS104")))

;; Test MATHDEC with floats
(let ((dt (make-test-dt)))
  (format t "~%-- MATHDEC with Floats --~%")

  ;; Test trigonometric functions
  (mathdec dt "DF100" nil "SIN(0)")
  (format t "SIN(0) = ~A (expected 0.0)~%" (get-float dt "DF100"))

  (mathdec dt "DF101" nil "COS(0)")
  (format t "COS(0) = ~A (expected 1.0)~%" (get-float dt "DF101"))

  ;; Test PI constant
  (mathdec dt "DF102" nil "PI")
  (format t "PI = ~A (expected ~A)~%" (get-float dt "DF102") pi))

;; Test MATHHEX operations
(let ((dt (make-test-dt)))
  (format t "~%-- MATHHEX Operations --~%")

  ;; Test bitwise operations
  (set-word dt "DH10" #xFF00)
  (set-word dt "DH11" #x00FF)

  (mathhex dt "DH100" nil "DH10 AND DH11")
  (format t "0xFF00 AND 0x00FF = ~X (expected 0)~%" (get-word dt "DH100"))

  (mathhex dt "DH101" nil "DH10 OR DH11")
  (format t "0xFF00 OR 0x00FF = ~X (expected FFFF)~%" (get-word dt "DH101"))

  (mathhex dt "DH102" nil "DH10 XOR DH11")
  (format t "0xFF00 XOR 0x00FF = ~X (expected FFFF)~%" (get-word dt "DH102")))

;; Test BCD conversions
(let ((dt (make-test-dt)))
  (format t "~%-- BCD Conversions --~%")

  (format t "BCD 0x1234 to int = ~D (expected 1234)~%" (bcd-to-int #x1234))
  (format t "Int 1234 to BCD = ~X (expected 1234)~%" (int-to-bcd 1234))
  (format t "BCD 0x9876 to int = ~D (expected 9876)~%" (bcd-to-int #x9876)))

;;; ============================================================
;;; Timer/Counter Tests
;;; ============================================================

(format t "~%=== Timer/Counter Tests ===~%")

;; Test TMR - On-delay timer
(let ((dt (make-test-dt)))
  (format t "~%-- TMR (On-Delay Timer) --~%")

  ;; Timer disabled - should not accumulate
  (tmr-execute dt "T1" nil 1000 100)
  (format t "Timer disabled: T1=~A TD1=~A (expected NIL, 0)~%"
          (get-bool dt "T1") (get-word dt "TD1"))

  ;; Timer enabled - accumulate time
  (tmr-execute dt "T1" t 1000 100)
  (format t "After 100ms: T1=~A TD1=~A (expected NIL, 100)~%"
          (get-bool dt "T1") (get-word dt "TD1"))

  ;; Continue accumulating
  (dotimes (i 10)
    (tmr-execute dt "T1" t 1000 100))
  (format t "After 1100ms: T1=~A TD1=~A (expected T, 1000)~%"
          (get-bool dt "T1") (get-word dt "TD1"))

  ;; Disable resets timer
  (tmr-execute dt "T1" nil 1000 100)
  (format t "Timer disabled: T1=~A TD1=~A (expected NIL, 0)~%"
          (get-bool dt "T1") (get-word dt "TD1")))

;; Test TMRA - Accumulating timer
(let ((dt (make-test-dt)))
  (format t "~%-- TMRA (Accumulating Timer) --~%")

  ;; Accumulate some time
  (dotimes (i 5)
    (tmra-execute dt "T2" t nil 1000 100))
  (format t "After 500ms: T2=~A TD2=~A~%" (get-bool dt "T2") (get-word dt "TD2"))

  ;; Disable - should retain time
  (tmra-execute dt "T2" nil nil 1000 100)
  (format t "Disabled (retained): T2=~A TD2=~A~%" (get-bool dt "T2") (get-word dt "TD2"))

  ;; Continue accumulating
  (dotimes (i 6)
    (tmra-execute dt "T2" t nil 1000 100))
  (format t "After more: T2=~A TD2=~A (expected T, 1000)~%"
          (get-bool dt "T2") (get-word dt "TD2"))

  ;; Reset clears timer
  (tmra-execute dt "T2" nil t 1000 100)
  (format t "After reset: T2=~A TD2=~A (expected NIL, 0)~%"
          (get-bool dt "T2") (get-word dt "TD2")))

;; Test TMROFF - Off-delay timer
(let ((dt (make-test-dt)))
  (format t "~%-- TMROFF (Off-Delay Timer) --~%")

  ;; Enable - output immediately on
  (tmroff-execute dt "T3" t 1000 100)
  (format t "Enabled: T3=~A (expected T)~%" (get-bool dt "T3"))

  ;; Disable - output stays on while timing
  (tmroff-execute dt "T3" nil 1000 100)
  (format t "Just disabled: T3=~A (expected T)~%" (get-bool dt "T3"))

  ;; Continue timing (after 9 more scans = 1000ms total, timer expires)
  (dotimes (i 8)
    (tmroff-execute dt "T3" nil 1000 100))
  (format t "After 900ms delay: T3=~A (expected T, still timing)~%" (get-bool dt "T3"))

  ;; Timer expires at 1000ms
  (tmroff-execute dt "T3" nil 1000 100)
  (format t "After 1000ms (timer expired): T3=~A (expected NIL)~%" (get-bool dt "T3")))

;; Test CNTU - Up counter
(let ((dt (make-test-dt)))
  (format t "~%-- CNTU (Up Counter) --~%")

  ;; Count up on rising edges
  (cntu-execute dt "CT1" nil nil 5)  ; No edge
  (cntu-execute dt "CT1" t nil 5)    ; Rising edge - count
  (format t "After 1 count: CT1=~A CTD1=~A~%" (get-bool dt "CT1") (get-word dt "CTD1"))

  (cntu-execute dt "CT1" nil nil 5)  ; Falling edge
  (cntu-execute dt "CT1" t nil 5)    ; Rising edge - count
  (cntu-execute dt "CT1" nil nil 5)
  (cntu-execute dt "CT1" t nil 5)    ; Rising edge - count
  (cntu-execute dt "CT1" nil nil 5)
  (cntu-execute dt "CT1" t nil 5)    ; Rising edge - count
  (cntu-execute dt "CT1" nil nil 5)
  (cntu-execute dt "CT1" t nil 5)    ; Rising edge - count (5 total, preset reached)
  (format t "After 5 counts: CT1=~A CTD1=~A (expected T, 5)~%"
          (get-bool dt "CT1") (get-word dt "CTD1"))

  ;; Reset
  (cntu-execute dt "CT1" nil t 5)
  (format t "After reset: CT1=~A CTD1=~A (expected NIL, 0)~%"
          (get-bool dt "CT1") (get-word dt "CTD1")))

;; Test CNTD - Down counter
(let ((dt (make-test-dt)))
  (format t "~%-- CNTD (Down Counter) --~%")

  ;; Reset loads preset
  (cntd-execute dt "CT2" nil t 5)
  (format t "After reset (loaded preset): CT2=~A CTD2=~A~%"
          (get-bool dt "CT2") (get-word dt "CTD2"))

  ;; Count down
  (cntd-execute dt "CT2" t nil 5)    ; Rising edge - count down
  (cntd-execute dt "CT2" nil nil 5)
  (cntd-execute dt "CT2" t nil 5)    ; Count down
  (cntd-execute dt "CT2" nil nil 5)
  (cntd-execute dt "CT2" t nil 5)    ; Count down
  (cntd-execute dt "CT2" nil nil 5)
  (cntd-execute dt "CT2" t nil 5)    ; Count down
  (cntd-execute dt "CT2" nil nil 5)
  (cntd-execute dt "CT2" t nil 5)    ; Count down to 0
  (format t "After 5 counts down: CT2=~A CTD2=~A (expected T, 0)~%"
          (get-bool dt "CT2") (get-word dt "CTD2")))

;; Test UDC - Up/Down counter
(let ((dt (make-test-dt)))
  (format t "~%-- UDC (Up/Down Counter) --~%")

  ;; Count up
  (udc-execute dt "CT3" t nil nil 10)  ; Up
  (udc-execute dt "CT3" nil nil nil 10) ; Release
  (udc-execute dt "CT3" t nil nil 10)   ; Up
  (udc-execute dt "CT3" nil nil nil 10)
  (udc-execute dt "CT3" t nil nil 10)   ; Up
  (format t "After 3 up counts: CTD3=~A~%" (get-word dt "CTD3"))

  ;; Count down
  (udc-execute dt "CT3" nil nil nil 10)
  (udc-execute dt "CT3" nil t nil 10)   ; Down
  (format t "After 1 down count: CTD3=~A~%" (get-word dt "CTD3"))

  ;; Reset
  (udc-execute dt "CT3" nil nil t 10)
  (format t "After reset: CTD3=~A (expected 0)~%" (get-word dt "CTD3")))

;;; ============================================================
;;; Table Operations Tests
;;; ============================================================

(format t "~%=== Table Operations Tests ===~%")

;; Test address parsing
(format t "~%-- Address Parsing --~%")
(format t "parse-address \"DS100\" = ~A (expected (DS . 100))~%" (parse-address "DS100"))
(format t "parse-address \"CTD25\" = ~A (expected (CTD . 25))~%" (parse-address "CTD25"))
(format t "make-address DS 100 = ~A (expected DS100)~%" (make-address "DS" 100))

;; Test address range generation
(format t "~%-- Address Range Generation --~%")
(format t "Range DS1-DS5: ~A~%" (generate-address-range "DS1" "DS5"))
(format t "Range C10-C8 (reverse): ~A~%" (generate-address-range "C10" "C8"))

;; Test COPY single
(let ((dt (make-test-dt)))
  (format t "~%-- COPY Single Value --~%")

  ;; Copy constant to word
  (copy-single dt 12345 "DS100")
  (format t "Copy 12345 to DS100: ~A (expected 12345)~%" (get-word dt "DS100"))

  ;; Copy word to word
  (copy-single dt "DS100" "DS101")
  (format t "Copy DS100 to DS101: ~A (expected 12345)~%" (get-word dt "DS101"))

  ;; Copy word to float
  (copy-single dt "DS100" "DF100")
  (format t "Copy DS100 to DF100: ~A (expected 12345.0)~%" (get-float dt "DF100"))

  ;; Copy to boolean
  (copy-single dt 1 "C100")
  (format t "Copy 1 to C100: ~A (expected T)~%" (get-bool dt "C100"))
  (copy-single dt 0 "C101")
  (format t "Copy 0 to C101: ~A (expected NIL)~%" (get-bool dt "C101")))

;; Test FILL
(let ((dt (make-test-dt)))
  (format t "~%-- FILL Range --~%")

  ;; Fill word range
  (fill-range dt 42 "DS1" "DS5")
  (format t "Fill DS1-DS5 with 42:~%")
  (loop for i from 1 to 5
        do (format t "  DS~D = ~A~%" i (get-word dt (format nil "DS~D" i))))

  ;; Fill boolean range
  (fill-range dt t "C1" "C4")
  (format t "Fill C1-C4 with T:~%")
  (loop for i from 1 to 4
        do (format t "  C~D = ~A~%" i (get-bool dt (format nil "C~D" i)))))

;; Test CPYBLK
(let ((dt (make-test-dt)))
  (format t "~%-- CPYBLK (Copy Block) --~%")

  ;; Setup source values
  (loop for i from 1 to 5
        do (set-word dt (format nil "DS~D" i) (* i 10)))

  ;; Copy block
  (copy-block dt "DS1" "DS5" "DS100")
  (format t "Copy DS1-DS5 to DS100-DS104:~%")
  (loop for i from 100 to 104
        do (format t "  DS~D = ~A~%" i (get-word dt (format nil "DS~D" i)))))

;; Test PACK
(let ((dt (make-test-dt)))
  (format t "~%-- PACK (Pack Bits to Word) --~%")

  ;; Set some boolean values (pattern: 10110 binary = 22)
  (set-bool dt "C1" nil)
  (set-bool dt "C2" t)
  (set-bool dt "C3" t)
  (set-bool dt "C4" nil)
  (set-bool dt "C5" t)

  (pack-bits dt "C1" "C5" "DH100")
  (format t "Pack C1-C5 (01101 LSB first) to DH100: ~D (expected 22)~%"
          (get-word dt "DH100")))

;; Test UNPACK
(let ((dt (make-test-dt)))
  (format t "~%-- UNPACK (Unpack Word to Bits) --~%")

  ;; Set word value
  (set-word dt "DH100" 22)  ; Binary: 10110

  (unpack-bits dt "DH100" "C1" "C5")
  (format t "Unpack DH100 (22 = 10110) to C1-C5:~%")
  (loop for i from 1 to 5
        do (format t "  C~D = ~A~%" i (get-bool dt (format nil "C~D" i)))))

;; Test SHFRG (Shift Register)
(let ((dt (make-test-dt)))
  (format t "~%-- SHFRG (Shift Register) --~%")

  ;; Initialize shift register
  (set-bool dt "C1" nil)
  (set-bool dt "C2" nil)
  (set-bool dt "C3" nil)
  (set-bool dt "C4" nil)

  ;; Shift in data with clock edge
  (shift-register dt t t nil nil "C1" "C4")  ; Rising edge, shift in T
  (format t "Shift in T: C1=~A C2=~A C3=~A C4=~A~%"
          (get-bool dt "C1") (get-bool dt "C2")
          (get-bool dt "C3") (get-bool dt "C4"))

  ;; Another shift
  (shift-register dt nil nil t nil "C1" "C4") ; Falling edge, no shift
  (shift-register dt t t nil nil "C1" "C4")   ; Rising edge, shift in T
  (format t "Shift in T: C1=~A C2=~A C3=~A C4=~A~%"
          (get-bool dt "C1") (get-bool dt "C2")
          (get-bool dt "C3") (get-bool dt "C4"))

  ;; Shift in NIL
  (shift-register dt nil nil t nil "C1" "C4")
  (shift-register dt nil t nil nil "C1" "C4")
  (format t "Shift in NIL: C1=~A C2=~A C3=~A C4=~A~%"
          (get-bool dt "C1") (get-bool dt "C2")
          (get-bool dt "C3") (get-bool dt "C4"))

  ;; Reset
  (shift-register dt nil nil nil t "C1" "C4")
  (format t "After reset: C1=~A C2=~A C3=~A C4=~A~%"
          (get-bool dt "C1") (get-bool dt "C2")
          (get-bool dt "C3") (get-bool dt "C4")))

;; Test FIND* operations
(let ((dt (make-test-dt)))
  (format t "~%-- FIND* (Search Operations) --~%")

  ;; Setup test data
  (loop for i from 1 to 10
        do (set-word dt (format nil "DS~D" i) (* i 10)))

  ;; FINDEQ - find equal
  (find-equal dt 50 "DS1" "DS10" "DS100" "C100")
  (format t "Find 50 in DS1-DS10: found=~A at index ~A (expected T, 5)~%"
          (get-bool dt "C100") (get-word dt "DS100"))

  ;; FINDNE - find not equal (first value not equal to 50)
  (find-not-equal dt 50 "DS1" "DS10" "DS101" "C101")
  (format t "Find != 50 in DS1-DS10: found=~A at index ~A (expected T, 1)~%"
          (get-bool dt "C101") (get-word dt "DS101"))

  ;; FINDGT - find greater than
  (find-greater dt 75 "DS1" "DS10" "DS102" "C102")
  (format t "Find > 75 in DS1-DS10: found=~A at index ~A (expected T, 8)~%"
          (get-bool dt "C102") (get-word dt "DS102"))

  ;; FINDLT - find less than
  (find-less dt 25 "DS1" "DS10" "DS103" "C103")
  (format t "Find < 25 in DS1-DS10: found=~A at index ~A (expected T, 1 or 2)~%"
          (get-bool dt "C103") (get-word dt "DS103"))

  ;; Search for non-existent value
  (find-equal dt 999 "DS1" "DS10" "DS104" "C104")
  (format t "Find 999 in DS1-DS10: found=~A at index ~A (expected NIL, -1)~%"
          (get-bool dt "C104") (get-word dt "DS104")))

;; Test incremental search
(let ((dt (make-test-dt)))
  (format t "~%-- Incremental Search --~%")

  ;; Setup test data with duplicates
  (set-word dt "DS1" 10)
  (set-word dt "DS2" 20)
  (set-word dt "DS3" 20)  ; Duplicate
  (set-word dt "DS4" 20)  ; Duplicate
  (set-word dt "DS5" 30)

  ;; First search for 20
  (find-equal dt 20 "DS1" "DS5" "DS100" "C100")
  (format t "First find 20: found=~A at index ~A (expected T, 2)~%"
          (get-bool dt "C100") (get-word dt "DS100"))

  ;; Incremental search - find next 20
  (find-equal-inc dt 20 "DS1" "DS5" "DS100" "C100")
  (format t "Incremental find 20: found=~A at index ~A (expected T, 3)~%"
          (get-bool dt "C100") (get-word dt "DS100"))

  ;; Another incremental search
  (find-equal-inc dt 20 "DS1" "DS5" "DS100" "C100")
  (format t "Incremental find 20: found=~A at index ~A (expected T, 4)~%"
          (get-bool dt "C100") (get-word dt "DS100"))

  ;; No more matches
  (find-equal-inc dt 20 "DS1" "DS5" "DS100" "C100")
  (format t "Incremental find 20: found=~A at index ~A (expected NIL, -1)~%"
          (get-bool dt "C100") (get-word dt "DS100")))

;; Test SUM
(let ((dt (make-test-dt)))
  (format t "~%-- SUM Operation --~%")

  ;; Setup test data
  (loop for i from 1 to 5
        do (set-word dt (format nil "DS~D" i) i))

  ;; Sum DS1-DS5 (1+2+3+4+5 = 15)
  (sum-range dt "DS1" "DS5" "DS100" nil)
  (format t "Sum DS1-DS5 (1+2+3+4+5): ~A (expected 15)~%"
          (get-word dt "DS100")))

;;; ============================================================
;;; Integration Tests with Interpreter
;;; ============================================================

(format t "~%=== Integration Tests ===~%")

;; Test COPY instruction in IL program
(let* ((source "NETWORK 1
STR SC1
COPY 12345 DS100

NETWORK 2
STR SC1
COPY DS100 DS101
")
       (interp (run-il-string source :max-scans 1)))
  (format t "~%-- COPY in IL Program --~%")
  (format t "DS100 = ~A (expected 12345)~%" (get-word-value interp "DS100"))
  (format t "DS101 = ~A (expected 12345)~%" (get-word-value interp "DS101")))

;; Test TMR instruction in IL program
(let* ((source "NETWORK 1
STR X1
TMR T1 1000 ms
")
       (compiled (compile-il-string source))
       (interp (make-plc-interpreter :program compiled)))
  (format t "~%-- TMR in IL Program --~%")

  ;; Set input and run scans (simulate 100ms scan time)
  (set-bool-value interp "X1" t)
  (run-continuous interp :max-scans 5)  ; 500ms simulated
  (format t "After 5 scans (500ms sim): T1=~A~%" (get-bool-value interp "T1"))

  (run-continuous interp :max-scans 6)  ; 1100ms simulated
  (format t "After 11 scans (1100ms sim): T1=~A~%" (get-bool-value interp "T1")))

;; Test MATHDEC instruction in IL program
(let* ((source "NETWORK 1
STR SC1
COPY 100 DS10
COPY 50 DS11
MATHDEC DS100 0 DS10 + DS11 * 2
")
       (interp (run-il-string source :max-scans 1)))
  (format t "~%-- MATHDEC in IL Program --~%")
  (format t "DS10=~A DS11=~A~%" (get-word-value interp "DS10") (get-word-value interp "DS11"))
  (format t "DS100 = DS10 + DS11 * 2 = ~A (expected 200)~%"
          (get-word-value interp "DS100")))

(format t "~%=== Phase 6 Testing Complete ===~%")
