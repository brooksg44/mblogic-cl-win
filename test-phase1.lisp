;;;; test-phase1.lisp - Manual Phase 1 testing script

(ql:quickload :mblogic-cl)
(in-package :mblogic-cl)

;; Create and initialize a data table
(format t "~%=== Phase 1 Testing ===~%")
(format t "Creating data table...~%")
(defparameter *dt* (make-data-table))
(init-data-table *dt*)
(format t "Data table created and initialized!~%~%")

;; Test Boolean operations
(format t "=== Boolean Operations ===~%")
(set-bool *dt* "X1" t)
(format t "set-bool X1 to T~%")
(format t "get-bool X1 => ~A~%" (get-bool *dt* "X1"))
(set-bool *dt* "Y1" nil)
(format t "set-bool Y1 to NIL~%")
(format t "get-bool Y1 => ~A~%~%" (get-bool *dt* "Y1"))

;; Test Word operations
(format t "=== Word Operations ===~%")
(set-word *dt* "DS100" 42)
(format t "set-word DS100 to 42~%")
(format t "get-word DS100 => ~A~%" (get-word *dt* "DS100"))
(set-word *dt* "DD1" -32768)
(format t "set-word DD1 to -32768~%")
(format t "get-word DD1 => ~A~%~%" (get-word *dt* "DD1"))

;; Test Float operations
(format t "=== Float Operations ===~%")
(set-float *dt* "DF1" 3.14159)
(format t "set-float DF1 to 3.14159~%")
(format t "get-float DF1 => ~A~%" (get-float *dt* "DF1"))
(set-float *dt* "DF100" -273.15)
(format t "set-float DF100 to -273.15~%")
(format t "get-float DF100 => ~A~%~%" (get-float *dt* "DF100"))

;; Test String operations
(format t "=== String Operations ===~%")
(set-string *dt* "TXT1" "Hello PLC")
(format t "set-string TXT1 to \"Hello PLC\"~%")
(format t "get-string TXT1 => ~A~%" (get-string *dt* "TXT1"))
(set-string *dt* "TXT500" "Industrial Automation")
(format t "set-string TXT500 to \"Industrial Automation\"~%")
(format t "get-string TXT500 => ~A~%~%" (get-string *dt* "TXT500"))

;; Test edge cases
(format t "=== Edge Cases ===~%")
;; Test boundary addresses
(set-bool *dt* "X2000" t)
(format t "X2000 (max input) => ~A~%" (get-bool *dt* "X2000"))
(set-word *dt* "DS10000" 99999)
(format t "DS10000 (max data reg) => ~A~%" (get-word *dt* "DS10000"))
(set-float *dt* "DF2000" 1.23456789d0)
(format t "DF2000 (max float) => ~A~%" (get-float *dt* "DF2000"))
(set-string *dt* "TXT10000" "Last string")
(format t "TXT10000 (max string) => ~A~%~%" (get-string *dt* "TXT10000"))

;; Test invalid address (should return NIL)
(format t "=== Invalid Address Test ===~%")
(format t "get-bool INVALID => ~A~%" (get-bool *dt* "INVALID"))
(format t "get-word INVALID => ~A~%~%" (get-word *dt* "INVALID"))

;; Summary
(format t "=== Summary ===~%")
(format t "All Phase 1 operations completed successfully!~%")
