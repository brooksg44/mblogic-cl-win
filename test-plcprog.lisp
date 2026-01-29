;;;; test-plcprog.lisp - Test interpreter with plcprog.txt

;;; Load Quicklisp if not already loaded
#-quicklisp
(let ((quicklisp-init (merge-pathnames "quicklisp/setup.lisp"
                                        (user-homedir-pathname))))
  (when (probe-file quicklisp-init)
    (load quicklisp-init)))

(ql:quickload :mblogic-cl)
(in-package :mblogic-cl)

(format t "~%=== Testing Interpreter with plcprog.txt ===~%")

(let ((interp (run-il-file "/Users/gregorybrooks/common-lisp/mblogic-cl/test/plcprog.txt" :max-scans 10)))
  (print-interpreter-status interp)
  (format t "~%Selected outputs after 10 scans:~%")
  (format t "  Y1=~A~%" (get-bool-value interp "Y1"))
  (format t "  Y2=~A~%" (get-bool-value interp "Y2"))
  (format t "  C1=~A~%" (get-bool-value interp "C1")))

(format t "~%=== plcprog.txt Test Complete ===~%")
