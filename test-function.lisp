;;; Simple test to check if network-to-ladder-rung exists
;;; Run with: sbcl --load test-function.lisp

(ql:quickload :mblogic-cl/web)

(in-package :mblogic-cl)

(defun test-function-exists ()
  "Test if network-to-ladder-rung function exists"
  (let ((f (find-symbol "NETWORK-TO-LADDER-RUNG" (find-package :mblogic-cl))))
    (if f
        (format t "Function exists: ~S~%" f)
        (format t "Function not found!~%"))))

(test-function-exists)