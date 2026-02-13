;;; Simple test script for Rung 3
;;; Run with: sbcl --load test-rung3.lisp

(ql:quickload :mblogic-cl/web)

(in-package :mblogic-cl-web)

(load "debug-functions.lisp")

(defun debug-rung3 ()
  "Debug Rung 3 specifically"
  (let* ((source "NETWORK 1\nSTR T5\nORN C1\nAND C2\nSTR C3\nAND C4\nSTR C5\nOR C6\nANDSTR\nORSTR\nSTRE DS100 50\nORPD C100\nANDGT DS112 86\nANDSTR\nSET C101\n")
         (program (mblogic-cl:parse-il-string source))
         (network (first (mblogic-cl:program-main-networks program))))
    (debug-network-to-ladder-rung network)))

(let ((rung (debug-rung3)))
  (format t "~%Test complete!~%")
  (format t "Rung: ~S~%" rung))