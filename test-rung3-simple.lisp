;;;; Simple test to print Rung 3 matrix
;;;; Run with: sbcl --load test-rung3-simple.lisp

(ql:quickload :mblogic-cl)

;;; Load the mblogic-cl package
(in-package :mblogic-cl)

;;; Load the mblogic-cl package
(in-package :mblogic-cl)

(defun rung3-symbol-matrix ()
  "Parse Rung 3 IL and return symbol matrix as printed output"
  (let* ((source "NETWORK 1
STR T5
ORN C1
AND C2
STR C3
AND C4
STR C5
OR C6
ANDSTR
ORSTR
STRE DS100 50
ORPD C100
ANDGT DS112 86
ANDSTR
SET C101
")
         (program (mblogic-cl:parse-il-string source))
         (network (first (mblogic-cl:program-main-networks program))))
    (mblogic-cl:network-to-ladder-rung network)))

(defun print-rung3-matrix ()
  "Parse Rung 3 and print the matrix structure"
  (let* ((rung (rung3-symbol-matrix))
         (cells (mblogic-cl:ladder-rung-cells rung))
         (rows (mblogic-cl:ladder-rung-rows rung))
         (cols (mblogic-cl:ladder-rung-cols rung))
         (matrix (make-array (list rows cols) :initial-element nil)))
    (dolist (cell cells)
      (let ((row (mblogic-cl:ladder-cell-row cell))
            (col (mblogic-cl:ladder-cell-col cell))
            (sym (mblogic-cl:ladder-cell-symbol cell)))
        (when (and (< row rows) (< col cols))
          (setf (aref matrix row col) sym))))
    (format t "~%~%=== RUNG 3 MATRIX ===~%")
    (format t "Rows: ~D Cols: ~D~%" rows cols)
    (format t "Actual (from Lisp):~%")
    (dotimes (r rows)
      (format t "Row ~D: " r)
      (dotimes (c cols)
        (format t "~A " (aref matrix r c)))
      (format t "~%"))
    (format t "~%Expected (from demodata.js):~%")
    (format t "Row 0: noc brancht noc hbar brancht compeq brancht compgt hbar~%")
    (format t "Row 1: ncc branchr hbar hbar branchr nocpd branchr hbar hbar~%")
    (format t "Row 2: noc noc branchttr noc branchttr hbar hbar hbar~%")
    (format t "Row 3: hbar hbar noc branchr hbar hbar hbar hbar~%")
    (format t "~%DIFFERENCES (known issue):~%")
    (format t "Row 0 Col 3: Expected hbar, Actual branchttr~%")
    (format t "Row 0 Col 6: Expected hbar, Actual branchttr~%")
    matrix))

(print-rung3-matrix)
