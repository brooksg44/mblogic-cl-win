;;; Debug script for Rung 3 matrix rendering
;;; Run with: sbcl --load debug-rung3.lisp

(ql:quickload :mblogic-cl/web)

(defpackage :debug-rung3
  (:use :cl :mblogic-cl :mblogic-cl-web))

(in-package :debug-rung3)

(defun run ()
  (let* ((source (format nil "NETWORK 1~%STR T5~%ORN C1~%AND C2~%STR C3~%AND C4~%STR C5~%OR C6~%ANDSTR~%ORSTR~%STRE DS100 50~%ORPD C100~%ANDGT DS112 86~%ANDSTR~%SET C101~%"))
         (program (parse-il-string source))
         (network (first (program-main-networks program)))
         (rung (network-to-ladder-rung network))
         (cells (ladder-rung-cells rung))
         (rows (ladder-rung-rows rung))
         (cols (ladder-rung-cols rung))
         (matrix (make-array (list rows cols) :initial-element nil)))
    ;; Print all cells with details
    (format t "~%=== ALL CELLS ===~%")
    (dolist (cell cells)
      (format t "  [~D,~D] sym=~A type=~A addr=~A~%"
              (ladder-cell-row cell) (ladder-cell-col cell)
              (ladder-cell-symbol cell) (ladder-cell-type cell)
              (ladder-cell-address cell)))
    ;; Fill matrix from cells
    (dolist (cell cells)
      (let ((r (ladder-cell-row cell))
            (c (ladder-cell-col cell))
            (sym (ladder-cell-symbol cell)))
        (when (and (< r rows) (< c cols))
          (setf (aref matrix r c) sym))))
    ;; Print matrix
    (format t "~%=== RUNG 3 MATRIX (~Dx~D) ===~%" rows cols)
    (dotimes (r rows)
      (format t "Row ~D:" r)
      (dotimes (c cols)
        (format t " ~12A" (or (aref matrix r c) "---")))
      (terpri))
    ;; Print expected
    (format t "~%=== EXPECTED (from demodata.js) ===~%")
    (format t "Row 0: noc          brancht      noc          hbar         brancht      compeq       brancht      compgt       hbar~%")
    (format t "Row 1: ncc          branchr      hbar         hbar         branchr      nocpd        branchr      hbar         hbar~%")
    (format t "Row 2: noc          noc          branchttr    noc          branchttr    hbar         hbar         hbar~%")
    (format t "Row 3: hbar         hbar         noc          branchr      hbar         hbar         hbar         hbar~%")
    ;; Check for differences
    (format t "~%=== CHECKING FOR ISSUES ===~%")
    (let ((issues 0))
      (dotimes (r rows)
        (dotimes (c cols)
          (let ((sym (aref matrix r c)))
            (when (and sym (string-equal sym "branchttr"))
              (format t "  branchttr at Row ~D Col ~D~%" r c)
              (incf issues)))))
      (format t "Total branchttr cells: ~D~%" issues))))

(run)
(sb-ext:exit)
