;;;; analyze-ladder.lisp
;;;; Analyze the ladder rendering output

(load "~/quicklisp/setup.lisp")
(push #p"/Users/gregorybrooks/common-lisp/mblogic-cl/" asdf:*central-registry*)
(ql:quickload :mblogic-cl :silent t)
(ql:quickload :mblogic-cl/web :silent t)

(defparameter *src* (mblogic-cl:parse-il-file "test/plcprog.txt"))
(defparameter *ldr* (mblogic-cl-web:program-to-ladder *src* "LadderDemo"))

(format t "=== LadderDemo Analysis ===~%~%")
(format t "Total rungs: ~A~%~%" (length (mblogic-cl-web:ladder-program-rungs *ldr*)))

;; Analyze first 3 rungs
(loop for rung in (subseq (mblogic-cl-web:ladder-program-rungs *ldr*) 0 3)
      for i from 1
      do (format t "Rung ~A:~%" i)
         (format t "  Network number: ~A~%" (mblogic-cl-web:ladder-rung-number rung))
         (format t "  Rows: ~A, Cols: ~A~%" 
                 (mblogic-cl-web:ladder-rung-rows rung)
                 (mblogic-cl-web:ladder-rung-cols rung))
         (format t "  Comment: ~S~%" (mblogic-cl-web:ladder-rung-comment rung))
         (format t "  Cells:~%")
         (loop for cell in (mblogic-cl-web:ladder-rung-cells rung)
               do (format t "    [~2A,~2A] ~8A ~10S ~S~%"
                         (mblogic-cl-web:ladder-cell-row cell)
                         (mblogic-cl-web:ladder-cell-col cell)
                         (mblogic-cl-web:ladder-cell-type cell)
                         (mblogic-cl-web:ladder-cell-symbol cell)
                         (mblogic-cl-web:ladder-cell-address cell)))
         (format t "~%"))

;; Now check JS format
(format t "=== JS Format Analysis ===~%~%")
(defparameter *js* (mblogic-cl-web:ladder-program-to-js-format *ldr*))
(format t "Top-level keys: ~A~%" (mapcar #'car *js*))
(format t "Subroutine name: ~A~%" (cdr (assoc :subroutinename *js*)))
(format t "Rung count: ~A~%~%" (length (cdr (assoc :rungdata *js*))))

;; Analyze first rung in JS format
(let ((rung (first (cdr (assoc :rungdata *js*)))))
  (format t "First rung in JS format:~%")
  (format t "  Keys: ~A~%" (mapcar #'car rung))
  (format t "  Rungtype: ~A~%" (cdr (assoc :rungtype rung)))
  (format t "  Comment: ~S~%" (cdr (assoc :comment rung)))
  (format t "  Reference: ~A~%" (cdr (assoc :reference rung)))
  (format t "  Matrixdata cells:~%")
  (loop for cell in (cdr (assoc :matrixdata rung))
        do (format t "    ~A: value=~S addr=~S~%"
                  (car cell)
                  (cdr (assoc :value (cdr cell)))
                  (cdr (assoc :addr (cdr cell))))))

;; Check if JSON encoding works
(format t "~%=== JSON Encoding Test ===~%~%")
(let ((json-str (cl-json:encode-json-alist-to-string *js*)))
  (format t "JSON length: ~A bytes~%" (length json-str))
  (format t "First 600 chars:~%~A~%~%" (subseq json-str 0 (min 600 (length json-str)))))

(format t "=== Analysis Complete ===~%")
