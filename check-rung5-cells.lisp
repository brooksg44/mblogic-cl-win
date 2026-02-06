;;;; check-rung5-cells.lisp
;;;; Check what cells are in rung 5 and their row/col values

;; Load quicklisp
(load "~/quicklisp/setup.lisp")

;; Add local system directory
(push #p"D:/common-lisp/mblogic-cl/" asdf:*central-registry*)

;; Load the systems
(format t "~%Loading systems...~%")
(ql:quickload :mblogic-cl :silent t)
(ql:quickload :mblogic-cl/web :silent t)

;; Parse and compile
(format t "Parsing program...~%")
(defparameter *parsed* (mblogic-cl:parse-il-file "D:/common-lisp/mblogic-cl/test/plcprog.txt"))

(format t "Compiling...~%")
(defparameter *compiler* (mblogic-cl:make-il-compiler))
(defparameter *compiled* (mblogic-cl:compile-program *compiler* *parsed*))

;; Convert to ladder
(format t "Converting LadderDemo to ladder format...~%~%")
(defparameter *ladder* (mblogic-cl-web:program-to-ladder *parsed* "LadderDemo"))

;; Get rung 5 (index 4)
(defparameter *rungs* (mblogic-cl-web:ladder-program-rungs *ladder*))
(defparameter *rung5* (nth 4 *rungs*))

(format t "Rung 5 details:~%")
(format t "  Number: ~D~%" (mblogic-cl-web:ladder-rung-number *rung5*))
(format t "  Rows: ~D~%" (mblogic-cl-web:ladder-rung-rows *rung5*))
(format t "  Cols: ~D~%~%" (mblogic-cl-web:ladder-rung-cols *rung5*))

(defparameter *cells* (mblogic-cl-web:ladder-rung-cells *rung5*))
(format t "Total cells: ~D~%~%" (length *cells*))

;; Group cells by row
(format t "Cells by row:~%")
(loop for row from 0 to 7
      do (let ((row-cells (remove-if-not (lambda (c) 
                                           (= (mblogic-cl-web:ladder-cell-row c) row))
                                         *cells*)))
           (when row-cells
             (format t "~%Row ~D (~D cells):~%" row (length row-cells))
             (dolist (cell row-cells)
               (format t "  col=~D type=~A symbol=~A~%" 
                       (mblogic-cl-web:ladder-cell-col cell)
                       (mblogic-cl-web:ladder-cell-type cell)
                       (mblogic-cl-web:ladder-cell-symbol cell))))))

(format t "~%~%Done!~%")
