;;;; test-ladder-render.lisp
;;;; Test script for ladder rendering

;; Load quicklisp
(load "~/quicklisp/setup.lisp")

;; Add local system directory
(push #p"D:/common-lisp/mblogic-cl/" asdf:*central-registry*)

;; Load the systems
(ql:quickload :mblogic-cl :silent t)
(ql:quickload :mblogic-cl/web :silent t)

;; Parse the test program
(defparameter *test-prog* 
  (mblogic-cl:parse-il-file "D:/common-lisp/mblogic-cl/test/plcprog.txt"))

;; Convert LadderDemo subroutine to ladder format
(defparameter *ladder-demo*
  (mblogic-cl-web:program-to-ladder *test-prog* "LadderDemo"))

;; Convert to JS format (demodata.js compatible)
(defparameter *ladder-js*
  (mblogic-cl-web:ladder-program-to-js-format *ladder-demo*))

;; Print JSON
(format t "~%~%=== Testing Ladder Render Output ===~%~%")
(format t "Subroutine: ~A~%" (mblogic-cl-web:ladder-program-name *ladder-demo*))
(format t "Total rungs: ~A~%~%" (length (mblogic-cl-web:ladder-program-rungs *ladder-demo*)))

;; Test first few rungs
(loop for rung in (subseq (mblogic-cl-web:ladder-program-rungs *ladder-demo*) 0 (min 5 (length (mblogic-cl-web:ladder-program-rungs *ladder-demo*))))
      for i from 1
      do (format t "Rung ~D: ~A rows, ~A cols, comment: ~S~%"
                 i
                 (mblogic-cl-web:ladder-rung-rows rung)
                 (mblogic-cl-web:ladder-rung-cols rung)
                 (mblogic-cl-web:ladder-rung-comment rung)))

;; Output JSON for first rung
(format t "~%~%=== First Rung JSON Structure ===~%")
(let ((first-rung (first (mblogic-cl-web:ladder-program-rungs *ladder-demo*))))
  (format t "Network number: ~A~%" (mblogic-cl-web:ladder-rung-number first-rung))
  (format t "Cells count: ~A~%" (length (mblogic-cl-web:ladder-rung-cells first-rung)))
  (format t "~%Cell details:~%")
  (loop for cell in (mblogic-cl-web:ladder-rung-cells first-rung)
        for i from 0
        do (format t "  Cell ~D: type=~A row=~D col=~D symbol=~S addr=~S~%"
                   i
                   (mblogic-cl-web:ladder-cell-type cell)
                   (mblogic-cl-web:ladder-cell-row cell)
                   (mblogic-cl-web:ladder-cell-col cell)
                   (mblogic-cl-web:ladder-cell-symbol cell)
                   (mblogic-cl-web:ladder-cell-address cell))))

;; Try to output as JSON using yason if available
(handler-case
    (progn
      (ql:quickload :yason :silent t)
      (format t "~%~%=== Full JSON Output (first 3 rungs) ===~%")
      (let ((first-three-rungs (subseq (mblogic-cl-web:ladder-program-rungs *ladder-demo*) 
                                        0 
                                        (min 3 (length (mblogic-cl-web:ladder-program-rungs *ladder-demo*))))))
        (let ((js-data (list (cons :subroutinename (mblogic-cl-web:ladder-program-name *ladder-demo*))
                            (cons :subrcomments "")
                            (cons :signature 0)
                            (cons :rungdata (loop for rung in first-three-rungs
                                                 for i from 0
                                                 collect (mblogic-cl-web:rung-to-js-format rung i))))))
          (yason:encode js-data *standard-output*)
          (terpri))))
  (error (e)
    (format t "~%Could not encode JSON: ~A~%" e)))

(format t "~%~%=== Test Complete ===~%")
