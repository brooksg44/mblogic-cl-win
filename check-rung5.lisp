;;;; check-rung5.lisp
;;;; Examine rung 5 in detail to see why it might not be rendering

;; Load quicklisp
(load "~/quicklisp/setup.lisp")

;; Add local system directory
(push #p"D:/common-lisp/mblogic-cl/" asdf:*central-registry*)

;; Load the systems
(format t "~%Loading systems...~%")
(ql:quickload :mblogic-cl :silent t)
(ql:quickload :mblogic-cl/web :silent t)
(ql:quickload :cl-json :silent t)

;; Parse and compile
(format t "Parsing program...~%")
(defparameter *parsed* (mblogic-cl:parse-il-file "D:/common-lisp/mblogic-cl/test/plcprog.txt"))

(format t "Compiling...~%")
(defparameter *compiler* (mblogic-cl:make-il-compiler))
(defparameter *compiled* (mblogic-cl:compile-program *compiler* *parsed*))

;; Convert to ladder
(format t "Converting LadderDemo to ladder format...~%")
(defparameter *ladder* (mblogic-cl-web:program-to-ladder *parsed* "LadderDemo"))

;; Convert to JS format
(format t "Converting to JS format...~%~%")
(defparameter *js-format* (mblogic-cl-web:ladder-program-to-js-format *ladder*))

;; Extract rungs
(defparameter *rungdata* (cdr (assoc :rungdata *js-format*)))

;; Look at rungs 3, 4, and 5
(format t "Examining rungs 3-5:~%~%")
(loop for i from 2 to 4
      do (let ((rung (nth i *rungdata*)))
           (format t "===== Rung ~D =====~%" (1+ i))
           (format t "reference: ~A~%" (cdr (assoc :reference rung)))
           (format t "rungtype: ~A~%" (cdr (assoc :rungtype rung)))
           (format t "comment: ~A~%" (cdr (assoc :comment rung)))
           (format t "matrixdata cells: ~D~%~%" 
                   (length (cdr (assoc :matrixdata rung))))))

;; Encode full JSON
(format t "~%Encoding full JSON...~%")
(defparameter *json-str* (cl-json:encode-json-to-string *js-format*))
(format t "Total JSON length: ~D bytes~%~%" (length *json-str*))

;; Find where rung 4 is in the JSON
(format t "Looking for rung 4 (reference 3) in JSON...~%")
(let ((pos (search "\"reference\":3" *json-str*)))
  (when pos
    (format t "Found at position ~D~%" pos)
    (format t "Context: ~A~%~%" 
            (subseq *json-str* (max 0 (- pos 50)) 
                                (min (length *json-str*) (+ pos 150))))))

;; Find where rung 5 is in the JSON
(format t "Looking for rung 5 (reference 4) in JSON...~%")
(let ((pos (search "\"reference\":4" *json-str*)))
  (when pos
    (format t "Found at position ~D~%" pos)
    (format t "Context: ~A~%~%" 
            (subseq *json-str* (max 0 (- pos 50)) 
                                (min (length *json-str*) (+ pos 150))))))

(format t "Done!~%")
