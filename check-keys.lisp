;;;; check-keys.lisp
;;;; Check what keys we're generating for the first few rungs

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

;; Check keys for first 5 rungs
(format t "Checking matrixdata keys for first 5 rungs:~%~%")
(loop for i from 0 to 4
      do (let* ((rung (nth i *rungdata*))
                (matrixdata (cdr (assoc :matrixdata rung)))
                (keys (mapcar #'car matrixdata)))
           (format t "Rung ~D (~A):~%" (1+ i) (cdr (assoc :rungtype rung)))
           (format t "  Keys: ~{~A~^, ~}~%~%" keys)))

(format t "~%Done!~%")
