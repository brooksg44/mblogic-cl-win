;;;; check-ladder-json.lisp
;;;; Check the ladder rendering JSON output

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

(format t "~%LadderDemo Statistics:~%")
(format t "  Total rungs: ~A~%~%" (length (mblogic-cl-web:ladder-program-rungs *ladder*)))

;; Convert to JS format
(defparameter *js-format* (mblogic-cl-web:ladder-program-to-js-format *ladder*))

;; Show first few rungs
(format t "First 5 rungs:~%")
(let ((rungs (cdr (assoc :rungdata *js-format*))))
  (loop for rung in (subseq rungs 0 (min 5 (length rungs)))
        for i from 1
        do (format t "~%Rung ~A:~%" i)
           (format t "  rungtype: ~A~%" (cdr (assoc :rungtype rung)))
           (format t "  comment: ~A~%" (cdr (assoc :comment rung)))
           (format t "  matrixdata keys: ~A~%~%" 
                   (mapcar #'car (cdr (assoc :matrixdata rung))))))

;; Check JSON encoding
(format t "~%Attempting JSON encoding...~%")
(handler-case
    (let ((json-str (cl-json:encode-json-to-string *js-format*)))
      (format t "JSON length: ~A bytes~%~%" (length json-str))
      (format t "First 500 chars of JSON:~%~A~%~%" (subseq json-str 0 (min 500 (length json-str)))))
  (error (e)
    (format t "ERROR encoding JSON: ~A~%~%" e)))

(format t "~%Done!~%")
