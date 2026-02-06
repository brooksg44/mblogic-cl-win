;;;; check-references.lisp
;;;; Check that all rungs have reference field

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
  
;; Extract rungdata
(defparameter *rungdata* (cdr (assoc :rungdata *js-format*)))
(format t "Total rungs: ~D~%~%" (length *rungdata*))

;; Show first 10 rungs with their reference numbers
(format t "First 10 rungs (showing reference field):~%")
(loop for rung in (subseq *rungdata* 0 (min 10 (length *rungdata*)))
      for i from 1
      do (let ((ref (cdr (assoc :reference rung)))
              (rungtype (cdr (assoc :rungtype rung))))
           (format t "  Rung ~D: reference=~A, rungtype=~A~%" i ref rungtype)))

;; Check if all rungs have reference field
(format t "~%Checking all rungs have reference field...~%")
(let ((missing-refs nil))
  (loop for rung in *rungdata*
        for i from 0
        do (unless (assoc :reference rung)
             (push i missing-refs)))
  (if missing-refs
      (format t "ERROR: Rungs missing reference: ~A~%" (nreverse missing-refs))
      (format t "SUCCESS: All rungs have reference field~%")))

;; Encode to JSON and show length
(format t "~%Encoding to JSON...~%")
(defparameter *json-str* (cl-json:encode-json-to-string *js-format*))
(format t "JSON length: ~D bytes~%" (length *json-str*))

;; Show snippet with reference field visible
(format t "~%Sample JSON (showing reference):~%")
(let ((pos (search "\"reference\"" *json-str*)))
  (when pos
    (format t "~A~%" (subseq *json-str* (max 0 (- pos 100)) 
                                         (min (length *json-str*) (+ pos 50))))))

(format t "~%Done!~%")
