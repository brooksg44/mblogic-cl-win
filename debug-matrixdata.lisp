;;;; debug-matrixdata.lisp
;;;; Debug exact matrixdata being generated and check against JavaScript expectations

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

;; Encode to JSON
(defparameter *json-str* (cl-json:encode-json-to-string *js-format*))

;; Write to file for inspection
(with-open-file (out "D:/common-lisp/mblogic-cl/test-output.json"
                     :direction :output
                     :if-exists :supersede)
  (write-string *json-str* out))

(format t "JSON written to test-output.json (~D bytes)~%" (length *json-str*))

;; Extract and analyze first failing rung
(defparameter *rungdata* (cdr (assoc :rungdata *js-format*)))

;; Check each rung to see which one might be problematic
(format t "~%Analyzing all rungs for potential issues:~%~%")
(loop for rung in *rungdata*
      for i from 1
      do (let* ((matrixdata (cdr (assoc :matrixdata rung)))
                (rungtype (cdr (assoc :rungtype rung)))
                (keys (mapcar #'car matrixdata))
                ;; Check if any keys would be out of bounds for GenRungDataObject
                (max-row-for-type (cond ((string= rungtype "single") 7)
                                       ((string= rungtype "double") 1)
                                       ((string= rungtype "triple") 2)
                                       (t 0)))
                (max-col-for-type 7)
                (bad-keys nil))
           
           ;; Check each key
           (dolist (key keys)
             (let ((key-str (string-downcase (symbol-name key))))
               (cond
                 ;; Check input keys
                 ((and (>= (length key-str) 11)
                       (string= (subseq key-str 0 9) "inputedit"))
                  (let* ((row (parse-integer (subseq key-str 9 10)))
                         (col (parse-integer (subseq key-str 10 11))))
                    (when (> row max-row-for-type)
                      (push (list key row col "row too high") bad-keys))))
                 ;; Check output keys
                 ((and (>= (length key-str) 11)
                       (string= (subseq key-str 0 10) "outputedit"))
                  (let ((row (parse-integer (subseq key-str 10))))
                    (when (> row 7)
                      (push (list key row "row too high for output") bad-keys)))))))
           
           (when bad-keys
             (format t "Rung ~D (~A): PROBLEMATIC - has out-of-bounds keys:~%" i rungtype)
             (dolist (bad bad-keys)
               (format t "  ~A~%" bad))
             (format t "~%"))))

(format t "Done!~%")
