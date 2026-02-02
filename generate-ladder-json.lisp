;;;; generate-ladder-json.lisp
;;;; Simple script to generate JSON output for the LadderDemo subroutine

(in-package :cl-user)

;; Load the systems
(format t "Loading mblogic-cl...~%")
(ql:quickload :mblogic-cl :silent t)

(format t "Loading mblogic-cl/web...~%")
(ql:quickload :mblogic-cl/web :silent t)

;; Parse the program
(format t "Parsing plcprog.txt...~%")
(defparameter *source* 
  (mblogic-cl:parse-il-file "test/plcprog.txt"))

;; Generate ladder structure for LadderDemo
(format t "Generating ladder structure for LadderDemo...~%")
(defparameter *ladder* 
  (mblogic-cl-web::program-to-ladder *source* "LadderDemo"))

;; Convert to plist
(format t "Converting to plist...~%")
(defparameter *plist* 
  (mblogic-cl-web::ladder-program-to-plist *ladder*))

;; Convert to JSON
(format t "Converting to JSON...~%")
(defparameter *json* 
  (mblogic-cl-web::plist-to-json *plist*))

;; Pretty print the JSON
(format t "~%=== LadderDemo JSON Output ===~%~%")
(format t "~A~%~%" *json*)

;; Save to file
(format t "Saving to LadderDemo-output.json...~%")
(with-open-file (out "LadderDemo-output.json"
                     :direction :output
                     :if-exists :supersede)
  (write-string *json* out))

(format t "~%Done! JSON saved to LadderDemo-output.json~%")
(format t "~%To see all available subroutines:~%")
(format t "(mblogic-cl-web::list-subroutine-names *source*)~%")
