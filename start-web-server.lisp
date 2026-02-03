;;;; start-web-server.lisp
;;;; Start the MBLogic web server with ladder visualization

;; Load quicklisp
(load "~/quicklisp/setup.lisp")

;; Add local system directory
(push #p"D:/common-lisp/mblogic-cl/" asdf:*central-registry*)

;; Load the systems
(format t "~%Loading MBLogic systems...~%")
(ql:quickload :mblogic-cl :silent t)
(ql:quickload :mblogic-cl/web :silent t)

;; Set static directory
(setf mblogic-cl-web:*static-directory* #p"D:/common-lisp/mblogic-cl/static/")

;; Parse and load the test program
(format t "~%Parsing IL program...~%")
(defparameter *parsed-prog* 
  (mblogic-cl:parse-il-file "D:/common-lisp/mblogic-cl/test/plcprog.txt"))

;; Compile the program
(format t "Compiling program...~%")
(defparameter *compiler* (mblogic-cl:make-il-compiler))
(defparameter *compiled-prog* 
  (mblogic-cl:compile-program *compiler* *parsed-prog*))

;; Create interpreter with compiled program
(defparameter *interp* 
  (mblogic-cl:make-plc-interpreter :program *compiled-prog*))

;; Start web server with interpreter
(format t "~%Starting web server on port 8080...~%")
(mblogic-cl-web:start-web-server :port 8080 :interpreter *interp*)

(format t "~%~%")
(format t "====================================================~%")
(format t "  MBLogic Web Server Running~%")
(format t "====================================================~%")
(format t "~%")
(format t "  Ladder Diagram Viewer: http://localhost:8080/laddertest.xhtml~%")
(format t "~%")
(format t "  Available Subroutines:~%")
(maphash (lambda (name sbr)
           (declare (ignore sbr))
           (format t "    - ~A~%" name))
         (mblogic-cl:program-subroutines *parsed-prog*))
(format t "~%")
(format t "  Press Ctrl+C to stop the server~%")
(format t "~%")
(format t "====================================================~%")
(format t "~%")

;; Keep server running
(loop (sleep 1))
