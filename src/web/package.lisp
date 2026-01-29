;;;; src/web/package.lisp
;;;;
;;;; Package definition for MBLogic-CL web interface

(defpackage #:mblogic-cl-web
  (:use #:cl #:alexandria)
  (:export
   ;; Server management
   #:start-web-server
   #:stop-web-server
   #:web-server-running-p
   #:quick-start

   ;; PLC thread control
   #:start-plc-thread
   #:stop-plc-thread
   #:plc-thread-running-p

   ;; Configuration
   #:*web-acceptor*
   #:*plc-interpreter*
   #:*plc-thread*
   #:*static-directory*

   ;; Ladder rendering
   #:network-to-ladder
   #:program-to-ladder
   #:instruction-to-ladder-symbol))
