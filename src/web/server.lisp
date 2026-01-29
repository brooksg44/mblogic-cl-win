;;;; src/web/server.lisp
;;;;
;;;; Hunchentoot Web Server Setup
;;;; Provides HTTP endpoints for ladder diagram visualization and PLC control

(in-package #:mblogic-cl-web)

;;; ============================================================
;;; Global State
;;; ============================================================

(defvar *web-acceptor* nil
  "The Hunchentoot acceptor instance")

(defvar *plc-interpreter* nil
  "The PLC interpreter being served")

(defvar *plc-thread* nil
  "Background thread running the PLC interpreter")

(defvar *plc-lock* (bt:make-lock "plc-lock")
  "Lock for thread-safe PLC operations")

(defvar *static-directory* nil
  "Directory for static files (HTML, CSS, JS)")

;;; ============================================================
;;; Static File Handling
;;; ============================================================

(defun find-static-directory ()
  "Find the static files directory relative to this source file"
  (let* ((this-file (or *compile-file-truename* *load-truename*))
         (src-web-dir (when this-file (directory-namestring this-file)))
         (project-root (when src-web-dir
                        (merge-pathnames "../../" src-web-dir)))
         (static-dir (when project-root
                      (merge-pathnames "static/" project-root))))
    ;; Try the computed path first
    (if (and static-dir (probe-file static-dir))
        static-dir
        ;; Fallback: try relative to current directory
        (let ((cwd-static (merge-pathnames "static/" (uiop:getcwd))))
          (when (probe-file cwd-static)
            cwd-static)))))

(defun initialize-static-directory ()
  "Initialize the static directory path"
  (unless *static-directory*
    (setf *static-directory* (find-static-directory))))

(defun serve-static-file (path)
  "Serve a static file with appropriate content type"
  (handler-case
      (progn
        (initialize-static-directory)
        (if (null *static-directory*)
            (progn
              (setf (hunchentoot:return-code*) hunchentoot:+http-internal-server-error+)
              (format nil "Static directory not configured"))
            (let* ((full-path (merge-pathnames path *static-directory*))
                   (extension (pathname-type full-path)))
              (if (probe-file full-path)
                  (progn
                    (setf (hunchentoot:content-type*)
                          (cond
                            ((string-equal extension "html") "text/html; charset=utf-8")
                            ((string-equal extension "css") "text/css; charset=utf-8")
                            ((string-equal extension "js") "application/javascript; charset=utf-8")
                            ((string-equal extension "json") "application/json; charset=utf-8")
                            ((string-equal extension "svg") "image/svg+xml")
                            ((string-equal extension "png") "image/png")
                            (t "application/octet-stream")))
                    (hunchentoot:handle-static-file full-path))
                  (progn
                    (setf (hunchentoot:return-code*) hunchentoot:+http-not-found+)
                    (format nil "File not found: ~A" path))))))
    (error (e)
      (format *error-output* "Error serving static file ~A: ~A~%" path e)
      (setf (hunchentoot:return-code*) hunchentoot:+http-internal-server-error+)
      (format nil "Internal server error"))))

;;; ============================================================
;;; PLC Thread Management
;;; ============================================================

(defun start-plc-thread ()
  "Start the PLC interpreter in a background thread"
  (bt:with-lock-held (*plc-lock*)
    (when (and *plc-interpreter* (not *plc-thread*))
      (setf *plc-thread*
            (bt:make-thread
             (lambda ()
               (handler-case
                   (mblogic-cl:run-continuous *plc-interpreter*
                                              :target-scan-time 10)
                 (error (e)
                   (format *error-output* "PLC thread error: ~A~%" e))))
             :name "plc-interpreter"))
      t)))

(defun stop-plc-thread ()
  "Stop the PLC interpreter thread"
  (bt:with-lock-held (*plc-lock*)
    (when *plc-interpreter*
      (mblogic-cl:stop-interpreter *plc-interpreter*))
    (when *plc-thread*
      ;; Give thread time to stop gracefully
      (sleep 0.1)
      (setf *plc-thread* nil))
    t))

(defun plc-thread-running-p ()
  "Check if PLC thread is running"
  (and *plc-thread*
       *plc-interpreter*
       (mblogic-cl:interpreter-running-p *plc-interpreter*)))

(defun step-plc ()
  "Execute a single PLC scan"
  (bt:with-lock-held (*plc-lock*)
    (when *plc-interpreter*
      (mblogic-cl:step-scan *plc-interpreter*)
      t)))

;;; ============================================================
;;; API Route Handlers
;;; ============================================================

(defun handle-api-statistics ()
  "Handle GET /api/statistics"
  (handler-case
      (progn
        (setf (hunchentoot:content-type*) "application/json")
        (statistics-response *plc-interpreter*))
    (error (e)
      (format *error-output* "Error in handle-api-statistics: ~A~%" e)
      (setf (hunchentoot:return-code*) hunchentoot:+http-internal-server-error+)
      (setf (hunchentoot:content-type*) "application/json")
      (plist-to-json (list :error (format nil "~A" e))))))

(defun handle-api-data ()
  "Handle GET /api/data?addr=X1,Y1,DS1"
  (handler-case
      (progn
        (setf (hunchentoot:content-type*) "application/json")
        (let ((addr-param (hunchentoot:get-parameter "addr")))
          (if addr-param
              (data-response *plc-interpreter* (parse-address-list addr-param))
              (plist-to-json (list :error "Missing 'addr' parameter")))))
    (error (e)
      (format *error-output* "Error in handle-api-data: ~A~%" e)
      (setf (hunchentoot:return-code*) hunchentoot:+http-internal-server-error+)
      (setf (hunchentoot:content-type*) "application/json")
      (plist-to-json (list :error (format nil "~A" e))))))

(defun handle-api-program ()
  "Handle GET /api/program?subrname=main"
  (handler-case
      (progn
        (setf (hunchentoot:content-type*) "application/json")
        (let ((subrname (or (hunchentoot:get-parameter "subrname") "main")))
          (program-response *plc-interpreter* subrname)))
    (error (e)
      (format *error-output* "Error in handle-api-program: ~A~%" e)
      (setf (hunchentoot:return-code*) hunchentoot:+http-internal-server-error+)
      (setf (hunchentoot:content-type*) "application/json")
      (plist-to-json (list :error (format nil "~A" e))))))

(defun handle-api-subroutines ()
  "Handle GET /api/subroutines"
  (setf (hunchentoot:content-type*) "application/json")
  (subroutine-list-response *plc-interpreter*))

(defun handle-api-control-start ()
  "Handle POST /api/control/start"
  (setf (hunchentoot:content-type*) "application/json")
  (if *plc-interpreter*
      (if (plc-thread-running-p)
          (control-response "start" t "Already running")
          (progn
            (start-plc-thread)
            (control-response "start" t)))
      (control-response "start" nil "No interpreter loaded")))

(defun handle-api-control-stop ()
  "Handle POST /api/control/stop"
  (setf (hunchentoot:content-type*) "application/json")
  (if *plc-interpreter*
      (progn
        (stop-plc-thread)
        (control-response "stop" t))
      (control-response "stop" nil "No interpreter loaded")))

(defun handle-api-control-step ()
  "Handle POST /api/control/step"
  (setf (hunchentoot:content-type*) "application/json")
  (if *plc-interpreter*
      (if (plc-thread-running-p)
          (control-response "step" nil "Stop continuous mode first")
          (progn
            (step-plc)
            (control-response "step" t)))
      (control-response "step" nil "No interpreter loaded")))

;;; ============================================================
;;; Route Dispatcher
;;; ============================================================

(defun create-dispatch-table ()
  "Create the URL dispatch table"
  (list
   ;; API routes
   (hunchentoot:create-prefix-dispatcher "/api/statistics" #'handle-api-statistics)
   (hunchentoot:create-prefix-dispatcher "/api/data" #'handle-api-data)
   (hunchentoot:create-prefix-dispatcher "/api/program" #'handle-api-program)
   (hunchentoot:create-prefix-dispatcher "/api/subroutines" #'handle-api-subroutines)
   (hunchentoot:create-prefix-dispatcher "/api/control/start" #'handle-api-control-start)
   (hunchentoot:create-prefix-dispatcher "/api/control/stop" #'handle-api-control-stop)
   (hunchentoot:create-prefix-dispatcher "/api/control/step" #'handle-api-control-step)

   ;; Static file routes
   (hunchentoot:create-prefix-dispatcher "/css/"
    (lambda () (serve-static-file (format nil "css/~A"
                                          (subseq (hunchentoot:script-name*) 5)))))
   (hunchentoot:create-prefix-dispatcher "/js/"
    (lambda () (serve-static-file (format nil "js/~A"
                                          (subseq (hunchentoot:script-name*) 4)))))

   ;; HTML pages
   (hunchentoot:create-prefix-dispatcher "/laddermonitor.html"
    (lambda () (serve-static-file "laddermonitor.html")))
   (hunchentoot:create-prefix-dispatcher "/index.html"
    (lambda () (serve-static-file "index.html")))

   ;; Root redirect
   (hunchentoot:create-prefix-dispatcher "/"
    (lambda ()
      (if (string= (hunchentoot:script-name*) "/")
          (progn
            (setf (hunchentoot:return-code*) hunchentoot:+http-moved-temporarily+)
            (setf (hunchentoot:header-out :location) "/laddermonitor.html")
            "Redirecting...")
          (serve-static-file (subseq (hunchentoot:script-name*) 1)))))))

(defun install-dispatch-table ()
  "Install our dispatch table into Hunchentoot's global dispatch table"
  (setf hunchentoot:*dispatch-table* (create-dispatch-table)))

;;; ============================================================
;;; Server Management
;;; ============================================================

(defun start-web-server (&key (port 8080) interpreter)
  "Start the web server.
   PORT: HTTP port to listen on (default 8080)
   INTERPRETER: PLC interpreter instance to serve"
  (when *web-acceptor*
    (stop-web-server))

  (setf *plc-interpreter* interpreter)
  (initialize-static-directory)

  ;; Install our dispatch table
  (install-dispatch-table)

  ;; Create and configure acceptor
  (setf *web-acceptor*
        (make-instance 'hunchentoot:easy-acceptor
                       :port port
                       :document-root *static-directory*))

  ;; Start the server
  (hunchentoot:start *web-acceptor*)

  (format t "~%MBLogic-CL Web Server started on port ~D~%" port)
  (format t "Open http://localhost:~D/laddermonitor.html in your browser~%" port)
  (when *static-directory*
    (format t "Static files from: ~A~%" *static-directory*))

  *web-acceptor*)

(defun stop-web-server ()
  "Stop the web server"
  ;; Stop PLC thread first
  (stop-plc-thread)

  ;; Stop web server
  (when *web-acceptor*
    (hunchentoot:stop *web-acceptor*)
    (setf *web-acceptor* nil))

  (format t "Web server stopped~%")
  nil)

(defun web-server-running-p ()
  "Check if web server is running"
  (and *web-acceptor*
       (hunchentoot:started-p *web-acceptor*)))

;;; ============================================================
;;; Convenience Functions
;;; ============================================================

(defun quick-start (il-source &key (port 8080))
  "Quickly start a web server with an IL program.
   IL-SOURCE: IL program string or pathname"
  (let* ((compiled (if (pathnamep il-source)
                       (mblogic-cl:compile-il-file il-source)
                       (mblogic-cl:compile-il-string il-source)))
         (interp (mblogic-cl:make-plc-interpreter :program compiled)))
    (start-web-server :port port :interpreter interp)))

;;; End of server.lisp
