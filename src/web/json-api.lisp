;;;; src/web/json-api.lisp
;;;;
;;;; JSON API Response Generation
;;;; Functions to generate JSON responses for the web API endpoints

(in-package #:mblogic-cl-web)

;;; ============================================================
;;; JSON Serialization Helpers
;;; ============================================================

(defun plist-to-json (plist)
  "Convert a plist to JSON string using cl-json.
   Handles nested plists and lists correctly."
  ;; Convert plist to alist for proper JSON encoding
  (let ((alist (plist-to-alist plist)))
    (cl-json:encode-json-alist-to-string alist)))

(defun string-keyed-alist-p (value)
  "Check if VALUE is an alist with string keys (e.g., matrixdata)."
  (and (listp value)
       (not (null value))
       (consp (first value))
       (stringp (car (first value)))))

(defun plist-p (value)
  "Check if VALUE looks like a plist (keyword followed by value pairs)."
  (and (listp value)
       (keywordp (first value))
       (> (length value) 1)
       (evenp (length value))))

(defun list-of-plists-p (value)
  "Check if VALUE is a list of plists."
  (and (listp value)
       (not (null value))
       (listp (first value))
       (plist-p (first value))))

(defun convert-value-for-json (value)
  "Convert a value for JSON encoding, handling plists, alists, and nested structures."
  (cond
    ;; String-keyed alist (like matrixdata) - convert to alist with converted values
    ((string-keyed-alist-p value)
     (mapcar (lambda (pair)
               (cons (car pair)  ; Keep string key as-is
                     (convert-value-for-json (cdr pair))))
             value))
    ;; Nested alist with keyword keys (like cell data) - recursively convert
    ((and (listp value)
          (not (null value))
          (consp (first value))
          (keywordp (car (first value))))
     (mapcar (lambda (pair)
               (cons (car pair)  ; Keep keyword key
                     (convert-value-for-json (cdr pair))))
             value))
    ;; Nested plist - recursively convert
    ((plist-p value)
     (plist-to-alist value))
    ;; List of plists - convert each
    ((list-of-plists-p value)
     (mapcar #'plist-to-alist value))
    ;; Ladder-cell struct - convert to plist format
    ;; Use find-class to check if value is a ladder-cell
    ((let ((cell-class (find-class 'ladder-cell nil)))
       (when cell-class
         (typep value cell-class)))
     (list :type (string-downcase (symbol-name (slot-value value 'type)))
           :value (slot-value value 'symbol)
           :addr (or (slot-value value 'addresses) (list ""))))
    ;; Plain list or atom - return as-is
    (t value)))

(defun plist-to-alist (plist)
  "Convert a plist to an alist recursively.
   Also handles string-keyed alists (like matrixdata) by converting their values."
  (if (null plist)
      nil
      (loop for (key value) on plist by #'cddr
            collect (cons (intern (string key) :keyword)
                         (convert-value-for-json value)))))

(defun alist-to-json (alist)
  "Convert an alist to JSON string"
  (cl-json:encode-json-alist-to-string alist))

(defun list-to-json (list)
  "Convert a list to JSON array string"
  (cl-json:encode-json-to-string list))

;;; ============================================================
;;; Statistics Response
;;; ============================================================

(defun statistics-response (interpreter)
  "Generate statistics JSON response"
  (if interpreter
      (let ((stats (mblogic-cl:interpreter-statistics interpreter)))
        (plist-to-json
         (list :running (mblogic-cl:interpreter-running-p interpreter)
               :scan-count (mblogic-cl:interpreter-scan-count interpreter)
               :scan-time (mblogic-cl:interpreter-scan-time interpreter)
               :exit-code (when (mblogic-cl:interpreter-exit-code interpreter)
                           (string-downcase
                            (symbol-name (mblogic-cl:interpreter-exit-code interpreter))))
               :total-scans (mblogic-cl:stats-total-scans stats)
               :min-scan-time (let ((min-t (mblogic-cl:stats-min-scan-time stats)))
                               (if (= min-t most-positive-fixnum) 0 min-t))
               :max-scan-time (mblogic-cl:stats-max-scan-time stats)
               :avg-scan-time (mblogic-cl:average-scan-time stats))))
      (plist-to-json
       (list :running nil
             :scan-count 0
             :scan-time 0
             :error "No interpreter loaded"))))

;;; ============================================================
;;; Data Values Response
;;; ============================================================

(defun get-address-value (interpreter address)
  "Get value of an address from interpreter's data table.
   Returns (values value type) where type is :bool, :word, :float, :string or nil."
  (let ((dt (mblogic-cl:interpreter-data-table interpreter)))
    (cond
      ((mblogic-cl:bool-addr-p address)
       (values (mblogic-cl:get-bool dt address) :bool))
      ((mblogic-cl:word-addr-p address)
       (values (mblogic-cl:get-word dt address) :word))
      ((mblogic-cl:float-addr-p address)
       (values (mblogic-cl:get-float dt address) :float))
      ((mblogic-cl:string-addr-p address)
       (values (mblogic-cl:get-string dt address) :string))
      (t (values nil nil)))))

(defun format-json-value (value type)
  "Format a value for JSON based on its type."
  (case type
    (:bool (if value "true" "false"))
    (:word (format nil "~D" (or value 0)))
    (:float (format nil "~F" (or value 0.0)))
    (:string (format nil "~S" (or value "")))
    (t "null")))

(defun data-response (interpreter addresses)
  "Generate data values JSON response.
   ADDRESSES is a list of address strings."
  (if interpreter
      ;; Build JSON manually to handle boolean false correctly
      (with-output-to-string (s)
        (write-char #\{ s)
        (let ((first t))
          (dolist (addr addresses)
            (multiple-value-bind (value type) (get-address-value interpreter addr)
              (if first
                  (setf first nil)
                  (write-char #\, s))
              (format s "~S:~A" addr (format-json-value value type)))))
        (write-char #\} s))
      (plist-to-json (list :error "No interpreter loaded"))))

(defun parse-address-list (addr-string)
  "Parse comma-separated address string into list"
  (when (and addr-string (> (length addr-string) 0))
    (mapcar (lambda (s) (string-trim '(#\Space #\Tab) s))
            (cl-ppcre:split "," addr-string))))

;;; ============================================================
;;; Program Structure Response
;;; ============================================================

(defun program-response (interpreter subrname &key (format :matrixdata))
  "Generate program structure JSON response.
   Returns ladder diagram matrix for the named subroutine.
   FORMAT can be :matrixdata (Python-compatible, default) or :legacy."
  (if interpreter
      (let* ((program (mblogic-cl:interpreter-program interpreter))
             (source (when program
                      (mblogic-cl:program-source program))))
        (if source
            (let ((ladder (program-to-ladder source (or subrname "main"))))
              (if ladder
                  (if (eq format :matrixdata)
                      ;; New Python-compatible format with explicit branch cells
                      (plist-to-json
                       (list :subrname (ladder-program-name ladder)
                             :addresses (ladder-program-addresses ladder)
                             :subrdata (mapcar #'rung-to-matrixdata
                                              (ladder-program-rungs ladder))))
                      ;; Legacy format (kept for backwards compatibility)
                      (plist-to-json (ladder-program-to-plist ladder)))
                  (plist-to-json
                   (list :error (format nil "Subroutine '~A' not found" subrname)))))
            (plist-to-json (list :error "No program source available"))))
      (plist-to-json (list :error "No interpreter loaded"))))

;;; ============================================================
;;; JavaScript-Compatible Program Response (demodata.js format)
;;; ============================================================

(defun program-js-response (interpreter subrname)
  "Generate JavaScript-compatible ladder JSON (demodata.js format).
   This format is expected by the MBLogic ladtest JavaScript files."
   (if interpreter
       (let* ((program (mblogic-cl:interpreter-program interpreter))
              (source (when program (mblogic-cl:program-source program))))
         (if source
             (let ((ladder (program-to-ladder source (or subrname "main"))))
               (if ladder
                   ;; Convert alist recursively before encoding
                   (let ((converted (convert-value-for-json
                                    (ladder-program-to-js-format ladder))))
                     (cl-json:encode-json-alist-to-string converted))
                   (plist-to-json
                    (list :error (format nil "Subroutine '~A' not found" subrname)))))
             (plist-to-json (list :error "No program source available"))))
       (plist-to-json (list :error "No interpreter loaded"))))

;;; ============================================================
;;; Subroutine List Response
;;; ============================================================

(defun subroutine-list-response (interpreter)
  "Generate list of available subroutines"
  (if interpreter
      (let* ((program (mblogic-cl:interpreter-program interpreter))
             (source (when program
                      (mblogic-cl:program-source program))))
        (if source
            (plist-to-json
             (list :subroutines (list-subroutine-names source)))
            (plist-to-json (list :subroutines '("main")))))
      (plist-to-json (list :error "No interpreter loaded"))))

;;; ============================================================
;;; Control Response
;;; ============================================================

(defun control-response (action success &optional message)
  "Generate control action response"
  (plist-to-json
   (list :action action
         :success success
         :message (or message (if success "OK" "Failed")))))

;;; ============================================================
;;; Error Response
;;; ============================================================

(defun error-response (message &optional (code 400))
  "Generate error JSON response"
  (values
   (plist-to-json (list :error message :code code))
   code))

;;; ============================================================
;;; Address Monitoring Helpers
;;; ============================================================

(defun get-all-monitored-addresses (interpreter subrname)
  "Get list of all addresses that should be monitored for a subroutine"
  (if interpreter
      (let* ((program (mblogic-cl:interpreter-program interpreter))
             (source (when program
                      (mblogic-cl:program-source program))))
        (when source
          (let ((ladder (program-to-ladder source (or subrname "main"))))
            (when ladder
              (ladder-program-addresses ladder)))))
      nil))

;;; End of json-api.lisp
