;;;; Debug multi-row handling - detailed
(ql:quickload :mblogic-cl/web :silent t)

(let* ((source "NETWORK 1
STR X1
STR X2
CNTU CT1 100
")
       (program (mblogic-cl:parse-il-string source))
       (network (first (mblogic-cl:program-main-networks program)))
       (instructions (mblogic-cl:network-instructions network))
       (inputs (remove-if (lambda (i) 
                            (member (mblogic-cl:parsed-opcode i) 
                                    '("OUT" "SET" "RST" "PD" "CNTU")
                                    :test #'string=))
                          instructions))
       (current-matrix (list (list)))
       (matrix-stack (list)))
  
  ;; Simulate processing
  (dolist (instr inputs)
    (let ((opcode (mblogic-cl:parsed-opcode instr)))
      (cond
        ((member opcode '("STR" "STRN") :test #'string=)
         (push current-matrix matrix-stack)
         (setf current-matrix (list (list)))
         (let ((cell (mblogic-cl-web::instruction-to-cell instr 0)))
           (setf current-matrix (mblogic-cl-web::append-cell-to-matrix cell current-matrix))))
        (t
         (let ((cell (mblogic-cl-web::instruction-to-cell instr 0)))
           (setf current-matrix (mblogic-cl-web::append-cell-to-matrix cell current-matrix)))))))
  
  ;; Check what we have
  (format t "Before multi-row handling:~%")
  (format t "  Stack len: ~D~%" (length matrix-stack))
  (dotimes (i (length matrix-stack))
    (let ((matrix (nth i matrix-stack)))
      (format t "  Stack[~D]: ~D rows~%" i (length matrix))
      (dotimes (r (length matrix))
        (let ((row (nth r matrix)))
          (format t "    Row ~D: ~D cells - " r (length row))
          (dolist (cell row)
            (format t "~A " (if cell (mblogic-cl-web::ladder-cell-symbol cell) "-")))
          (format t "~%")))))
  
  (format t "  Current: ~D rows~%" (length current-matrix))
  (dotimes (r (length current-matrix))
    (let ((row (nth r current-matrix)))
      (format t "    Row ~D: ~D cells - " r (length row))
      (dolist (cell row)
        (format t "~A " (if cell (mblogic-cl-web::ladder-cell-symbol cell) "-")))
      (format t "~%")))
  
  ;; Apply multi-row handling
  (let ((stack-len (length matrix-stack)))
    (when (= stack-len 2)
      (let ((last-matrix (nth (1- stack-len) matrix-stack)))
        (format t "~%Appending last-matrix (~D rows) to current (~D rows)~%" 
                (length last-matrix) (length current-matrix))
        (setf current-matrix (append last-matrix current-matrix))
        (format t "Result: ~D rows~%" (length current-matrix))
        (dotimes (r (length current-matrix))
          (let ((row (nth r current-matrix)))
            (format t "  Row ~D: ~D cells - " r (length row))
            (dolist (cell row)
              (format t "~A " (if cell (mblogic-cl-web::ladder-cell-symbol cell) "-")))
            (format t "~%"))))))

(sb-ext:quit)