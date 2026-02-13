(ql:quickload :mblogic-cl/web :silent t)

(let* ((source "NETWORK 1
STR X1
STR X2
CNTU CT1 100
")
       (program (mblogic-cl:parse-il-string source))
       (network (first (mblogic-cl:program-main-networks program)))
       (rung (mblogic-cl-web::network-to-ladder-rung network)))
  (format t "Rung rows: ~D~%" (mblogic-cl-web::ladder-rung-rows rung))
  (format t "Cells:~%")
  (dolist (cell (mblogic-cl-web::ladder-rung-cells rung))
    (format t "  (~D,~D): ~A ~A~%"
            (mblogic-cl-web::ladder-cell-row cell)
            (mblogic-cl-web::ladder-cell-col cell)
            (mblogic-cl-web::ladder-cell-symbol cell)
            (mblogic-cl-web::ladder-cell-address cell))))

(sb-ext:quit)
