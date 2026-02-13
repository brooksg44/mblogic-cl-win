;;; Debug functions for ladder rendering
;;; Contains debug version of network-to-ladder-rung

(defun debug-network-to-ladder-rung (network)
  "Debug version of network-to-ladder-rung that prints matrix state"
  (let ((instructions (mblogic-cl:network-instructions network))
        (all-addresses nil)
        ;; Input matrix processing
        (current-matrix (list (list)))  ; Start with one empty row
        (matrix-stack (list))           ; Stack for STR blocks
        ;; Output collection
        (output-cells nil)
        ;; First STR flag - don't push empty matrix on first STR
        (first-store t))

    ;; Debug output
    (format t "~%=== DEBUG: Starting network-to-ladder-rung ===~%")
    (format t "DEBUG: Network instructions: ~S~%" instructions)
    (format t "DEBUG: Initial current-matrix: ~S~%" current-matrix)
    (format t "DEBUG: Initial matrix-stack: ~S~%" matrix-stack)

    ;; Separate inputs from outputs
    (let ((inputs nil)
          (outputs nil))
      (dolist (instr instructions)
        (let ((opcode (mblogic-cl:parsed-opcode instr)))
          (cond
            ((coil-instruction-p opcode)
             (push instr outputs))
            ((control-instruction-p opcode)
             (push instr outputs))
            ((output-block-instruction-p opcode)
             (push instr outputs))
            (t
             (push instr inputs)))))
      (setf inputs (nreverse inputs))
      (setf outputs (nreverse outputs))

      ;; Debug output
      (format t "DEBUG: Separated ~D input instructions~%" (length inputs))
      (format t "DEBUG: Separated ~D output instructions~%" (length outputs))
      (format t "DEBUG: First input instruction: ~S~%" (when inputs (first inputs)))

      ;; Process input instructions using matrix algorithm
      (dolist (instr inputs)
        (let* ((opcode (mblogic-cl:parsed-opcode instr))
               (cell (instruction-to-cell instr 0)))

          ;; Collect addresses
          (dolist (addr (ladder-cell-addresses cell))
            (pushnew addr all-addresses :test #'string-equal))

          (cond
            ;; STR instruction - start new logic block
            ((store-instruction-p opcode)
             (if first-store
                 ;; First STR - just add to current matrix
                 (progn
                   (setf first-store nil)
                   (setf current-matrix (append-cell-to-matrix cell current-matrix)))
                 ;; Subsequent STR - push current and start new
                 (progn
                   (push current-matrix matrix-stack)
                   (setf current-matrix (list (list)))
                   (setf current-matrix (append-cell-to-matrix cell current-matrix)))))

            ;; AND instruction - append to current row
            ((and-instruction-p opcode)
             (setf current-matrix (append-cell-to-matrix cell current-matrix)))

            ;; OR instruction - create parallel branch below, then close block
            ;; This matches Python algorithm: merge below, then add right-side closing connectors
            ((or-instruction-p opcode)
             (let ((new-matrix (list (list))))
               (setf new-matrix (append-cell-to-matrix cell new-matrix))
               (setf current-matrix (merge-matrix-below current-matrix new-matrix))
               (setf current-matrix (close-branch-block current-matrix))))

            ;; ORSTR - pop and merge below, then add right-side closing connectors
            ;; This matches Python algorithm: merge below, then close-branch-block
            ((orstr-instruction-p opcode)
             (when matrix-stack
               (let ((old-matrix (pop matrix-stack)))
                 (setf current-matrix (merge-matrix-below old-matrix current-matrix))
                 (setf current-matrix (close-branch-block current-matrix)))))

            ;; Other instructions - add to current row
            (t
             (setf current-matrix (append-cell-to-matrix cell current-matrix)))))

      ;; Debug output after processing inputs
      (format t "DEBUG: After processing inputs - current-matrix: ~S~%" current-matrix)
      (format t "DEBUG: After processing inputs - matrix-stack: ~S~%" matrix-stack)
      (format t "DEBUG: After processing inputs - output-cells: ~S~%" output-cells)

      ;; Build the rung (no longer needs branch metadata - it's in the cells)
      (make-ladder-rung
       :number (mblogic-cl:network-number network)
       :cells (append (nreverse (first current-matrix)) (nreverse output-cells))
       :rows (max (length current-matrix) (length output-cells) 1)
       :cols (matrix-width current-matrix)
       :addresses (nreverse all-addresses)
       :comment (first (mblogic-cl:network-comments network))
        :branches nil           ; No longer needed - explicit cells
        :output-branches nil))))