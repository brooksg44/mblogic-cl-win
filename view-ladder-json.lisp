;;;; view-ladder-json.lisp
;;;; Simple script to view ladder JSON with nice formatting

(in-package :cl-user)

;; Load systems quietly
(ql:quickload :mblogic-cl :silent t)
(ql:quickload :mblogic-cl/web :silent t)

;; Parse program
(format t "Loading test/plcprog.txt...~%")
(defparameter *source* 
  (mblogic-cl:parse-il-file "test/plcprog.txt"))

;; Show available subroutines
(format t "~%Available subroutines:~%")
(let ((names (mblogic-cl-web::list-subroutine-names *source*)))
  (dolist (name names)
    (format t "  - ~A~%" name)))

;; Function to display ladder summary
(defun show-ladder-summary (subrname)
  "Show a summary of the ladder structure for a subroutine"
  (let* ((ladder (mblogic-cl-web::program-to-ladder *source* subrname))
         (rungs (mblogic-cl-web::ladder-program-rungs ladder))
         (addrs (mblogic-cl-web::ladder-program-addresses ladder)))
    (format t "~%=== Ladder Summary for ~A ===~%" subrname)
    (format t "Total rungs: ~D~%" (length rungs))
    (format t "Total addresses: ~D~%" (length addrs))
    (format t "~%Rungs:~%")
    (dolist (rung rungs)
      (let ((rungnum (mblogic-cl-web::ladder-rung-number rung))
            (rows (mblogic-cl-web::ladder-rung-rows rung))
            (cols (mblogic-cl-web::ladder-rung-cols rung))
            (cells (mblogic-cl-web::ladder-rung-cells rung))
            (comment (mblogic-cl-web::ladder-rung-comment rung)))
        (format t "  Rung ~D: ~Dx~D grid, ~D cells" 
                rungnum rows cols (length cells))
        (when comment
          (format t " - ~A" (subseq comment 0 (min 40 (length comment)))))
        (format t "~%")))))

;; Function to generate and save JSON
(defun generate-json (subrname &optional (filename nil))
  "Generate JSON for a subroutine and optionally save to file"
  (let* ((ladder (mblogic-cl-web::program-to-ladder *source* subrname))
         (plist (mblogic-cl-web::ladder-program-to-plist ladder))
         (json (mblogic-cl-web::plist-to-json plist))
         (output-file (or filename (format nil "~A-output.json" subrname))))
    (format t "~%Generating JSON for ~A...~%" subrname)
    (with-open-file (out output-file
                         :direction :output
                         :if-exists :supersede)
      (write-string json out))
    (format t "Saved to ~A (~D bytes)~%" output-file (length json))
    json))

;; Show LadderDemo summary
(show-ladder-summary "LadderDemo")

;; Show instructions
(format t "~%~%=== How to Use ===~%")
(format t "To view summary of any subroutine:~%")
(format t "  (show-ladder-summary \"SubroutineName\")~%~%")
(format t "To generate JSON for any subroutine:~%")
(format t "  (generate-json \"SubroutineName\")~%~%")
(format t "Examples:~%")
(format t "  (show-ladder-summary \"TankSim\")~%")
(format t "  (generate-json \"PickAndPlace\")~%")
(format t "  (generate-json \"main\" \"main-ladder.json\")~%~%")
