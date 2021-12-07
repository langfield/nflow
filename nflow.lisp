; Read in a file.
(defun get-file (filename)
  (with-open-file (stream filename)
    (loop for line = (read-line stream nil)
          while line
          collect line)))

; Print an arbitrary list.
(defun print-list (printable-list)
  (format t "~{~A ~}~%" printable-list))

; Check whether STR starts with PREFIX.
(defun starts-with (prefix)
  (defun starts-with-prefix (str)
    (cond
      ; If STR and PREFIX match exactly, then MISMATCH returns NIL, so we must
      ; return T.
      ((equal (mismatch str prefix) NIL) t)
      ; Return T if PREFIX is a substring of STR.
      ((>= (mismatch str prefix) 2) t)
      ; Return NIL otherwise.
      (t) (NIL))))

; Main function.
(defun main (argv)
  (print-list (get-file (nth 1 argv)))
  (terpri)
  (terpri)
  (print-list (remove-if (starts-with "- ") (get-file (nth 1 argv)))))
