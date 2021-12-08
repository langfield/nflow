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
    (print (list "Mismatch expression:" (mismatch str prefix)))
    (print (list "Equal expression:" (equal (mismatch str prefix) NIL)))
    (print (list ">= expression:" (>= (mismatch str prefix) 2)))
    (cond
      ; If STR and PREFIX match exactly, then MISMATCH returns NIL, so we must
      ; return T.
      ((equal (mismatch str prefix) NIL) (print "strict equality branch"))
      ; Return T if PREFIX is a substring of STR.
      ((>= (mismatch str prefix) 2) (print "substring branch"))
      ; Return NIL otherwise.
      (t (print "else branch")))))

; Main function.
(defun main (argv)
  (print (list "'- hello' starts with '- ':" (funcall (starts-with "- ") "- hello")))
  (print (list "'hello' starts with '- ':" (funcall (starts-with "- ") "hello")))
  (write-line ""))
  ; (print-list (get-file (nth 1 argv)))
  ; (print-list (remove-if (starts-with "- ") (get-file (nth 1 argv)))))
