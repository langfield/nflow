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
      (t NIL))))

; Returns a list of the form ``(<non-dashed-lines> <dashed-lines>)``.
(defun get-dashed-and-non-dashed-lines (lines)
  (list (remove-if (starts-with "- ") lines) (remove-if-not (starts-with "- ") lines)))

; Main function.
(defun main (argv)
  (print-list (get-dashed-and-non-dashed-lines (get-file (nth 1 argv)))))
  ; Read the whole file in.
  ; Get all elements after the empty line that start with "- ".
  ; Get all elements after the empty line that do not start with "- ".
  ; Move the dashed lines to just before the empty line in the sequence.
  ; Put the new list back on the file.
