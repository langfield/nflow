; Read in a file.
(defun get-file (filename)
  (with-open-file (stream filename)
    (loop for line = (read-line stream nil)
          while line
          collect line)))

; Print an arbitrary list.
(defun print-list (printable-list)
 (format t "~{~A ~}~%" printable-list))

; Main function.
(defun main (argv)
  (write-line "hello world")
  (print-list argv)
  (print-list (get-file (nth 1 argv))))
