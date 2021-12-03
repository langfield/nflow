; Read in a file.
(defun get-file (filename)
  (with-open-file (stream filename)
    (loop for line = (read-line stream nil)
          while line
          collect line)))

; Print an arbitrary list.
(defun print-list (list)
 (format t "~{~A ~}~%" list))

; Main function.
(defun main (argv)
  (write-line "hello world")
  (print-list argv)
  (setf readme-lines (get-file (nth 1 argv)))
  (print-list readme-lines)
  )
