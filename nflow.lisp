(load "~/.quicklisp/setup.lisp")
(ql:quickload :str)

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

; Returns a list of the form ``(<undashed-lines> <dashed-lines>)``.
(defun get-dashed-and-undashed-lines (lines)
  (list (remove-if (starts-with "- ") lines) (remove-if-not (starts-with "- ") lines)))

; Get ``lst[:num]``.
(defun get-n-items (lst num)
  (if (> num 0)
    (cons (car lst) (get-n-items (cdr lst) (- num 1)))
      '()))

; Take a slice of ``lst`` of the form ``lst[start:start + size]``.
(defun slice (lst start size)
  (if (> start 0)
    (slice (cdr lst) (- start 1) size)
    (get-n-items lst size)))

; Print each element of a list on a new line.
(defun print-elements-of-list (name lst)
  " Print each element of LIST on a line of its own. "
  (format t "~A:~%" name)
  (loop while lst do
    (format t "~A~%" (car lst))
    (setq lst (cdr lst)))
  (terpri))

; Main function.
(defun main (argv)
  (let*
    ((lines (get-file (nth 1 argv)))
    (position-of-empty-line (position "" lines :test #'string=))
    (dashed-undashed-pair (get-dashed-and-undashed-lines lines))
    (undashed-lines (cdr (car dashed-undashed-pair)))
    (lines-above-delimeter (slice lines 0 position-of-empty-line))
    (num-lines-below-delimeter (- (- (length lines) position-of-empty-line) 1))
    (lines-below-delimeter (slice lines (+ position-of-empty-line 1) num-lines-below-delimeter))
    (dashed-lines-below-delimeter (nth 0 (cdr (get-dashed-and-undashed-lines lines-below-delimeter))))
    (reflowed-lines (concatenate 'list lines-above-delimeter dashed-lines-below-delimeter '("") undashed-lines)))
  (terpri)
  (print-elements-of-list "lines" lines)
  (print-elements-of-list "reflowed-lines" reflowed-lines)))


; OUTLINE
; -------
; Read the whole file in. (DONE)
; Get all elements after the empty line that start with "- ". (DONE)
; Get all elements after the empty line that do not start with "- ". (DONE)
; Move the dashed lines to just before the empty line in the sequence. (DONE)
; Put the new list back on the file.
