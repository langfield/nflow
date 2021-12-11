(load "~/.quicklisp/setup.lisp")
(ql:quickload :str)

(defun get-file (filename)
  " Read in a file. "
  (with-open-file (stream filename)
    (loop for line = (read-line stream nil)
      while line
      collect line)))

(defun print-list (printable-list)
  " Print an arbitrary list. "
  (format t "~{~A ~}~%" printable-list))

(defun starts-with (prefix)
  " Check whether STR starts with PREFIX. "
  (defun starts-with-prefix (str)
    (cond
      ; If STR and PREFIX match exactly, then MISMATCH returns NIL, so we must
      ; return T.
      ((equal (mismatch str prefix) NIL) t)
      ; Return T if PREFIX is a substring of STR.
      ((>= (mismatch str prefix) 2) t)
      ; Return NIL otherwise.
      (t NIL))))

(defun get-dashed-and-undashed-lines (lines)
  " Returns a list of the form ``(<undashed-lines> <dashed-lines>)``. "
  (list (remove-if (starts-with "- ") lines) (remove-if-not (starts-with "- ") lines)))

(defun get-n-items (lst num)
  " Get ``lst[:num]``. "
  (if (> num 0)
    (cons (car lst) (get-n-items (cdr lst) (- num 1)))
      '()))

(defun slice (lst start size)
  " Take a slice of ``lst`` of the form ``lst[start:start + size]``. "
  (if (> start 0)
    (slice (cdr lst) (- start 1) size)
    (get-n-items lst size)))

(defun print-elements-of-list (name lst)
  " Print each element of LIST on a line of its own. "
  (format t "~A:~%" name)
  (loop while lst do
    (format t "~A~%" (car lst))
    (setq lst (cdr lst)))
  (terpri))

(defun main (argv)
  " Main function. "
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
; Put the new list back in the file.
