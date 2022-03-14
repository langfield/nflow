(load "~/.quicklisp/setup.lisp")
(ql:quickload :str)
(ql:quickload :numcl)
(ql:quickload :for)
(ql:quickload :draw-cons-tree)

(defun get-file (filename)
  " Read in a file. "
  (with-open-file (stream filename)
    (loop for line = (read-line stream nil)
      while line
      collect line)))

(defun pretty-print (name lst)
  " Print each element of LST on a line of its own. "
  (format t "~A:~%" name)
  (loop while lst do
    (format t "~A~%" (car lst))
    (setq lst (cdr lst)))
  (terpri))

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

(defun get-position-of-empty-line (lines)
  " Get the index of a guaranteed-to-uniquely-exist empty line. "
  (position "" lines :test #'string=))

(defun get-lines-above-delimiter (lines)
  " Get lines above a guaranteed-to-uniquely-exist delimiter. "
  (let*
    ((position-of-empty-line (get-position-of-empty-line lines)))
    (slice lines 0 position-of-empty-line)))

(defun get-lines-below-delimiter (lines)
  " Get lines below a guaranteed-to-uniquely-exist delimiter. "
  (let*
    ((position-of-empty-line (get-position-of-empty-line lines))
     (num-lines-below-delimiter (- (- (length lines) position-of-empty-line) 1)))
    (slice lines (+ position-of-empty-line 1) num-lines-below-delimiter)))

(defun get-indent-level (s)
  " Count leading spaces of a string. "
  (let*
    ((left-trimmed-s (str:trim-left s)))
    (- (length s) (length left-trimmed-s))))

(defun main (argv)
  " Entry point of program. "
  (let*
    ((lines (get-file (nth 1 argv)))
     (lines-below-delimiter (get-lines-below-delimiter lines))
     (indent-levels (mapcar 'get-indent-level lines-below-delimiter))
     (max-indent-level (apply 'max indent-levels)))
    (pretty-print "lines" lines)
    (pretty-print "indent levels" indent-levels)
    (format t "max indent level: ~A~%" max-indent-level)))
