(load "~/.quicklisp/setup.lisp")
(ql:quickload :str)
(ql:quickload :numcl)

(defun get-file (filename)
  " Read in a file. "
  (with-open-file (stream filename)
    (loop for line = (read-line stream NIL)
      while line
      collect line)))

(defun print-list (printable-list)
  " Print an arbitrary list. "
  (format t "~{~A ~}~%" printable-list))

(defun starts-with (prefix)
  " Check whether STR starts with PREFIX. "
  (defun starts-with-prefix (str)
    (str:starts-with? prefix str)))

(defun get-undashed-lines (lines)
  " Returns a list of the undashed lines. "
  (cond
    ((equal (length lines) 0) '())
    (t (remove-if (starts-with "- ") lines))))

(defun get-dashed-lines (lines)
  " Returns a list of the dashed lines. "
  (cond
    ((equal (length lines) 0) '())
    (t (remove-if-not (starts-with "- ") lines))))

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
  " Print each element of LST on a line of its own. "
  (format t "~A:~%" name)
  (loop while lst do
    (format t "~A~%" (car lst))
    (setq lst (cdr lst)))
  (terpri))

(defun dump-lines (lst)
  " Print each element of LST on a line of its own. "
  (loop while lst do
    (format t "~A~%" (car lst))
    (setq lst (cdr lst))))

(defun reflow-dashed-lines (lines)
  " Return the reflowed list of lines, with dashed lines moved above the delimiter, order-preserved. "
  (cond

    ; If LINES has length 0, return an empty list.
    ((equal (length lines) 0) '())

    (t
      (cond

        ; If there is no delimiter, return LINES unchanged.
        ((equal (position "" lines :test #'string=) NIL) lines)

        (t
          (let*
            ; Find the empty line (delimiter).
            ((position-of-empty-line (position "" lines :test #'string=))

            ; Get the undashed lines (excluding the delimiter line).
            (undashed-lines (cdr (get-undashed-lines lines)))

            ; Get lines above delimiter.
            (lines-above-delimiter (slice lines 0 position-of-empty-line))

            ; Get number of lines below delimiter.
            (num-lines-below-delimiter (- (- (length lines) position-of-empty-line) 1))

            ; Get a list of the lines below the delimiter.
            (lines-below-delimiter (slice lines (+ position-of-empty-line 1) num-lines-below-delimiter))

            ; Get only the dashed lines below the delimiter.
            (dashed-lines-below-delimiter (get-dashed-lines lines-below-delimiter)))

          ; Concatenate everything, adding delimiter back in.
          (concatenate 'list lines-above-delimiter dashed-lines-below-delimiter '("") undashed-lines)))))))

(defun main (argv)
  " Main function. "
  (let*
    ; Read in the file.
    ((lines (get-file (nth 1 argv)))
    (reflowed-lines (reflow-dashed-lines lines)))
  (dump-lines reflowed-lines)))


; OUTLINE
; -------
; Read the whole file in. (DONE)
; Get all elements after the empty line that start with "- ". (DONE)
; Get all elements after the empty line that do not start with "- ". (DONE)
; Move the dashed lines to just before the empty line in the sequence. (DONE)
; Put the new list back in the file.
