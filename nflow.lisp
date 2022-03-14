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

(defun in-list (x lst)
  " Check whether X is in LST. "
   (cond
      ((null lst) ())
      ((equal (car lst) x) lst)
      (t (in-list x (cdr lst)))))

(defun subset (lst1 lst2)
  " Check whether every element in LST1 is in LST2. "
   (cond
      ((null lst1) t)
      ((in-list (car lst1) lst2)(subset (cdr lst1) lst2))
      (t ())))

(defun list-equals (lst1 lst2)
  " Check if two lists are equal. "
  (cond
    ((and (subset lst1 lst2) (subset lst2 lst1)) t)
    (t nil)))


(defun make-tree (item)
   "it creates a new node with item."
   (cons (cons item nil) nil))


(defun first-child (tree)
    " Access first child. "
   (if (null tree)
      nil
      (cdr (car tree))))


(defun next-sibling (tree)
  " Get next sibling. "
   (cdr tree))


(defun data (tree)
  " Get data of tree. "
   (car (car tree)))


(defun add-child (tree child)
  " Add child to tree. "
   (setf (car tree) (append (car tree) child))
   tree)


(defun count-leading-spaces (s)
  " Count leading spaces of a string. "
  (let*
    ((left-trimmed-s (str:trim-left s)))
    (- (length s) (length left-trimmed-s))))


(defun unroll-parent-stack (parent-stack num-indents)
  " Go up NUM-INDENTS levels, returning the parent and the parent-stack. "
  (assert (> num-indents 0))
  (let*
    ((parent nil))
    (dotimes (i num-indents)

      ; Get PARENT from PARENT-STACK.
      (setq parent (last parent-stack))

      ; Remove PARENT from PARENT-STACK.
      (setq parent-stack (butlast parent-stack)))
    (list parent-stack parent)))


(defun get-indent-size (delta indent-size)
  " Get the indent size. "
  (if (> indent-size 0)
    indent-size
    (abs delta)))


(defun get-num-indents (indent-level old-indent-level indent-size line)
  " Compute NUM-INDENTS and INDENT-SIZE from the indent levels. "
  (let*
    (
      (indent-delta 0))

    (setq indent-delta (- indent-level old-indent-level))
    (setq indent-size (get-indent-size indent-delta indent-size))
    (if (not (equal (mod indent-delta indent-size) 0))
      (error "Inconsistent indentation on line with contents: ~A~%" line))

    ; Return (NUM-INDENTS, INDENT-SIZE).
    (list (floor indent-delta indent-size) indent-size)))


(defun parse-todo-tree (lst)
  " Parse a todolist into a tree, preserving hierarchy. "
  (let*
    (
      ; Initialize TREE with a dummy root.
      (tree (make-tree "root"))
      (indent-level 0)
      (old-indent-level 0)
      (node tree)
      (indent-size 0)
      (last-created-child nil)
      (parent-stack '())
      (parent nil)
      (parent-stack-pair nil)
      (indent-num-size-pair nil)
      (num-indents 0))

    ; Loop over LST
    (for:for ((line over lst))

      ; If LINE is nonempty:
      (if (not (str:empty? line))
        (progn

          ; Update INDENT-LEVEL.
          (setq old-indent-level indent-level)
          (setq indent-level (count-leading-spaces line))

          ; If INDENT-LEVEL increased:
          (if (> indent-level old-indent-level)
            (progn

              ; Get NUM-INDENTS and INDENT-SIZE.
              (setq indent-num-size-pair (get-num-indents indent-level old-indent-level indent-size line))
              (setq num-indents (first indent-num-size-pair))
              (setq indent-size (second indent-num-size-pair))
              (assert (> num-indents 0))
              (if (> num-indents 1)
                (error "Indent level increased by more than 1: ~A~%" line))

              ; Append NODE to PARENT-STACK.
              (setq parent-stack (append parent-stack node))

              ; Set NODE to LAST-CREATED-CHILD.
              (setq node last-created-child))

            ; Otherwise if INDENT-LEVEL decreased:
            (if (< indent-level old-indent-level)
              (progn

                ; Get NUM-INDENTS and INDENT-SIZE.
                (setq indent-num-size-pair (get-num-indents indent-level old-indent-level indent-size line))
                (setq num-indents (first indent-num-size-pair))
                (setq indent-size (second indent-num-size-pair))
                (assert (< num-indents 0))

                ; Get updated PARENT-STACK and PARENT.
                (setq parent-stack-pair (unroll-parent-stack parent-stack (- num-indents)))
                (setq parent-stack (first parent-stack-pair))
                (setq parent (second parent-stack-pair))

                ; Set NODE to PARENT.
                (setq node parent))))

          ; Add LINE as another child.
          (setq last-created-child (make-tree (str:trim-left line)))
          (setq node (add-child node last-created-child))))

      ; Get a reference to the root.
      (if (> (length parent-stack) 0)
        (setq tree (car parent-stack))
        (setq tree node)))

    ; Draw TREE on each iteration.
    (terpri)
    (draw-cons-tree:draw-tree tree)
    (terpri)

    ; Print cons form of TREE.
    (format t "Tree: ~A~%" tree)
    tree))


(defun check-no-undashed-lines-above-delimiter (lines position-of-empty-line)
    "Check that there are no undashed lines above delimiter."
    (let*
      ((i 0))
      (for:for ((line over lines))
        (if (not (str:starts-with? "- " line))
          (progn
            (if (< i position-of-empty-line)
              (error "Line: '~A' is above delimiter on line: ~A~%" line position-of-empty-line))
            (if (equal i position-of-empty-line)
              (assert (equal line "")))))
        (setq i (1+ i)))
      t))


(defun reflow-nontrivial-lines (lines)
  " Return the reflowed list of lines, with dashed lines moved above the delimiter, order-preserved. "
  (let*
    (
      (position-of-empty-line nil)
      (undashed-lines nil)
      (lines-above-delimiter nil)
      (lines-below-delimiter nil)
      (num-lines-below-delimiter nil)
      (dashed-lines-below-delimiter nil))

    ; Find the empty line (delimiter).
    (setq position-of-empty-line (position "" lines :test #'string=))
    (check-no-undashed-lines-above-delimiter lines position-of-empty-line)

    ; Get the undashed lines (excluding the delimiter line).
    (setq undashed-lines (rest (get-undashed-lines lines)))

    ; Get lines above delimiter.
    (setq lines-above-delimiter (slice lines 0 position-of-empty-line))

    ; Get number of lines below delimiter.
    (setq num-lines-below-delimiter (- (- (length lines) position-of-empty-line) 1))

    ; Get a list of the lines below the delimiter.
    (setq lines-below-delimiter (slice lines (+ position-of-empty-line 1) num-lines-below-delimiter))

    ; Get only the dashed lines below the delimiter.
    (setq dashed-lines-below-delimiter (get-dashed-lines lines-below-delimiter))

    ; Test the parse function.
    (parse-todo-tree lines)

    ; Concatenate everything, adding delimiter back in.
    (concatenate 'list lines-above-delimiter dashed-lines-below-delimiter '("") undashed-lines)))



(defun reflow-dashed-lines (lines)
  " Checks for zero-length files and files with no delimiter. "
  (if (equal (length lines) 0)
    '()
    (if (equal (position "" lines :test #'string=) nil)
      lines
      (reflow-nontrivial-lines lines))))


(defun main (argv)
  " Main function. "
  (let*
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
