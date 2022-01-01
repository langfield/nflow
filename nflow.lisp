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
   (cons (cons item nil) nil)
)

(defun first-child (tree)
    " Access first child. "
   (if (null tree)
      nil
      (cdr (car tree))
   )
)

(defun next-sibling (tree)
  " Get next sibling. "
   (cdr tree)
)
(defun data (tree)
  " Get data of tree. "
   (car (car tree))
)

(defun add-child (tree child)
  " Add child to tree. "
   (setf (car tree) (append (car tree) child))
   tree
)

(defun count-leading-spaces (s)
  " Count leading spaces of a string. "
  (let*
    (
      (left-trimmed-s (str:trim-left s))
    )
    (- (length s) (length left-trimmed-s))
  )
)


(defun parse-todo-tree (lst)
  " Parse a todolist into a tree, preserving hierarchy. "
  (let*
    (
      ; Initialize TREE with a dummy root.
      (tree (make-tree "root"))
      (indent-level 0)
      (old-indent-level 0)
      (node tree)
      (last-created-child nil)
      (parent-stack '())
      (parent nil)
    )
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

              ; Append NODE to PARENT-STACK.
              (setq parent-stack (append parent-stack node))

              ; Set NODE to LAST-CREATED-CHILD.
              (setq node last-created-child)
            )

            ; Otherwise if INDENT-LEVEL decreased:
            (if (< indent-level old-indent-level)
              (progn
                ; Get PARENT from PARENT-STACK.
                (setq parent (last parent-stack))

                ; Remove PARENT from PARENT-STACK.
                (setq parent-stack (butlast parent-stack))

                ; Set NODE to PARENT.
                (setq node parent)
              )
            )
          )

          ; Add LINE as another child.
          (setq last-created-child (make-tree (str:trim-left line)))
          (setq node (add-child node last-created-child))
        )
      )
      ; Get a reference to the root.
      (if (> (length parent-stack) 0)
        (setq tree (car parent-stack))
        (setq tree node)
      )

      ; Draw TREE on each iteration.
      (terpri)
      (draw-cons-tree:draw-tree tree)
      (terpri)

    )


    ; Print cons form of TREE.
    (format t "Tree: ~A~%" tree)
  )
)




(defun reflow-dashed-lines (lines)
  " Return the reflowed list of lines, with dashed lines moved above the delimiter, order-preserved. "
  (cond

    ; If LINES has length 0, return an empty list.
    ((equal (length lines) 0) '())

    ; Else:
    (t
      (cond

        ; If there is no delimiter, return LINES unchanged.
        ((equal (position "" lines :test #'string=) nil) lines)

        ; Else:
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
          (concatenate 'list lines-above-delimiter dashed-lines-below-delimiter '("") undashed-lines)
          (parse-todo-tree lines)))))))

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
