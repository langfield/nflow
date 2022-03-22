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
   "Create a new node with item."
   (cons (cons item nil) nil))


(defun first-child (tree)
    " Access first child. "
   (if (null tree)
      nil
      (cdr (car tree))))


(defun children (tree)
    " Get children of root. "
   (if (null tree)
      nil
      (cdr tree)))


(defun next-sibling (tree)
  " Get next sibling. "
   (cdr tree))


(defun data (tree)
  " Get the data of the root node of a tree. "
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


(defun looks-like-node (node)
  (assert (equal (type-of node) 'cons))
  (assert (equal (type-of (car node)) 'cons))
  (assert (not (equal (type-of (car (car node))) 'cons)))
  t)


(defun print-parent-stack (parent-stack)
  (format t "Parent stack (bottom-to-top): ")
  (for:for ((node over parent-stack))
        (format t "'~A' " (data node)))
  (terpri))


(defun pop-from-parent-stack (parent-stack num-indents)
  " Go up NUM-INDENTS levels, returning the parent and the parent-stack. "
  (format t "Popping '~A' indents from PARENT-STACK~%" num-indents)
  (print-parent-stack parent-stack)
  (assert (> num-indents 0))
  (let*
    ((parent nil))
    (dotimes (i num-indents)

      ; Get PARENT from PARENT-STACK.
      (format t "Raw PARENT-STACK: '~A'~%" parent-stack)
      (setq parent (first (last parent-stack)))
      (assert (looks-like-node parent))
      (format t "Got PARENT '~A' from top of PARENT-STACK~%" (data parent))

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
    ; Difference between the indent level and the previous indent level.
    ((indent-delta 0))
    (setq indent-delta (- indent-level old-indent-level))

    ; INDENT-SIZE acts as a kind of default. If it's 0, we consider it
    ; uninitialized and we take INDENT-DELTA as the indent size. If it's
    ; nonzero, we just use INDENT-SIZE, since we consider it "already
    ; initialized" in that case.
    (setq indent-size (get-indent-size indent-delta indent-size))

    ; If INDENT-DELTA is not a multiple of INDENT-SIZE, we have inconsistent
    ; indentation.
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

      (format t "===================================~%")
      (format t "Tree at BEGINNING of iteration: '~A'~%" tree)
      (format t "Processing line: '~A'~%" line)

        ; If LINE is nonempty:
      (if (not (str:empty? line))
        (progn

          ; Update INDENT-LEVEL.
          (setq old-indent-level indent-level)
          (setq indent-level (count-leading-spaces line))

          ; If INDENT-LEVEL increased:
          (if (> indent-level old-indent-level)
            (progn

              ; Get NUM-INDENTS (the number of indents we increased by) and
              ; INDENT-SIZE. Note that after INDENT-SIZE is set in the first
              ; call to GET-NUM-INDENTS, it is initialized and will never
              ; change again, so we enforce consistent indentation.
              (setq indent-num-size-pair (get-num-indents indent-level old-indent-level indent-size line))
              (setq num-indents (first indent-num-size-pair))
              (setq indent-size (second indent-num-size-pair))

              ; We should always increase indentation by exactly one indent
              ; level.
              (assert (> num-indents 0))
              (if (> num-indents 1)
                (error "Indent level increased by more than 1: '~A'~%" line))

              ; Append NODE to PARENT-STACK.
              ; PARENT-STACK is exactly what it sounds like, it is a stack of
              ; the ancestors of the current node. So the top element of the
              ; stack is the direct parent of the current node, and the bottom
              ; element should always be the root.
              (format t "Appending NODE '~A' to PARENT-STACK~%" (data node))
              (assert (looks-like-node node))

              ; NODE must be wrapped in a CONS because of the way APPEND works.
              (setq parent-stack (append parent-stack (cons node nil)))
              (print-parent-stack parent-stack)

              ; Set NODE to LAST-CREATED-CHILD.
              (format t "Setting NODE equal to LAST-CREATED-CHILD: '~A'~%" (data last-created-child))
              (setq node last-created-child)
              (assert (looks-like-node node)))

            ; Otherwise if INDENT-LEVEL decreased:
            (if (< indent-level old-indent-level)
              (progn

                ; Get NUM-INDENTS and INDENT-SIZE.
                (setq indent-num-size-pair (get-num-indents indent-level old-indent-level indent-size line))
                (setq num-indents (first indent-num-size-pair))
                (setq indent-size (second indent-num-size-pair))
                (assert (< num-indents 0))

                ; Get updated PARENT-STACK and PARENT.
                ; The PARENT-STACK-PAIR is the return value of UNROLL-PARENT-STACK.
                ; It is of the form (PARENT-STACK, PARENT).
                (format t "Popping from PARENT-STACK.~%")
                (setq parent-stack-pair (pop-from-parent-stack parent-stack (- num-indents)))
                (setq parent-stack (first parent-stack-pair))
                (print-parent-stack parent-stack)
                (setq parent (second parent-stack-pair))
                (format t "Popped parent: '~A'~%" (data parent))
                (assert (looks-like-node parent))

                ; Set NODE to PARENT.
                (format t "Setting NODE equal to popped PARENT '~A'~%" (data parent))
                (setq node parent))
              (progn
               (format t "Indent level did not change from previous iteration!~%")
               (print-parent-stack parent-stack))))

          ; Add LINE as another child.
          (setq last-created-child (make-tree (str:trim-left line)))
          (assert (looks-like-node last-created-child))

          (setq node (add-child node last-created-child))
          (assert (looks-like-node node))))

        ; Get a reference to the root.
      (if (> (length parent-stack) 0)
        (progn
          (setq tree (car parent-stack))
          (format t "Set TREE equal to bottom of PARENT-STACK: '~A'~%" (data (car parent-stack)))
          (assert (looks-like-node tree)))
        (progn
          (setq tree node)
          (format t "Set TREE equal to NODE: '~A'~%" (data node))
          (assert (looks-like-node tree))))
      (terpri)
      (draw-cons-tree:draw-tree tree)
      (terpri))
    tree))

(defun is-checked-off (tree)
  (str:starts-with? "- " (data tree)))

(defun and-fn (a b)
  (if (and a b)
    t
    nil))

(defun wrap-in-list (item)
  (format t "Wrapping: ~A~%" item)
  (cons item nil))


(defun get-children-as-roots (tree)
  (mapcar #'wrap-in-list (first-child tree)))

(defun resolve-todo-tree (tree)
  "Check off nodes if all their children are checked off.  This is a recursive
  function that will return a tree with the root node checked off if and only
  if all its children are 'resolved', which it determines by making recursive
  calls on all the children."
  (format t "Resolving: ~A~%" tree)
  (if (equal (first-child tree) nil)

    ; Check if the values of TREE have a dash prefix, i.e. are checked off.
    tree

    ; Reduce the children with AND to determine if all of them are checked off or not.
    (let*
      ((wrapped-children (get-children-as-roots tree)))
      (format t "Wrapped children: ~A~%" wrapped-children)
      (reduce #'and-fn (mapc #'resolve-todo-tree wrapped-children) :key #'is-checked-off :initial-value t))))


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
      (dashed-lines-below-delimiter nil)
      (tree nil))

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

    ; Get a tree representation of the lines below delimiter.
    (setq tree (parse-todo-tree lines-below-delimiter))

    (print-elements-of-list "Original" lines)

    ; Draw TREE on each iteration.
    (terpri)
    (draw-cons-tree:draw-tree tree)
    (terpri)

    ; Print cons form of TREE.
    (format t "Tree: ~A~%" tree)

    ; Print result of RESOLVE-TODO-TREE.
    (format t "All checked off: ~A~%" (resolve-todo-tree tree))

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
   (print-elements-of-list "Single-level implementation result" reflowed-lines)))


; OUTLINE
; -------
; Read the whole file in. (DONE)
; Get all elements after the empty line that start with "- ". (DONE)
; Get all elements after the empty line that do not start with "- ". (DONE)
; Move the dashed lines to just before the empty line in the sequence. (DONE)
; Put the new list back in the file.
