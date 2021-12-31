(load "~/.quicklisp/setup.lisp")
(defun make-tree (item)
   "it creates a new node with item."
   (cons (cons item nil) nil)
)
(defun first-child (tree)
   (if (null tree)
      nil
      (cdr (car tree))
   )
)

(defun next-sibling (tree)
   (cdr tree)
)
(defun data (tree)
   (car (car tree))
)
(defun add-child (tree child)
   (setf (car tree) (append (car tree) child))
   tree
)


(defun main (argv)
  " Main function. "
  (setq mytree (make-tree 1))
  (format t "~A~%" mytree)
  (setq mytree (add-child mytree (make-tree 2)))
  (setq mytree (add-child mytree (make-tree 3)))
  (setq mytree (add-child mytree (make-tree 4)))
  (setq mytree (add-child mytree (make-tree 5)))
  (setq mytree (add-child mytree (make-tree 6)))
  (format t "~A~%" mytree)
)
