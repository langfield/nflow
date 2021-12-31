SOURCE = nflow.lisp
TREE_SOURCE = tree.lisp
default:
	buildapp --output nflow --load $(SOURCE) --entry main
	buildapp --output tree --load $(TREE_SOURCE) --entry main
clean:
	rm nflow
	rm tree
