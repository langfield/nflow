SOURCE = nflow.lisp
default:
	buildapp --output nflow --load $(SOURCE) --entry main
clean:
	rm nflow
install:
	cp nflow ~/.local/bin/
