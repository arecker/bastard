build:
	sbcl --load bastard.lisp \
	--eval "(sb-ext:save-lisp-and-die #p\"~/bin/bastard\" :toplevel #'bastard:main :executable t)"
