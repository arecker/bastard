;; -*- mode: lisp -*-
#-quicklisp
(let ((quicklisp-init (merge-pathnames ".quicklisp/setup.lisp"
				       (user-homedir-pathname))))
  (when (probe-file quicklisp-init)
    (load quicklisp-init)))
