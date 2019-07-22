(defpackage bastard
  (:use :cl)
  (:export :hardlink))

(in-package :bastard)

(defun info (msg &rest args)
  (format t "BASTARD: ~A"
	  (apply #'format nil msg args)))

(defun already-hardlinked (src dest)
  "Returns T if pathspec SRC and DEST are already hardlinked to one
another."
  (and (equal (sb-posix:stat-ino (sb-posix:stat src))
	      (sb-posix:stat-ino (sb-posix:stat dest)))))

(defun hardlink (src dest)
  "Creates a hardlink from pathspec SRC to DEST.  
Overwrites DEST if already linking to another file."
  (cond ((not (probe-file src))
	 (error 'hardlink-src-nonexistent))
	((and (probe-file dest) (not (already-hardlinked src dest)))
	 (info "~A already exists, deleting" dest)
	 (delete-file dest))
	((not (probe-file dest))
	 (info "creating hardlink from ~A to ~A")
	 (sb-posix:link src dest))
	(t (info "~A already hardlinked to ~A" dest src))))

(define-condition hardlink-src-nonexistent (error) ())
