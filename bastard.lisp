(defpackage bastard
  (:use :cl)
  (:export :hardlink))

(require :sb-posix)

(in-package :bastard)

(defun abs-path-to-relative (path)
  "Turn an absolute path into a path relative to the home dir.
If not under the home dir, just returns the path."
  (let* ((path (truename (pathname path)))
	 (result (enough-namestring path (user-homedir-pathname))))
    (if (string= result "~" :end1 1 :end2 1) result
	(concatenate 'string "~/" result))))

(defun glyph-action (action)
  (let ((lookup '((:up-to-date "✓")
		  (:changed "⚡")
		  (:error "✗"))))
    (first (cdr (assoc action lookup)))))

(defun report (symbol action info-msg &optional error-msg)
  "Report a state result."
  (format t "[~A] ~A: ~A~%" (glyph-action action) symbol info-msg)
  (when error-msg
    (format t "     E: ~A~%" error-msg)))

(defun already-hardlinked (src dest)
  "Returns T if pathspec SRC and DEST are already hardlinked to one
another."
  (equal (sb-posix:stat-ino (sb-posix:stat src))
	 (sb-posix:stat-ino (sb-posix:stat dest))))

(defun hardlink (src dest)
  "Creates a hardlink from pathspec SRC to DEST.  
Overwrites DEST if already linking to another file."
  (let* ((src (pathname src))
	 (dest (pathname dest))
	 (info-msg (format nil "~A -> ~A" (abs-path-to-relative src) (abs-path-to-relative dest)))
	 (symbol "HL"))
    (cond ((not (probe-file src))
	   (let ((error-msg (format nil "~A does not exist" (abs-path-to-relative src))))
	     (report symbol :error info-msg error-msg)))
	  ((and (probe-file dest) (not (already-hardlinked src dest)))
	   (delete-file dest)
	   (sb-posix:link src dest)
	   (report symbol :changed info-msg))
	  ((not (probe-file dest))
	   (sb-posix:link src dest)
	   (report symbol :changed info-msg))
	  (t (report symbol :up-to-date info-msg)))))

