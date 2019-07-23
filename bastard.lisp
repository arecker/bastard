(defpackage bastard
  (:use :cl)
  (:export :hardlink))

(in-package :bastard)

(defun abs-path-to-relative (path)
  "Turn an absolute path into a path relative to the home dir.
If not under the home dir, just returns the path."
  (let ((result (enough-namestring path (user-homedir-pathname))))
    (if (string= result "~" :end1 1 :end2 1) result
	(concatenate 'string "~/" result))))

(defun report-hardlink (src dest action &optional error-msg)
  "Report a hardlink.  Print soruce, destination, action, and error if
it occured."
  (let ((symbol (cond ((equal action :up-to-date) "✓")
		      ((equal action :changed) "⚡")
		      ((equal action :error) "✗"))))
    (format t "[~A] HL: ~A -> ~A~%" symbol (abs-path-to-relative src) (abs-path-to-relative dest))
    (when error-msg
      (format t "     E: ~A~%" error-msg))))

(defun already-hardlinked (src dest)
  "Returns T if pathspec SRC and DEST are already hardlinked to one
another."
  (equal (sb-posix:stat-ino (sb-posix:stat src))
	 (sb-posix:stat-ino (sb-posix:stat dest))))

(defun hardlink (src dest)
  "Creates a hardlink from pathspec SRC to DEST.  
Overwrites DEST if already linking to another file."
  (let ((src (pathname src))
	(dest (pathname dest)))
    (cond ((not (probe-file src))
	   (let ((msg (format nil "~A does not exist" (abs-path-to-relative src))))
	     (report-hardlink src dest :error msg)))
	  ((and (probe-file dest) (not (already-hardlinked src dest)))
	   (delete-file dest)
	   (sb-posix:link src dest)
	   (report-hardlink src dest :changed))
	  ((not (probe-file dest))
	   (sb-posix:link src dest)
	   (report-hardlink src dest :changed))
	  (t (report-hardlink src dest :up-to-date)))))

