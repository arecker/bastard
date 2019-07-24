(defpackage bastard
  (:use :cl)
  (:export :main :hardlink))

(require :sb-posix)
(require :uiop)

(in-package :bastard)

(defun abs-path-to-relative (path)
  "Turn an absolute path into a path relative to the home dir.
If not under the home dir, just returns the path."
  (let* ((path (truename (pathname path)))
	 (result (enough-namestring path (user-homedir-pathname))))
    (if (string= result "~" :end1 1 :end2 1) result
	(concatenate 'string "~/" result))))

(defun glyph-action (action)
  "Return a coresponding glyph symbol for an action."
  (getf '(:up-to-date "✓" :changed "⚡" :error "✗") action))

(defun report (symbol action info-msg &optional error-msg)
  "Report a state result."
  (format t "[~A] ~A: ~A~%" (glyph-action action) symbol info-msg)
  (when error-msg
    (format t "     E: ~A~%" error-msg)))

(defun shell-out (cmd)
  "Runs shell command CMD.  Returns plist of exit status, stdout, and
stderr."
  (multiple-value-bind (output error exit)
      (uiop:run-program cmd
			:ignore-error-status t
			:output '(:string :stripped t)
			:error '(:string :stripped t))
    (list :exit exit :output output :error error)))

(defun hardlinked-p (src dest)
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
	  ((and (probe-file dest) (not (hardlinked-p src dest)))
	   (delete-file dest)
	   (sb-posix:link src dest)
	   (report symbol :changed info-msg))
	  ((not (probe-file dest))
	   (sb-posix:link src dest)
	   (report symbol :changed info-msg))
	  (t (report symbol :up-to-date info-msg)))))

(defun collect-forms (filename)
  "Collect all the forms from a lisp file."
  (with-open-file (stream filename)
    (loop for form = (read stream nil nil) while form collect form)))

(defun execute-file (filename)
  "Execute each form in a lisp file."
  (dolist (form (collect-forms filename))
    (eval form)))

(defun main ()
  "Run the bastard program.
Expects an argument to a file containing bastard lisp."
  (unless (= (length sb-ext:*posix-argv*) 2)
    (format t "Usage: bastard <path/to/config.lisp>~%")
    (sb-ext:exit :code 1))
  (let ((filename (pathname (nth 1 sb-ext:*posix-argv*))))
    (handler-case (progn
		    (execute-file filename)
		    (sb-ext:exit :code 0))
      (error (e)
	(format t "Error: ~A~%" e)
	(sb-ext:exit :code 1)))))
  
