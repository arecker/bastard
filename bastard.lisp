(defpackage bastard
  (:use :cl)
  (:export :main :build :join :each :hardlink :symlink :git :folder :is-linux))

(require :sb-posix)
(require :uiop)

(in-package :bastard)

(defparameter *real-exits* nil "Set to T to actually exit the program")

(defun exit (&optional (code 0))
  "Exit, only if *real-exits* is T."
  (when *real-exits*
    (sb-ext:exit :code code)))

(defun abspath (pathname)
  "Make relative path absolute."
  (merge-pathnames pathname))

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
  (let ((src (abspath src))
	(dest (abspath dest)))
    (equal (sb-posix:stat-ino (sb-posix:stat src))
	   (sb-posix:stat-ino (sb-posix:stat dest)))))

(defun hardlink (src dest)
  "Creates a hardlink from pathspec SRC to DEST.  
Overwrites DEST if already linking to another file."
  (let* ((src (abspath src))
	 (dest (abspath dest))
	 (info-msg (format nil "~A -> ~A"  src dest))
	 (symbol "HDL"))
    (cond ((not (probe-file src))
	   (let ((error-msg (format nil "~A does not exist" src)))
	     (report symbol :error info-msg error-msg)))
	  ((and (probe-file dest) (not (hardlinked-p src dest)))
	   (delete-file dest)
	   (sb-posix:link src dest)
	   (report symbol :changed info-msg))
	  ((not (probe-file dest))
	   (sb-posix:link src dest)
	   (report symbol :changed info-msg))
	  (t (report symbol :up-to-date info-msg)))))

(defun each (pattern)
  "Return a list of files matching a relative path, wildcard
expression."
  (uiop:directory-files (abspath pattern)))

(defun join (&rest paths)
  "Join directory names."
  (apply #'merge-pathnames paths))

(defun symlinked-p (src dest)
  "Return T if SRC is already symlinked to DEST."
  (let ((src (abspath src))
	(dest (abspath dest)))
    (handler-case (equal src (abspath (sb-posix:readlink dest)))
      (sb-posix:syscall-error (e)
	(declare (ignore e))
	nil))))

(defun symlink (src dest)
  "Create a symlink from SRC to DEST.  Delete DEST if it's already
there, whether it's a file or another symlink."
  (let* ((src (abspath src))
	 (dest (abspath dest))
	 (info-msg (format nil "~A -> ~A"  src dest))
	 (symbol "SYM"))
    (cond ((not (probe-file src))
	   (let ((error-msg (format nil "~A does not exist" src)))
	     (report symbol :error info-msg error-msg)))
	  ((not (probe-file dest))
	   (sb-posix:symlink src dest)
	   (report symbol :changed info-msg))
	  ((and (probe-file dest) (symlinked-p src dest))
	   (report symbol :up-to-date info-msg))
	  ((probe-file dest)
	   (delete-file dest)
	   (sb-posix:symlink src dest)
	   (report symbol :changed info-msg)))))

(defun folder-p (target)
  "Return T if TARGET is a folder."
  (and (probe-file target)
       (not (pathname-name (probe-file target)))))

(defun git-repo-p (dest)
  "Return T if DEST is a git repo."
  (not (null (folder-p (join dest ".git")))))

(defun git (src dest)
  "Clone a git repo."
  (let ((dest (abspath dest))
	(info-msg (format nil "~A -> ~A"  src dest))
	(symbol "GIT"))
    (cond ((git-repo-p dest)
	   (report symbol :up-to-date info-msg))
	  ((not (probe-file dest))
	   (shell-out (format nil "git clone ~A ~A" src dest))
	   (report symbol :changed info-msg))
	  (t (report symbol :error info-msg (format nil "~A already exists" dest))))))

(defun is-linux ()
  "Return T if running on linux."
  (equal "Linux" (software-type)))

(defun folder (dest)
  "Create a folder."
  (let ((dest (abspath dest))
	(symbol "DIR"))
    (cond ((folder-p dest)
	   (report symbol :up-to-date dest))
	  ((probe-file dest)
	   (report symbol :error dest "already exists as file"))
	  (t
	   (ensure-directories-exist (pathname (format nil "~A/" dest)))
	   (report symbol :changed dest)))))

(defun collect-forms (filename)
  "Collect all the forms from a lisp file."
  (with-open-file (stream filename)
    (loop for form = (read stream nil nil) while form collect form)))

(defun execute-file (filename)
  "Execute each form in a lisp file."
  (dolist (form (collect-forms filename))
    (eval form)))

(defun filename-from-args (&optional (args sb-ext:*posix-argv*))
  "Parse filename from args."
  (unless (= (length args) 2)
    (error (format nil "Usage: bastard <path/to/config.lisp>~%")))
  (nth 1 args))

(defun main (&optional filename)
  "Run the bastard program.
Expects an argument to a file containing bastard lisp."
  (handler-case (let* ((filename (abspath (or filename (filename-from-args))))
		       (directory (pathname (directory-namestring filename)))
		       (*default-pathname-defaults* directory))
		  (execute-file filename)
		  (exit 0))
    (error (e)
      (apply #'format `(t "Error: ~A~%" ,e))
      (exit 1))))

(defun build (path-to-executable)
  (let ((*real-exits* 't))
    (sb-ext:save-lisp-and-die (abspath path-to-executable) :toplevel #'main :executable t)))
