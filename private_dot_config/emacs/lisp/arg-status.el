;;; arg-status.el --- A simpler micro-blogging   -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(defcustom arg-status-path ""
  "Path of the Status directory")

(defvar arg-status-file-name #'arg--status-file-name
  "Function used to generate Status filename")

(defun arg--status-file-name ()
  (format "%d.md" (float-time)))

(defun arg--status-file-path (filename)
  (expand-file-name (file-name-concat arg-status-path filename)))

(defun arg--status-template ()
  (let ((status (file-name-base (buffer-name)))
		(timestamp (format-time-string "%Y-%m-%d %H:%M:%S")))
	(string-join (list "+++"
					   (format "date     = \"%s\"" timestamp)
					   "template = \"status/page.html\""
					   (format "aliases  = [\"/s/%s/\"]" status)
					   "+++"
					   "\n")
				 "\n")))

;;;###autoload
(defun arg-status ()
  (interactive)
  (progn
	(find-file (arg--status-file-path (funcall arg-status-file-name)))
	(insert (arg--status-template))
	(arg--status-template)
	(when (boundp 'evil-state)
	  (evil-insert-state))))

(provide 'arg-status)
