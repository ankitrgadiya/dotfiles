;;; arg-ol-gitea.el --- Gitea Links in Org-mode         -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(require 'ol)
(require 'browse-url)

(defcustom org-gitea-instance-url "https://git.argp.in"
  "URL of the Gitea instance to use for the link."
  :type 'string)

(defcustom org-gitea-default-namespace "ankit"
  "Name of the default Gitea namespace (user or organization). This is
used in case the link does not specify a username or organization."
  :type 'string)

(org-link-set-parameters "gitea"
						 :follow #'org-gitea-open
						 :export #'org-gitea-export)

(defun org-gitea-open (path _)
  "Open Gitea repository."
  (browse-url (org-gitea--convert-to-link path)))

(defun org-gitea-export (link description format _)
  "Export Gitea repository link from Org files."
  (let ((path (org-gitea--convert-to-link link))
        (desc (or description link)))
    (pcase format
      (`html (format "<a target=\"_blank\" href=\"%s\">%s</a>" path desc))
      (`latex (format "\\href{%s}{%s}" path desc))
      (`texinfo (format "@uref{%s,%s}" path desc))
      (`ascii (format "%s (%s)" desc path))
      (_ path))))

(defun org-gitea--convert-to-link (path)
  (let* ((path (org-gitea--ensure-namespace path))
		 (splits (string-split path "#")))
	(if (> (length splits) 1)
		(format "%s/%s/issues/%s" org-gitea-instance-url (car splits) (cadr splits))
	  (format "%s/%s" org-gitea-instance-url path))))

(defun org-gitea--ensure-namespace (path)
  (let ((splits (string-split path "/")))
	(if (> (length splits) 1)
		path
	  (format "%s/%s" org-gitea-default-namespace path))))

(provide 'arg-ol-gitea)
