;;; arg-ol-github.el --- Github Links in Org-mode         -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(require 'ol)
(require 'browse-url)

(defcustom org-github-default-namespace "rapyuta-robotics"
  "Name of the default Github namespace (user or organization). This is
used in case the link does not specify a username or organization."
  :type 'string)

(org-link-set-parameters "gh"
						 :follow #'org-github-open
						 :export #'org-github-export)

(defun org-github-open (path _)
  "Open Github repository."
  (browse-url (org-github--convert-to-link path)))

(defun org-github-export (link description format _)
  "Export Github repository link from Org files."
  (let ((path (org-github--convert-to-link link))
        (desc (or description link)))
    (pcase format
      (`html (format "<a target=\"_blank\" href=\"%s\">%s</a>" path desc))
      (`latex (format "\\href{%s}{%s}" path desc))
      (`texinfo (format "@uref{%s,%s}" path desc))
      (`ascii (format "%s (%s)" desc path))
      (t path))))

(defun org-github--convert-to-link (path)
  (let* ((path (org-github--ensure-namespace path))
		 (splits (string-split path "#")))
	(if (> (length splits) 1)
		(format "https://github.com/%s/pull/%s" (car splits) (cadr splits))
	  (format "https://github.com/%s" path))))

(defun org-github--ensure-namespace (path)
  (let ((splits (string-split path "/")))
	(if (> (length splits) 1)
		path
	  (format "%s/%s" org-github-default-namespace path))))

(provide 'arg-ol-github)
