;;; arg-org.el -- The Great Org-Mode Configurations

(use-package org
  :ensure t
  :hook (org-mode . org-indent-mode)
  :config
  ;; Global Configuration
  (setq org-directory "~/.local/share/org"
		org-tags-column 0
		org-fold-catch-invisible-edits 'show-and-error
		org-insert-heading-respect-content t
		org-startup-folded t)

  ;; Babel Configuration
  (setq org-src-fontify-natively t
		org-src-tab-acts-natively t)

  ;; Capture Configuration
  (setq org-default-notes-file (concat org-directory "/inbox.org")
		org-capture-templates
		'(("w" "Work Task" entry (file+headline "~/.local/share/org/inbox.org" "Work")
		   "* TODO %?\n  %i\n  %a")
		  ("h" "Home Task" entry (file+headline "~/.local/share/org/inbox.org" "Home")
		   "* TODO %?\n  %i\n  %a")))

  ;; Agenda Configuration
  (setq org-agenda-files '("inbox.org" "archive.org")
		org-archive-location (concat org-directory "/archive.org::datetree/")
		org-log-done 'time)
  (require 'evil)
  (evil-define-key 'normal 'global
	(kbd "<leader>aa") 'org-agenda
	(kbd "<leader>ao") 'org-cycle-agenda-files
	(kbd "<leader>ac") 'org-capture))

;; `org-present' implements Presentations for org-mode documents.
(use-package org-present
  :ensure t
  :config
  (setq org-present-text-scale 3)

  (require 'evil)
  (evil-define-key 'normal 'global (kbd "<leader>op") 'org-present)

  (add-hook 'org-present-mode-hook 'org-present-big)
  (add-hook 'org-present-mode-quit-hook 'org-present-small))

;; `org-modern' is the eye-candy package for Org-mode.
(use-package org-modern
  :ensure t
  :config
  (add-hook 'org-mode-hook #'org-modern-mode)
  (add-hook 'org-agenda-finalize-hook #'org-modern-agenda)
  (set-face-attribute 'org-modern-symbol nil :font "Iosevka" :weight 'bold :height 140)
  (set-face-attribute 'org-document-title nil :font "Iosevka" :weight 'bold :height 1.3)
  (setq org-hide-emphasis-markers t
		org-pretty-entities t
		org-ellipsis "…"))

;; `alert' is the notification library for Emacs.
(use-package alert
  :ensure t
  :config
  (if (eq system-type 'darwin)
	  (setq alert-default-style 'osx-notifier)
	(setq alert-default-style 'libnotify)))

;; `org-alert' notifies about the Agenda items
(use-package org-alert
  :ensure t
  :config
  (setq org-alert-notification-title "Agenda"))
