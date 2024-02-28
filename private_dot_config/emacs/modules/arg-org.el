;;; arg-org.el -- The Great Org-Mode Configurations

(use-package org
  :ensure t
  :config
  ;; Global Configuration
  (setq org-directory "~/.local/share/org")

  ;; Babel Configuration
  (setq org-src-fontify-natively t
		org-src-tab-acts-natively t)

  ;; Agenda Configuration
  (setq org-agenda-files '("inbox.org")
		org-todo-keywords '((sequence "BACKLOG" "TODO" "IN PROGRESS" "BLOCKED"
									  "|" "DONE")))
  (require 'evil)
  (evil-define-key 'normal 'global (kbd "<leader>aa") 'org-agenda))

;; `org-present' implements Presentations for org-mode documents.
(use-package org-present
  :ensure t
  :config
  (add-hook 'org-present-mode-hook 'org-present-big)
  (add-hook 'org-present-mode-quit-hook 'org-present-small)

  (require 'evil)
  (evil-define-key 'normal 'global (kbd "<leader>op") 'org-present))
