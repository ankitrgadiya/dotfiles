;;; arg-proj.el -- Project and Workspace Management

;; `magit' is the magical Git client for Emacs.
(use-package magit
  :ensure t)

;; `force' integrates Git Force support in Magit.
(use-package forge
  :ensure t
  :after magit)

;; `project' is the built-in Project management package in Emacs.
(use-package project
  :config
  ;; In Mac OS, the `find' command is the BSD variant. It may not support all
  ;; the flags. I usually install GNU coreutils, use that instead.
  (if (eq system-type 'darwin)
      (setq find-program "gfind"))

  ;; Project's default switching behaviour prompts for the next command. It is
  ;; annoying if you switch projects a lot. This configures it to directly
  ;; find-file.
  (setq project-switch-commands 'project-find-file)

  (require 'persistent-scratch)
  (defun arg-proj--project-scratch ()
	"Opens the Project-local persistent scratch buffer"
	(interactive)
	  (switch-to-buffer (concat (project-name (project-current))
								" *scratch*"))
	  (persistent-scratch-restore)
	  (persistent-scratch-mode))


  ;; Maps Project's built-in Keymap to "<LEADER> p". Some of the commands are
  ;; overwritten to use the consult alternatives.
  (require 'evil-leader)
  (require 'consult)
  (require 'magit)
  (evil-leader/set-key
    "p!" 'project-shell-command
    "p&" 'project-async-shell-command
    "pf" 'project-find-file
    "pF" 'project-or-external-find-file
    "pb" 'consult-project-buffer
    "ps" 'project-shell
	"pS" 'magit-save-repository-buffers
    "pd" 'project-find-dir
    "pD" 'project-dired
    "pv" 'project-vc-dir
    "pc" 'project-compile
    "pe" 'project-eshell
    "pk" 'project-kill-buffers
    "pp" 'project-switch-project
    "pg" 'consult-ripgrep
    "pG" 'project-or-external-find-regexp
    "pr" 'project-query-replace-regexp
    "px" 'project-execute-extended-command
    "p\C-b" 'project-list-buffers
	"px" #'arg-proj--project-scratch)

  ;; Override the key-bindings to the Global Tab Switching
  (evil-define-key evil-collection-magit-state magit-status-mode-map "gt" 'evil-tab-next)
  (evil-define-key evil-collection-magit-state magit-status-mode-map "gT" 'tab-bar-switch-to-prev-tab))

;; `tab-bar' is the built-in Tabbar in Emacs.
(use-package tab-bar
  :config
  ;; If inside a project, use project-name as the tab-name.
  (require 'project)
  (defun arg-proj--tab-bar-name ()
	(let ((cur (project-current)))
	  (if cur
		  (project-name cur)
		(tab-bar-tab-name-current))))

  (setq tab-bar-new-tab-choice "*scratch*"
		tab-bar-close-button-show nil
		tab-bar-tab-name-function #'arg-proj--tab-bar-name
        ;; Show Tab bar if there is more than one tab
		tab-bar-show 1)

  ;; Configure (some of) Doomemacs' key-bindings for Magit.
  (require 'evil-leader)
  (evil-leader/set-key
    "<tab>n" 'tab-bar-new-tab
    "<tab>r" 'tab-bar-rename-tab
    "<tab>d" 'tab-bar-close-tab
    "<tab><tab>" 'tab-bar-switch-to-tab))
