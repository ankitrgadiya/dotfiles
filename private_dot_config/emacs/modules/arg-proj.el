;;; arg-proj.el -- Project and Workspace Management

;; `magit' is the magical Git client for Emacs.
(use-package magit
  :ensure t
  :defer t
  :config
  (require 'evil)
  (evil-define-key 'normal 'global
	(kbd "<leader>gg") 'magit-status
	(kbd "<leader>gR") 'magit-refresh-all
	(kbd "<leader>gd") 'magit-dispatch)

  ;; Remove Leader key from Magit's modemap.
  (define-key magit-status-mode-map (kbd "SPC") nil))

;; `project' is the built-in Project management package in Emacs.
(use-package project
  :defer t
  :config
  ;; In Mac OS, the `find' command is the BSD variant. It may not support all
  ;; the flags. I usually install GNU coreutils, use that instead.
  (if (eq system-type 'darwin)
      (setq find-program "gfind"))

  ;; The project.el module supports detecting projects backed by VC. The
  ;; following defines a local backend that adds support for detecting projects
  ;; if the `.project' file is present in the root directory. This is similar to
  ;; the `.projectile' file used by the `projectile' package. I borrowed this
  ;; from `project-x' package.
  ;; https://github.com/karthink/project-x/blob/master/project-x.el#L157
  (defgroup project-local nil
	"Non-VC backed Project backend"
	:group 'project)

  (cl-defmethod project-root ((project (head local)))
	"Returns root directory of local project"
	(cdr project))

  (defun arg-proj--project-try-marker (dir)
	(if-let ((root (locate-dominating-file dir ".project")))
	  (cons 'local root)))

  (add-hook 'project-find-functions #'arg-proj--project-try-marker 90)

  ;; Project's default switching behaviour prompts for the next command. It is
  ;; annoying if you switch projects a lot. This configures it to directly
  ;; find-file.
  (setq project-switch-commands 'project-find-file)

  (defun arg-proj--get-project-hyperbole-buttons-file ()
	"Opens the Project-local Hyperbole Buttons File"
	(interactive)
	(find-file (concat (project-root (project-current)) "HYPB")))

  ;; Maps Project's built-in Keymap to "<LEADER> p". Some of the commands are
  ;; overwritten to use the consult alternatives.
  (require 'evil)
  (require 'consult)
  (require 'magit)
  (require 'eat)
  (evil-define-key 'normal 'global
	(kbd "<leader>p!") 'project-shell-command
	(kbd "<leader>p&") 'project-async-shell-command
	(kbd "<leader>pf") 'project-find-file
	(kbd "<leader>pF") 'project-or-external-find-file
	(kbd "<leader>pb") 'consult-project-buffer
	(kbd "<leader>ps") 'project-shell
	(kbd "<leader>pS") 'magit-save-repository-buffers
	(kbd "<leader>pd") 'project-find-dir
	(kbd "<leader>pD") 'project-dired
	(kbd "<leader>pv") 'project-vc-dir
	(kbd "<leader>pc") 'project-compile
	(kbd "<leader>pe") 'project-eshell
	(kbd "<leader>pt") 'eat-project
	(kbd "<leader>pk") 'project-kill-buffers
	(kbd "<leader>pp") 'project-switch-project
	(kbd "<leader>pg") 'consult-ripgrep
	(kbd "<leader>pG") 'project-or-external-find-regexp
	(kbd "<leader>pr") 'project-query-replace-regexp
	(kbd "<leader>px") 'project-execute-extended-command
	(kbd "<leader>ph") #'arg-proj--get-project-hyperbole-buttons-file)

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
  (require 'evil)
  (evil-define-key 'normal 'global
	(kbd "<leader><tab>n") 'tab-bar-new-tab
	(kbd "<leader><tab>r") 'tab-bar-rename-tab
	(kbd "<leader><tab>d") 'tab-bar-close-tab
	(kbd "<leader><tab><tab>") 'tab-bar-switch-to-tab))
