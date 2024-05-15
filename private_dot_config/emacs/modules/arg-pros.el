;;; arg-pros.el -- Prose Configurations

;; `logos' is a generic Focus mode.
(use-package logos
  :ensure t
  :defer t
  :config
  (setq logos-outlines-are-pages t)
  (setq-default logos-hide-mode-line t
				logos-hide-header-line t)

  (defun arg-pros--reveal-entry ()
	"Reveal Org or Outline entry."
	(cond
	 ((and (eq major-mode 'org-mode)
           (org-at-heading-p))
      (org-show-entry))
	 ((or (eq major-mode 'outline-mode)
          (bound-and-true-p outline-minor-mode))
      (outline-show-entry))))

  (add-hook 'logos-page-motion-hook #'arg-pros--reveal-entry)

  (let ((map org-mode-map))
	(define-key map [remap forward-page] #'logos-forward-page-dwim)
	(define-key map [remap backward-page] #'logos-backward-page-dwim)))


;; `org' is the Great Outline Major mode for everything.
(use-package org
  :ensure t
  :defer t
  :init
  (defun arg-pros--org-work-agenda ()
	(interactive)
	(org-agenda nil "w"))

  (defun arg-pros--org-home-agenda ()
	(interactive)
	(org-agenda nil "h"))

  (evil-define-key 'normal 'global
	(kbd "<leader>aa") 'org-agenda
	(kbd "<leader>aw") #'arg-pros--org-work-agenda
	(kbd "<leader>ah") #'arg-pros--org-home-agenda
	(kbd "<leader>ao") 'org-cycle-agenda-files
	(kbd "<leader>ac") 'org-capture
	(kbd "<leader>ocg") 'org-clock-goto)

  (evil-define-key 'normal org-mode-map
	(kbd "<leader>o,") 'org-insert-structure-template
	(kbd "<leader>ot") 'org-todo
	(kbd "<leader>oq") 'org-set-tags-command
	(kbd "<leader>o$") 'org-archive-subtree
	(kbd "<leader>os") 'org-schedule
	(kbd "<leader>od") 'org-deadline
	(kbd "<leader>oci") 'org-clock-in
	(kbd "<leader>oco") 'org-clock-out
	(kbd "<leader>ocd") 'org-clock-display
	(kbd "<leader>occ") 'org-clock-cancel)
  :config
  ;; Global Configuration
  (setq org-directory "~/Dropbox/Org"
		org-tags-column -77
		org-fold-catch-invisible-edits 'show-and-error
		org-insert-heading-respect-content t
		org-startup-indented t
		org-startup-folded t

		;; Prose
		org-hide-emphasis-markers t
		org-pretty-entities t
		org-ellipsis "…"

		;; Babel Configuration
		org-src-fontify-natively t
		org-src-tab-acts-natively t

		;; Capture Configuration
		org-default-notes-file (concat org-directory "/inbox.org")
		org-capture-templates
		'(("w" "[Work] New Task" entry (file+headline "~/Dropbox/Org/inbox.org" "Work")
		   "* TODO %?\n  %i\n  %a" :clock-in t :clock-resume t)
		  ("m" "[Work] Meeting (Work)" entry (file+headline "~/Dropbox/Org/inbox.org" "Work")
		   "* MEETING %?\n  %i\n  %a" :clock-in t :clock-resume t)
		  ("p" "[Personal] New Task" entry (file+headline "~/Dropbox/Org/inbox.org" "Personal")
		   "* TODO %?\n  %i\n  %a" :clock-in t :clock-resume t)
		  ("h" "[Household] New Task" entry (file+headline "~/Dropbox/Org/inbox.org" "Household")
		   "* TODO %?\n  %i\n  %a" :clock-in t :clock-resume t))

		;; Custom Faces for the TODO Keywords
		org-todo-keyword-faces
		`(("BACKLOG" :foreground ,(modus-themes-get-color-value 'slate) :weight bold)
		  ("TODO" :foreground ,(modus-themes-get-color-value 'red-warmer) :weight bold)
		  ("NEXT" :foreground ,(modus-themes-get-color-value 'blue-cooler) :weight bold)
		  ("INPROGRESS" :foreground ,(modus-themes-get-color-value 'cyan-warmer) :weight bold)
		  ("WAITING" :foreground ,(modus-themes-get-color-value 'yellow-warmer) :weight bold)
		  ("BLOCKED" :foreground ,(modus-themes-get-color-value 'magenta) :weight bold)
		  ("DONE" :foreground ,(modus-themes-get-color-value 'cyan-cooler) :weight bold)
		  ("CANCELLED" :foreground ,(modus-themes-get-color-value 'cyan-faint) :weight bold)
		  ("MEETING" :foreground ,(modus-themes-get-color-value 'cyan-warmer) :weight bold))

		;; HTML Export Configuration
		org-html-validation-link nil
		org-html-head-include-scripts nil
		org-html-head-include-default-style nil
		org-html-head "<link rel=\"stylesheet\" href=\"https://www.w3.org/StyleSheets/Core/Midnight\" />"

		;; Agenda Configuration
		org-agenda-files '("inbox.org" "archive.org")
		org-archive-location (concat org-directory "/archive.org::datetree/")
		org-archive-subtree-add-inherited-tags t
		org-log-done 'time
		org-enforce-todo-dependencies t
		org-enforce-todo-checkbox-dependencies t
		org-agenda-custom-commands
		'(("w" "Work Agenda" agenda ""
		   ((org-agenda-tag-filter-preset '("+@work"))))
		  ("h" "Home Agenda" agenda ""
		   ((org-agenda-tag-filter-preset '("-@work")))))

		;; Org Clock
		org-clock-idle-time 10
		org-clock-persist 'history
		org-clock-in-resume t
		org-clock-out-remove-zero-time-clocks t
		org-clock-out-when-done t
		org-clock-persist-query-resume nil)


  (org-clock-persistence-insinuate)
  (require 'ox-md))


;; `arg-ol-github' adds support for Github Links in Orgmode.
(use-package arg-ol-github
  :after org
  :config
  (require 'arg-ol-github))


;; `org-modern' is the eye-candy package for Org-mode.
(use-package org-modern
  :ensure t
  :after org
  :config

  ;; Presentation
  (defun arg-start-presentation ()
	"Start Presentation - Configures Fonts, Style, etc."
	(interactive)
	(cond ((eq major-mode 'org-mode)
		   (progn
			 (setq-local arg-presentation t)
			 (fontaine-set-preset 'presentation)
			 (variable-pitch-mode 1)
			 (org-modern-mode 1)
			 (logos-focus-mode 1)
			 (logos-narrow-dwim)
			 (org-fold-show-entry)))
		   (t (error "This command only works in Org buffers."))))

  (defun arg-stop-presentation ()
	"Stop Presentation - Reverts Fonts, Style, etc."
	(interactive)
	(cond ((not (eq major-mode 'org-mode))
		   (error "This command only works in Org buffers."))
		  ((not arg-presentation)
		   (error "Not presenting right now."))
		  (t (progn
			   (org-modern-mode -1)
			   (variable-pitch-mode -1)
			   (logos-focus-mode -1)
			   (fontaine-set-preset 'regular)
			   (widen)
			   (setq-local arg-presentation nil))))))

;; Markdown Configurations
(use-package markdown-mode
  :ensure t
  :autoload gfm-mode
  :mode ("\\.\\(?:md\\|markdown\\|mkd\\|mdown\\|mkdn\\|mdwn\\)\\'" . gfm-mode)
  :config
  (setq markdown-command '("pandoc" "--from=gfm" "--to=html5")))
