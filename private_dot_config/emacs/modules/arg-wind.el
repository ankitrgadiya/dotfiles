;;; arg-wind.el -- Window Configurations

(use-package popper
  :ensure t ; or :straight t
  :bind (("C-`"   . popper-toggle)
         ("M-`"   . popper-cycle)
         ("C-M-`" . popper-toggle-type))
  :init
  (setq popper-group-function #'popper-group-by-project
		popper-reference-buffers
        '("\\**eshell\\*"
		  ".*-eat\\*"
		  "\\*Dictionary\\*"
		  "\\*Draft\\*"
		  comint-mode
		  inferior-emacs-lisp-mode
		  term-mode
		  sly-mrepl-mode
          help-mode
          compilation-mode))

  (popper-mode +1)
  (popper-echo-mode +1))                ; For echo area hints

(use-package emacs
  :config
  (setq switch-to-buffer-obey-display-actions t)

  ;; Compilation mode
  (add-to-list 'display-buffer-alist
			   '((derived-mode . compilation-mode)
				 (display-buffer-reuse-window
				  display-buffer-at-bottom)
				 (window-height . 0.3)
				 (dedicated . t)))

  ;; Comint and Shell buffers
  (add-to-list 'display-buffer-alist
			   '((or . ((derived-mode . comint-mode)
						(derived-mode . eat-mode)
						"\\**eshell\\*"))
				 (display-buffer-reuse-window
				  display-buffer-at-bottom)
				 (body-function . select-window)
				 (window-height . 0.5)
				 (dedicated . t)))

  ;; Help Buffers
  (add-to-list 'display-buffer-alist
			   '((derived-mode . help-mode)
				 (display-buffer-reuse-window
				  display-buffer-at-bottom)
				 (body-function . select-window)
				 (window-height . 0.3)
				 (window-parameters . ((mode-line-format . none)))
				 (dedicated . t)))

  ;; Org Agenda
  (add-to-list 'display-buffer-alist
			   '("\\*Org Agenda\\*"
				 (display-buffer-use-some-window)
				 (body-function . delete-other-windows)
				 (dedicated . t)))

  ;; Org Agenda Command and Org Capture buffers
  (add-to-list 'display-buffer-alist
			   '((or . ("\\*Agenda Commands\\*"
						"\\*Org Select\\*"
						"CAPTURE-*"))
				 (display-buffer-reuse-window
				  display-buffer-at-bottom)
				 (window-height . 0.3)
				 (dedicated . t)))

  ;; Dired Buffers
  (add-to-list 'display-buffer-alist
			   '((derived-mode . dired-mode)
				 (display-buffer-use-some-window)
				 (body-function . delete-other-windows)
				 (dedicated . t)))

  ;; Magit Status
  (add-to-list 'display-buffer-alist
			   '((derived-mode . magit-status-mode)
				 (display-buffer-use-some-window)
				 (body-function . delete-other-windows)
				 (dedicated . t)))

  ;; Checkdoc Status
  (add-to-list 'display-buffer-alist
			   '("\\*Checkdoc Status\\*"
				 (display-buffer-reuse-window
				  display-buffer-at-bottom)
				 (window-height . 0.3)
				 (dedicated . t)))

  ;; Draft Buffer
  (add-to-list 'display-buffer-alist
			   '("\\*Draft\\*"
				 (display-buffer-reuse-window
				  display-buffer-at-bottom)
				 (window-height . 0.4)
				 (dedicated . t))))
