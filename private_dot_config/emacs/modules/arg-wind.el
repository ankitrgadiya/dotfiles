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
		  term-mode
		  sly-mrepl-mode
          help-mode
          compilation-mode
		  dired-mode))

  (popper-mode +1)
  (popper-echo-mode +1))                ; For echo area hints

(use-package emacs
  :config
  (setq display-buffer-alist
		`(((or . ("\\**eshell\\*"
				  ".*-eat\\*"))
		   (display-buffer-at-bottom)
		   (dedicated . t))
		  ((derived-mode . help-mode)
		   (display-buffer-reuse-mode-window
			display-buffer-below-selected)
		   (body-function . select-window)
		   (window-height . 0.3)
		   (window-parameters . ((mode-line-format . none)))
		   (dedicated . t))
		  ((derived-mode . dired-mode)
		   (display-buffer-reuse-mode-window
			display-buffer-in-side-window)
		   (body-function . select-window)
		   (window-width . 0.5)
		   (side . right)
		   (dedicated . t))
		  ((derived-mode . compilation-mode)
		   (display-buffer-reuse-mode-window
			display-buffer-below-selected)
		   (window-height . 0.3)
		   (dedicated . t))
		  ((or . ((derived-mode . magit-status-mode)
				  "\\*Org Agenda\\*"))
		   (display-buffer-use-some-window)
		   (body-function . delete-other-windows)))))
