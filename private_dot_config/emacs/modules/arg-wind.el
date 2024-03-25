;;; arg-wind.el -- Window Configurations

;; `winner' is a simple Window management for Emacs.
(use-package winner
  :config
  (winner-mode 1))

(use-package emacs
  :config
  (setq display-buffer-alist
		`(("\\**eshell\\*"
		   (display-buffer-at-bottom)
		   (dedicated . t))
		  ((or . ((derived-mode . help-mode)
				  (derived-mode . compilation-mode)
				  "\\*Agenda Commands\\*"))
		   (display-buffer-reuse-mode-window
			display-buffer-below-selected)
		   (body-function . select-window)
		   (window-height . 0.3)
		   (window-parameters . ((mode-line-format . none)))
		   (dedicated . t))
		  ((or . ((derived-mode . magit-status-mode)
				  "\\*Org Agenda\\*"))
		   (display-buffer-use-some-window)
		   (body-function . delete-other-windows)))))
