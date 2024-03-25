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
		  ((derived-mode . help-mode)
		   (display-buffer-reuse-mode-window
			display-buffer-below-selected)
		   (body-function . select-window)
		   (window-size . 0.2)
		   (window-parameters . ((mode-line-format . none)))
		   (dedicated . t))
		  ((derived-mode . magit-mode)
		   (display-buffer-use-some-window)
		   (body-function . delete-other-windows)))))
