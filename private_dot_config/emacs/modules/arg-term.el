;;; arg-term.el -- Terminal and Shells in Emacs

;; `eshell' is the built-in Hybrid Shell with Emacs Lisp support.
(use-package eshell
  :bind (("C-r" . consult-history))
  :config
  (require 'evil)
  (evil-set-initial-state 'eshell-mode 'emacs)
  (add-hook 'eshell-mode-hook (lambda ()
								(setq-local corfu-auto nil)
								(corfu-mode))))

;; `eat' is the improved Terminal emulation for Emacs.
(use-package eat
  :ensure t
  :config
  (eat-eshell-mode)
  (require 'evil)
  (evil-define-key 'normal 'global
	(kbd "<leader>st") 'eat))
