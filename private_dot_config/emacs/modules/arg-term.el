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
  ;; Kill Eat buffer on exit
  (add-hook 'eat-exit-hook
			(lambda (_)
			  (kill-buffer (current-buffer))))

  ;; Enable Eat mode in Eshell buffer
  (eat-eshell-mode)
  (require 'evil)
  (evil-set-initial-state 'eshell-mode 'emacs)
  (evil-define-key 'normal 'global
	(kbd "<leader>st") 'eat))
