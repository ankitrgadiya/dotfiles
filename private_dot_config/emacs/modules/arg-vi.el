;;; arg-vi.el -- Vi Emulation in Emacs

;; `evil' is the most comprehensive Vi emulation implementation for Emacs.
(use-package evil
  :ensure t
  :init
  (setq evil-respect-visual-line-mode t
		evil-undo-system 'undo-redo
		evil-want-C-u-scroll t
		evil-want-keybinding nil)
  :config
  ;; Configuring initial major mode for some modes
  (evil-set-initial-state 'vterm-mode 'emacs)
  (evil-set-leader nil (kbd "C-SPC"))
  (evil-set-leader 'normal (kbd "SPC"))
  (evil-set-leader 'normal (kbd "m") t)

  (defun arg-vi--kill-this-buffer ()
	(interactive)
	(kill-buffer (current-buffer)))

  (require 'consult)
  (evil-define-key 'normal 'global
	(kbd "<leader>fs") 'save-buffer
	(kbd "<leader>ff") 'find-file
	(kbd "<leader>bb") 'consult-buffer
	(kbd "<leader>bB") 'ibuffer
	(kbd "<leader>bk") 'arg-vi--kill-this-buffer
	(kbd "<leader>bl") 'consult-line
	(kbd "<leader>bo") 'delete-other-windows
	(kbd "<leader>wh") 'winner-undo
	(kbd "<leader>wl") 'winner-redo
    (kbd "<leader>se") 'eshell
	(kbd "<leader>x")  'scratch-buffer))

;; `evil-collection' package configures various popular packages (build-in and
;; 3rd Party) to play well with `evil-mode'.
(use-package evil-collection
  :after evil
  :ensure t
  :config
  (evil-mode)
  (evil-collection-init))

;; `evil-commentary' is Emacs implementation of Tim Pope's popular plugin.
(use-package evil-commentary
  :after evil
  :ensure t
  :config
  (evil-commentary-mode))
