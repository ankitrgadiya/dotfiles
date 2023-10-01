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
  (evil-set-initial-state 'vterm-mode 'emacs))

;; `evil-leader' brings the Leader key from Vi. It exposes convenice functions
;; to use them.
(use-package evil-leader
  :ensure t
  :after evil
  :config
  (require 'consult)
  (evil-leader/set-leader "<SPC>")
  (evil-leader/set-key
   "fs" 'save-buffer
   "ff" 'find-file)
  (evil-leader/set-key
    "bb" 'consult-buffer
    "bB" 'ibuffer
    "bk" 'kill-this-buffer
    "bo" 'delete-other-windows)
  (require 'magit)
  ;; Configure (some of) Doomemacs' key-bindings for Magit.
  (evil-leader/set-key
    "gg" 'magit-status
    "gR" 'magit-refresh-all
    "gd" 'magit-dispatch)
  (global-evil-leader-mode)
  (evil-mode))

;; `evil-collection' package configures various popular packages (build-in and
;; 3rd Party) to play well with `evil-mode'.
(use-package evil-collection
  :after evil
  :ensure t
  :config
  (evil-collection-init))

;; `evil-commentary' is Emacs implementation of Tim Pope's popular plugin.
(use-package evil-commentary
  :after evil
  :ensure t
  :config
  (evil-commentary-mode))
