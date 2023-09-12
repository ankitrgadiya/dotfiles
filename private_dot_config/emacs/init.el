;;; Add MELPA Repository
(with-eval-after-load 'package
  (add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t))



;;; UI Enhancements

(setq inhibit-splash-screen t)                        ; No Splash Screen
(blink-cursor-mode -1)                                ; No blinking
(pixel-scroll-precision-mode)                         ; Smooth scrolling
(menu-bar-mode -1)                                    ; Fullscreen, no menu-bar

(setq line-number-mode t                              ; Display Line number in Modeline
      column-number-mode t                            ; Display Column number in Modeline
      display-line-numbers-type 'visual               ; Relative Line numbers
      x-underline-at-descent-line nil                 ; Prettier underlines
      tab-bar-show 1)                                 ; Show Tab bar if there is more than one tab

(setq-default display-line-numbers-width 3            ; Sets Line number's minimum width
	      fill-column 80                          ; Sets Fill Column width
	      show-trailing-whitespace nil            ; By default, don't underline trailing spaces
	      indicate-buffer-boundaries 'left        ; Show buffer top and bottom in the margin, Looks Good!
	      tab-width 4)

; Display Line numbers for Programming Buffers
(add-hook 'prog-mode-hook 'display-line-numbers-mode)
(add-hook 'prog-mode-hook 'hl-line-mode)

; Display Line numbers with word-wrapping for Text Buffers
(add-hook 'text-mode-hook 'visual-line-mode)
(add-hook 'text-mode-hook 'hl-line-mode)

; Use built-in Modus Colorscheme
(use-package emacs
  :config
  (load-theme 'modus-vivendi))

(set-face-attribute 'default nil :font "Iosevka" :height 160)



;;; Sane Behaviours

(setq switch-to-buffer-obey-display-actions t         ; Make switching buffers more consistent
      initial-major-mode 'fundamental-mode            ; Default mode for the *scratch* buffer
      display-time-default-load-average nil           ; This information is useless for most
      sentence-end-double-space nil)

(setq auto-revert-interval 1                          ; Updates the File if modified outside Emacs
      auto-revert-check-vc-info t)                    ; Updates the File if Version control modifies it
(global-auto-revert-mode)

(savehist-mode)                                       ; Persist Minibuffer history

; Fix various packages (built-in and 3rd party) to use standard directories
; instead of littering the `user-emacs-directory'.
(use-package no-littering
  :ensure t
  :config
  ; Exclude the `no-littering' directories from `recentf'.
  (require 'recentf)
  (add-to-list 'recentf-exclude
               (recentf-expand-file-name no-littering-var-directory))
  (add-to-list 'recentf-exclude
               (recentf-expand-file-name no-littering-etc-directory))

  ; Configure backups explicitly.
  (no-littering-theme-backups)

  ; Set a dedicated file for Emacs customizations.
  (setq custom-file (no-littering-expand-etc-file-name "custom.el")))

(setq enable-recursive-minibuffers t                  ; Use the minibuffer whilst in the minibuffer
      tab-always-indent 'complete                     ; When I hit TAB, try to complete, otherwise, indent
      completion-cycle-threshold 1                    ; TAB cycles candidates
      completions-detailed t                          ; Show annotations
      completion-auto-help 'always                    ; Open completion always; `lazy' another option
      completions-max-height 20                       ; This is arbitrary
      completions-format 'one-column 
      completions-group t 
      completion-auto-select 'second-tab)             ; Much more eager

; TAB acts more like how it does in the shell
(keymap-set minibuffer-mode-map "TAB" 'minibuffer-complete)

; Load environment variables from Shell when not launched with proper
; environment.
(use-package exec-path-from-shell
  :ensure t
  :config
  (setq exec-path-from-shell-variables '("PATH" "MANPATH"
					 "SSH_AUTH_SOCK" "SSH_AGENT_PID"
					 "GPG_AGENT_INFO" "LANG" "LC_CTYPE"
					 "GOROOT" "GOPATH" "GOPRIVATE" "GOPROXY"
					 "RUSTUP_HOME" "CARGO_HOME"))
  (if (or (eq system-type 'darwin)
	  (daemonp))
      (exec-path-from-shell-initialize)))



;;; Vi Key Bindings

(setq evil-want-keybinding nil)

(use-package evil
  :ensure t
  :init
  (setq evil-respect-visual-line-mode t
	evil-undo-system 'undo-redo
	evil-want-C-u-scroll t)
  :config
  ;; Configuring initial major mode for some modes
  (evil-set-initial-state 'vterm-mode 'emacs))

(use-package evil-leader
  :ensure t
  :after evil
  :config
  (evil-leader/set-leader "<SPC>")
  (evil-leader/set-key
   "fs" 'save-buffer
   "ff" 'find-file)
  (evil-leader/set-key
    "bb" 'consult-buffer
    "bB" 'ibuffer
    "bk" 'kill-this-buffer
    "bo" 'delete-other-windows)
  (global-evil-leader-mode)
  (evil-mode))

(use-package evil-collection
  :after evil
  :ensure t
  :config
  (evil-collection-init))

;;; Minibuffer and Autocompletion

(use-package consult
  :ensure t
  ;; Other good things to bind: consult-ripgrep, consult-line-multi,
  ;; consult-history, consult-outline
  :bind (("C-x b" . consult-buffer) ; orig. switch-to-buffer
         ("M-y" . consult-yank-pop) ; orig. yank-pop
         ("C-s" . consult-line))    ; orig. isearch
  :config
  ;; Narrowing lets you restrict results to certain groups of candidates
  (setq consult-narrow-key "<"))

(use-package embark
  :ensure t
  :demand t
  :after avy
  :bind (("C-c a" . embark-act))        ; bind this to an easy key to hit
  :init
  ;; Add the option to run embark when using avy
  (defun bedrock/avy-action-embark (pt)
    (unwind-protect
        (save-excursion
          (goto-char pt)
          (embark-act))
      (select-window
       (cdr (ring-ref avy-ring 0))))
    t)

  ;; After invoking avy-goto-char-timer, hit "." to run embark at the next
  ;; candidate you select
  (setf (alist-get ?. avy-dispatch-alist) 'bedrock/avy-action-embark))

(use-package embark-consult
  :ensure t)

(use-package prescient
  :ensure t
  :config
  ;; (setq completion-styles '(prescient basic initials substring)))
  (setq completion-styles '(prescient)))

;; Vertico: better vertical completion for minibuffer commands
(use-package vertico
  :ensure t
  :init
  ;; You'll want to make sure that e.g. fido-mode isn't enabled
  (vertico-mode))

(use-package vertico-directory
  :after vertico
  :bind (:map vertico-map
              ("M-DEL" . vertico-directory-delete-word)))

(use-package vertico-prescient
  :ensure t
  :config
  (setq vertico-prescient-enable-filtering t
	vertico-prescient-enable-sorting t)
  (vertico-prescient-mode))

;; Marginalia: annotations for minibuffer
(use-package marginalia
  :ensure t
  :config
  (marginalia-mode))

;; Popup completion-at-point
(use-package corfu
  :ensure t
  :init
  (global-corfu-mode)
  :bind
  (:map corfu-map
        ("SPC" . corfu-insert-separator)
        ("C-n" . corfu-next)
        ("C-p" . corfu-previous)))

(use-package corfu-prescient
  :ensure t
  :config
  (setq corfu-prescient-enable-filtering t
	corfu-prescient-enable-sorting t)
  (corfu-prescient-mode))

;; Part of corfu
(use-package corfu-popupinfo
  :after corfu
  :hook (corfu-mode . corfu-popupinfo-mode)
  :custom
  (corfu-popupinfo-delay '(0.25 . 0.1))
  (corfu-popupinfo-hide nil)
  :config
  (corfu-popupinfo-mode))

;; Make corfu popup come up in terminal overlay
(use-package corfu-terminal
  :if (not (display-graphic-p))
  :ensure t
  :config
  (corfu-terminal-mode))

;; Pretty icons for corfu
(use-package kind-icon
  :if (display-graphic-p)
  :ensure t
  :after corfu
  :custom
  (kind-icon-default-face 'corfu-default) ; to compute blended backgrounds correctly
  :config
  (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter)
  (setq kind-icon-use-icons nil))

(use-package eshell
  :bind (("C-r" . consult-history)))

;; Orderless: powerful completion style
(use-package orderless
  :ensure t
  :config
  (setq completion-styles '(orderless)))



;;; Programming

; Enable Code Folding
(add-hook 'prog-mode-hook 'hs-minor-mode)
(add-hook 'prog-mode-hook 'electric-pair-mode)

; Go Configuration
(add-to-list 'auto-mode-alist (cons "\\.go\\'" 'go-ts-mode))
(add-to-list 'auto-mode-alist (cons "go\\.mod\\'" 'go-mod-ts-mode))
(setq go-ts-mode-indent-offset 4)

; Rust Configuration
(add-to-list 'auto-mode-alist (cons "\\.rs\\'" 'rust-ts-mode))

; C Configuration
(add-to-list 'auto-mode-alist (cons "\\.c\\'" 'c-ts-mode))

; Python Configuration
(add-to-list 'auto-mode-alist (cons "\\.py\\'" 'python-ts-mode))

; Bash Configuration
(add-to-list 'major-mode-remap-alist '(bash-mode . bash-ts-mode))

; Markdown Configuration
(use-package markdown-mode
  :ensure t
  :mode
  ("README\\.md\\'" . gfm-mode)
  :init
  (setq markdown-command '("pandoc" "--from=gfm" "--to=html5")))

(use-package yaml-mode
  :ensure t
  :config
  (add-to-list 'major-mode-remap-alist '(yaml-mode . yaml-ts-mode)))

(use-package dockerfile-mode
  :ensure t
  :config
  (add-to-list 'major-mode-remap-alist '(dockerfile-mode . dockerfile-ts-mode)))

; Use built-in Tree-sitter implementation.
(use-package treesit
  :init
  ; Configure Tree Sitter grammers for the Modes I use.
  (setq treesit-language-source-alist
	'((go . ("https://github.com/tree-sitter/tree-sitter-go"))
	  (gomod . ("https://github.com/camdencheek/tree-sitter-go-mod"))
	  (gowork . ("https://github.com/omertuc/tree-sitter-go-work"))
	  (rust . ("https://github.com/tree-sitter/tree-sitter-rust"))
	  (c . ("https://github.com/tree-sitter/tree-sitter-c"))
	  (cpp . ("https://github.com/tree-sitter/tree-sitter-cpp"))
	  (python . ("https://github.com/tree-sitter/tree-sitter-python"))
	  (lua . ("https://github.com/Azganoth/tree-sitter-lua"))
	  (bash . ("https://github.com/tree-sitter/tree-sitter-bash"))
	  (fish . ("https://github.com/ram02z/tree-sitter-fish"))
	  (sql . ("https://github.com/m-novikov/tree-sitter-sql"))
	  (json . ("https://github.com/tree-sitter/tree-sitter-json"))
	  (make . ("https://github.com/alemuller/tree-sitter-make"))
	  (dockerfile . ("https://github.com/camdencheek/tree-sitter-dockerfile"))
	  (toml . ("https://github.com/tree-sitter/tree-sitter-toml"))
	  (html . ("https://github.com/tree-sitter/tree-sitter-html"))
	  (yaml . ("https://github.com/ikatyang/tree-sitter-yaml")))))

; Use built-in Language Server Client for Language-aware features.
(use-package eglot
  :hook
  ; Enable Eglot on the Modes I use.
  (go-ts-mode . eglot-ensure)
  (rust-ts-mode . eglot-ensure)
  (bash-ts-mode . eglot-ensure)
  (c-ts-mode . eglot-ensure)
  :defines eglot-managed-p
  :custom
  (eglot-send-changes-idle-time 0.1)
  :config
  (fset #'jsonrpc--log-event #'ignore)               ; massive perf boost---don't log every event
  (setq eglot-extend-to-xref t                       ; Allows Eglot to add out of project files under LSP Server
	eglot-report-progress t
	eglot-ignored-server-capabilities '(:codeLensProvider :documentHighlightProvider))
  ;; Use Eglot to Format the buffer before saving.
  (defun arg/format-buffer ()
    (when (eglot-managed-p)
      (eglot-format-buffer)))
  (add-hook 'before-save-hook #'arg/format-buffer))

(setq eldoc-echo-area-use-multiline-p nil)



;;; Project and Workspace Management

; Use built-in Project management
(use-package project
  :config
  (if (eq system-type 'darwin)
      ; The built-in find may not support all possible options.
      (setq find-program "gfind"))
  ; Directly load find-file in Project.
  (setq project-switch-commands 'project-find-file)
  (evil-leader/set-key
    "p!" 'project-shell-command
    "p&" 'project-async-shell-command
    "pf" 'project-find-file
    "pF" 'project-or-external-find-file
    "pb" 'project-switch-to-buffer
    "ps" 'project-shell
    "pd" 'project-find-dir
    "pD" 'project-dired
    "pv" 'project-vc-dir
    "pc" 'project-compile
    "pe" 'project-eshell
    "pk" 'project-kill-buffers
    "pp" 'project-switch-project
    "pg" 'project-find-regexp
    "pG" 'project-or-external-find-regexp
    "pr" 'project-query-replace-regexp
    "px" 'project-execute-extended-command
    "p\C-b" 'project-list-buffers))

(use-package tab-bar
  :config
  (setq tab-bar-new-tab-choice "*scratch*"
	tab-bar-close-button-show nil)
  (evil-leader/set-key
    "<tab>n" 'tab-bar-new-tab
    "<tab>r" 'tab-bar-rename-tab
    "<tab>d" 'tab-bar-close-tab
    "<tab><tab>" 'tab-bar-switch-to-tab))

; Convenient cheatsheet for key-bindings in Minibuffer.
(use-package which-key
  :ensure t
  :config
  (which-key-mode))

; Magical Git experience
(use-package magit
  :ensure t
  :config
  (evil-leader/set-key
    "gg" 'magit-status
    "gR" 'magit-refresh-all
    "gd" 'magit-dispatch))

; Command Runner akin to Make
(use-package just-mode
  :ensure t)

(use-package avy
  :ensure t
  :demand t
  :bind (("C-c j" . avy-goto-line)
	 ("C-c c" . avy-goto-char)
         ("s-j"   . avy-goto-char-timer)))

