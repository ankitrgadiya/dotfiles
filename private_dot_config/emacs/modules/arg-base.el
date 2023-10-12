;;; arg-base.el -- Base Configurations


;; Misc Configurations
(setq-default fill-column 80                          ; Sets Fill Column width
			  show-trailing-whitespace nil            ; By default, don't underline trailing spaces
			  indicate-buffer-boundaries 'left        ; Show buffer top and bottom in the margin, Looks Good!
			  tab-width 4)

(setq switch-to-buffer-obey-display-actions t         ; Make switching buffers more consistent
      initial-major-mode 'fundamental-mode            ; Default mode for the *scratch* buffer
      display-time-default-load-average nil           ; This information is useless for most
      sentence-end-double-space nil
	  enable-recursive-minibuffers t                  ; Use the minibuffer whilst in the minibuffer
      tab-always-indent 'complete                     ; When I hit TAB, try to complete, otherwise, indent
	  line-number-mode t                              ; Display Line number in Modeline
      column-number-mode t                            ; Display Column number in Modeline
      x-underline-at-descent-line nil                 ; Prettier underlines
      completion-cycle-threshold 1                    ; TAB cycles candidates
      completions-detailed t                          ; Show annotations
      completion-auto-help 'always                    ; Open completion always; `lazy' another option
      completions-max-height 20                       ; This is arbitrary
	  create-lockfiles nil                            ; Prevent creating lockfiles. I'm the only editor.
      completions-format 'one-column
      completions-group t
      completion-auto-select 'second-tab)             ; Much more eager

;; TAB acts more like how it does in the shell
(keymap-set minibuffer-mode-map "TAB" 'minibuffer-complete)

;; `autorevert' is a built-in minor-mode that enables reverting the buffer
;; content when the file changes on disk. This is specially useful in
;; conjunction with a VCS.
(use-package autorevert
  :config
  (setq auto-revert-interval 1                          ; Updates the File if modified outside Emacs
		auto-revert-check-vc-info t)                    ; Updates the File if Version control modifies it
  (global-auto-revert-mode))

;; `savehist' is a built-in minor-mode that remembers the minibuffer history.
(use-package savehist
  :config
  ; I keep the sql login configuration in the project-local `.dir-locals.el'.
  ; Turning off saving historing for sql-connect function.
  (setq savehist-ignored-variables '(sql-connect))
  (savehist-mode))

;; Emacs packages (even built-ins) don't follow the convention to store files.
;; It can cause litter in the `user-emacs-directory' over time. The
;; `no-littering' configures / patches various common packages to use the
;; standard directories under `user-emacs-directory'.
(use-package no-littering
  :ensure t
  :config
  ; Configure backups explicitly.
  (no-littering-theme-backups)

  ; Set a dedicated file for Emacs customizations.
  (setq custom-file (no-littering-expand-etc-file-name "custom.el"))

  ; Exclude the `no-littering' directories from `recentf'.
  (require 'recentf)
  (add-to-list 'recentf-exclude
               (recentf-expand-file-name no-littering-var-directory))
  (add-to-list 'recentf-exclude
               (recentf-expand-file-name no-littering-etc-directory)))

;; In some cases, Emacs might not be launched with proper environment variables
;; (MacOS, systemd, etc). The `exec-path-from-shell' package loads the correct
;; environment variables by starting the default user-shell.
(use-package exec-path-from-shell
  :ensure t
  :config
  (setq exec-path-from-shell-variables
		'("PATH" "MANPATH" "SSH_AUTH_SOCK" "SSH_AGENT_PID"
		  "GPG_AGENT_INFO" "LANG" "LC_CTYPE"
		  "GOROOT" "GOPATH" "GOPRIVATE" "GOPROXY"
		  "RUSTUP_HOME" "CARGO_HOME"
		  "GUILE_LOAD_PATH" "GUILE_LOAD_COMPILED_PATH"
		  "GUILE_SYSTEM_EXTENSIONS_PATH" "GUILE_TLS_CERTIFICATE_DIRECTORY"
		  ))
  (if (or (eq system-type 'darwin)
		  (daemonp))
      (exec-path-from-shell-initialize)))


;; `which-key' is a convenient cheatsheet for key-bindings in Minibuffer.
(use-package which-key
  :ensure t
  :config
  (which-key-mode))


;; `tramp' is a powerful remote file-editing for Emacs.
(use-package tramp
  :config
  ;; Configure Tramp to be as fast as possible.
  (setq remote-file-name-inhibit-cache nil
		vc-ignore-dir-regexp (format "%s\\|%s"
									 vc-ignore-dir-regexp
									 tramp-file-name-regexp)
		tramp-verbose 1))


(use-package consult
  :ensure t
  :config
  (setq consult-narrow-key "<"))

(use-package vertico
  :ensure t
  :init
  (vertico-mode))

(use-package marginalia
  :ensure t
  :config
  (marginalia-mode))

(use-package corfu
  :ensure t
  :init
  (global-corfu-mode)
  :bind
  (:map corfu-map
        ("SPC" . corfu-insert-separator)
        ("C-n" . corfu-next)
  :config

(use-package corfu-popupinfo
  :after corfu
  :hook (corfu-mode . corfu-popupinfo-mode)
  :custom
  (corfu-popupinfo-delay '(0.25 . 0.1))
  (corfu-popupinfo-hide nil)
  :config
  (corfu-popupinfo-mode))

(use-package corfu-terminal
  :if (not (display-graphic-p))
  :ensure t
  :config
  (corfu-terminal-mode))

(use-package kind-icon
  :if (display-graphic-p)
  :ensure t
  :after corfu
  :custom
  (kind-icon-default-face 'corfu-default)
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
