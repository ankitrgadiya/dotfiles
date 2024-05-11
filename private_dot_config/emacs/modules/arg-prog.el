;;; arg-prog.el -- Programming Configurations

;; `display-line-numbers' enables line numbers in the buffer.
(use-package display-line-numbers
  :hook
  (prog-mode . display-line-numbers-mode)
  (yaml-ts-mode . display-line-numbers-mode)
  :config
  ; Display relative numbers, accounting for folds.
  (setq display-line-numbers-type 'visual)
  (setq-default display-line-numbers-width 3))

;; `hl-line' enables highlighting the current line in the buffer.
(use-package hl-line
  :hook ((prog-mode . hl-line-mode)
		 (text-mode . hl-line-mode)))

;; `visual-line-mode' enables word-wrapping in the buffer.
(add-hook 'text-mode-hook 'visual-line-mode)

;; `delete-trailing-whitespace' removes the tailing whitespace.
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; `hideshow' is a built-in minor-mode that provides code-folding. If evil-mode
;; is enabled, it can automatically use it for folding with vim-like
;; key-bindings.
(use-package hideshow
  :hook (prog-mode . hs-minor-mode))

;; `elec-pair' is a built-in minor-mode that enables auto-pairing of parens (or
;; similar delimiters).
(use-package elec-pair
  :hook (prog-mode . electric-pair-mode))


;; The Treesitter library implements Syntax-tree implementations for programming
;; languages. This gives a better way to access the language buffers as opposed
;; to the traditional way using regular expressions. Emacs 29 comes built-in
;; with `treesit' module to use Treesitter. However, the language grammers must
;; be installed separately. This configures the Language sources for various
;; languages that I work with. The grammers can be installed using the
;; `treesit-install-language-grammar' command.
(use-package treesit
  :init
  ; Configure Tree Sitter grammers for the Modes I use.
  (setq treesit-language-source-alist
	'((go . ("https://github.com/tree-sitter/tree-sitter-go"))
	  (gomod . ("https://github.com/camdencheek/tree-sitter-go-mod"))
	  (gowork . ("https://github.com/omertuc/tree-sitter-go-work"))
	  (rust . ("https://github.com/tree-sitter/tree-sitter-rust"))
	  (python . ("https://github.com/tree-sitter/tree-sitter-python"))
	  (c . ("https://github.com/tree-sitter/tree-sitter-c"))
	  (cpp . ("https://github.com/tree-sitter/tree-sitter-cpp"))
	  (lua . ("https://github.com/tree-sitter-grammars/tree-sitter-lua"))
	  (bash . ("https://github.com/tree-sitter/tree-sitter-bash"))
	  (yaml . ("https://github.com/ikatyang/tree-sitter-yaml"))
	  (json . ("https://github.com/tree-sitter/tree-sitter-json"))
	  (dockerfile . ("https://github.com/camdencheek/tree-sitter-dockerfile")))))

(defun arg-prog--install-ts-grammer (lang)
  (unless (treesit-language-available-p lang)
	(treesit-install-language-grammar lang)))

;; Go Configurations
;; The canonical package for Go support is dominikh/go-mode.el. However, Emacs
;; 29.1 comes built-in with `go-ts-mode'. I'm now using treesitter mode over the
;; canonical package.
(use-package go-ts-mode
  :init
  (progn (arg-prog--install-ts-grammer 'go)
		 (arg-prog--install-ts-grammer 'gomod)
		 (arg-prog--install-ts-grammer 'gowork))
  :mode (("\\.go\\'"    . go-ts-mode)
  		 ("go\\.mod\\'" . go-mod-ts-mode))
  :config
  (setq go-ts-mode-indent-offset 4)
  (defun arg-prog--go-test-get-functions-in-range (beg end)
	(let* ((curnode (or (treesit-thing-at beg treesit-defun-type-regexp)
						(treesit-thing-next beg treesit-defun-type-regexp)))
		   (name (treesit-defun-name curnode)))
	  ;; (if (< (treesit-node-end curnode) end)
	  ;; 	  (cons name (arg-prog--go-test-get-functions-in-range (treesit-node-end curnode) end))
	  ;; 	(cons name nil))))
	  (if (or (> beg (treesit-node-end curnode))
			  (< end (treesit-node-start curnode)))
		  nil
		  (cons name (arg-prog--go-test-get-functions-in-range (treesit-node-end curnode) end)))))
  (defun arg-prog--go-test-compile (regexp dir)
	(compile (format "go test -v -run '%s' %s" regexp dir)))
  (defun arg-prog--go-test-function-at-point ()
	"Runs Go Test for the function at point"
	(interactive)
	(let ((regexp (if (region-active-p)
					  (string-join (arg-prog--go-test-get-functions-in-range (region-beginning)
																			 (region-end))
								   "|")
					(treesit-defun-name (treesit-defun-at-point)))))
	  (arg-prog--go-test-compile regexp default-directory)))
  (defun arg-prog--go-test-file-at-point ()
	"Runs Go Test for the File at point"
	(interactive)
	(arg-prog--go-test-compile (string-join (mapcar #'car (cdr (assoc "Function"
																	  (treesit-simple-imenu)))))))
  (defun arg-prog--go-test-directory-at-point ()
	"Runs Go Test for the Directory at point"
	(interactive)
	(compile (format "go test -v -run %s" default-directory)))

  (require 'mode-local)
  (setq-mode-local go-ts-mode adaptive-fill-regexp "[ \t]*//+[ \t]*")

  (require 'evil)
  (evil-define-key 'normal go-ts-mode-map
	(kbd "<leader>mtt") #'arg-prog--go-test-function-at-point
	(kbd "<leader>mtf") #'arg-prog--go-test-file-at-point
	(kbd "<leader>mtd") #'arg-prog--go-test-directory-at-point)
  (evil-define-key 'visual go-ts-mode-map
	(kbd "C-c mtt") #'arg-prog--go-test-function-at-point))

;; Rust Configurations
;; Emacs 29.1 comes built-in with `rust-ts-mode'.
(use-package rust-ts-mode
  :init
  (arg-prog--install-ts-grammer 'rust)
  :mode "\\.rs\\'")

;; Python Configurations
;; Emacs 29.1 comes built-in with `python-ts-mode'.
(use-package python-ts-mode
  :init
  (arg-prog--install-ts-grammer 'python)
  :mode "\\.py[iw]?\\'"
  :interpreter "python[0-9.]*")

;; Zig Configurations
(use-package zig-mode
  :ensure t
  :mode "\\.\\(zig\\|zon\\)\\'")

;; C/C++ Configurations
(use-package c-ts-mode
  :init
  (progn (arg-prog--install-ts-grammer 'c)
		 (arg-prog--install-ts-grammer 'cpp)))

;; Lua Configurations
;; Emacs 30 comes built-in with `lua-ts-mode'.
(use-package lua-ts-mode
  :mode "\\.lua\\'")

;; Fish Shell Configurations
(use-package fish-mode
  :ensure t
  :mode (("\\.fish\\'"           . fish-mode)
  		 ("/fish_funced\\..*\\'" . fish-mode))
  :interpreter "fish")

;; YAML Configurations
(use-package yaml-ts-mode
  :init
  (arg-prog--install-ts-grammer 'yaml)
  :mode "\\.ya?ml\\'")

;; JSON Configurations
(use-package json-ts-mode
  :init
  (arg-prog--install-ts-grammer 'json)
  :mode "\\.json\\'")

(use-package jq-mode
  :ensure t)

;; Dockerfile Configurations
;; Emacs 29.1 comes built-in with `dockerfile-ts-mode'.
(use-package dockerfile-ts-mode
  :init
  (arg-prog--install-ts-grammer 'dockerfile)
  :mode "\\(?:Dockerfile\\(?:\\..*\\)?\\|\\.[Dd]ockerfile\\)\\'")

;; Just Configurations
(use-package just-mode
  :ensure t)

;; `eglot' is the built-in language server client for Emacs. LSP Clients can add
;; rich semantics understanding of languages to Emacs.
(use-package eglot
  :hook ((go-ts-mode          . eglot-ensure)
		 (rust-ts-mode        . eglot-ensure)
		 (python-ts-mode      . eglot-ensure)
		 (bash-ts-mode        . eglot-ensure)
		 (c-or-c++-ts-mode    . eglot-ensure)
		 (zig-mode            . eglot-ensure))
  :custom
  (eglot-send-changes-idle-time 0.1)
  :config
  ;; Disables excessive logging for JSONRPC
  (fset #'jsonrpc--log-event #'ignore)
  (setq
   ;; Adds out-of-project files (dependencies) under LSP Server.
   eglot-extend-to-xref t
   ;; Eglot will report progress for long-running RPCs.
   eglot-report-progress t
   ;; Disable logging of events.
   eglot-events-buffer-size 0
   ;; Shutdown LSP Server after closing the last buffer.
   eglot-autoshutdown t
   ;; Capabilities that I want to disable globally.
   eglot-ignored-server-capabilities '(:documentHighlightProvider
									   :hoverProvider))

  ;; Workspace configuration per Server
  (setq-default eglot-workspace-configuration
				'(:gopls (:usePlaceholders t)))

  ;; Use Eglot to Format the buffer before saving.
  (defun arg-prog--format-buffer ()
    (when (eglot-managed-p)
      (eglot-format-buffer)))
  (add-hook 'before-save-hook #'arg-prog--format-buffer)

  (require 'eldoc)
  (setq eldoc-idle-delay 0.75))

;; `dape' is the DAP client for Emacs. DAP Clients add a generic Debugging
;; Client for languages that implement Debug Adapter Protocol based Debuggers.
(use-package dape
  :ensure t
  :defer t
  :init
  (setq dape-buffer-window-arrangement 'right)
  :config
  (require 'evil)
  (evil-define-key
    (kbd "<leader>d") #'dape
    (kbd "<leader>p") #'dape-pause
    (kbd "<leader>c") #'dape-continue
    (kbd "<leader>n") #'dape-next
    (kbd "<leader>s") #'dape-step-in
    (kbd "<leader>o") #'dape-step-out
    (kbd "<leader>r") #'dape-restart
    (kbd "<leader>i") #'dape-info
    (kbd "<leader>R") #'dape-repl
    (kbd "<leader>m") #'dape-read-memory
    (kbd "<leader>l") #'dape-breakpoint-log
    (kbd "<leader>e") #'dape-breakpoint-expression
    (kbd "<leader>b") #'dape-breakpoint-toggle
    (kbd "<leader>B") #'dape-breakpoint-remove-all
    (kbd "<leader>t") #'dape-select-thread
    (kbd "<leader>S") #'dape-select-stack
    (kbd "<leader>x") #'dape-evaluate-expression
    (kbd "<leader>w") #'dape-watch-dwim
    (kbd "<leader>D") #'dape-disconnect-quit
    (kbd "<leader>q") #'dape-quit))


;; `compile' defines the compilation for the Project.
(use-package compile
  :defer t
  :config
  (add-hook 'compilation-filter-hook 'ansi-color-compilation-filter)

  (require 'evil)
  (require 'consult)
  (evil-define-key 'normal compilation-mode-map
	(kbd "<leader>me") 'consult-compile-error))

(use-package flymake
  :defer t
  :config
  (require 'evil)
  (require 'consult)
  (evil-define-key 'normal flymake-mode-map
	(kbd "<leader>me") 'consult-flymake))


;; `geiser' is the super-powerful mode for Lisp goodness.
(use-package geiser
  :ensure t
  :defer t)

;; `geiser-guile' adds support for GNU Guile in `geiser'.
(use-package geiser-guile
  :ensure t
  :defer t)

;; `sly' is like `geiser' but for Common Lisp.
(use-package sly
  :ensure t
  :mode "\\.lisp\\'"
  :defer t
  :config
  (if (eq system-type 'darwin)
	  (setq inferior-lisp-program "/opt/homebrew/bin/sbcl")
	(setq inferior-lisp-program "/usr/bin/sbcl")))


;; `eldoc' is the built-in documentation system for Emacs.
(use-package eldoc
  :config
  ;; Don't resize the echo area if the documentation is longer. This is very
  ;; distracting when combined with Eglot's highlight functionality.
  (setq eldoc-echo-area-use-multiline-p nil))

;; `yasnippet' is a snippet injection system. I only use it along with Eglot's
;; signature expansion feature.
(use-package yasnippet
  :ensure t
  :defer t
  :hook
  (prog-mode-hook . yas-minor-mode))

;; `restclient' is the HTTP REST API client for Emacs.
(use-package restclient
  :ensure t
  :defer t
  :mode ("\\.http\\'" . restclient-mode)
  :hook (restclient-mode . display-line-numbers-mode)
  :config
  (setq restclient-same-buffer-response t))

;; `pikchr' is a markup language for Diagrams.
(use-package pikchr-mode
  :ensure t
  :mode ("\\.pikchr" . pikchr-mode)
  :after org
  :config
  (require 'pikchr-mode))
