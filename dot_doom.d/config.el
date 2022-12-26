;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;;; Copyright 2020-2022, Ankit R Gadiya

;; Introductions
(setq user-full-name    "Ankit R Gadiya"
      user-mail-address "git@argp.in")

;; I like to use Iosevka fonts.
(setq doom-font (font-spec :family "Iosevka Heavy Extended" :size 16))

;; Doom Emacs comes with loads of themes pre-loaded. This sets the theme to
;; Gruvbox.
(setq doom-theme 'doom-gruvbox)

;; White-label Doom Emacs
(setq icon-title-format "%b – Emacs"
      frame-title-format "%b - Emacs")

;; Use visual to display the line numbers. Visual is like relative numbers, but
;; also works with folds.
(setq display-line-numbers-type 'visual)

;; Language Server - Eglot Configuration
(with-eval-after-load 'eglot
  (add-to-list 'eglot-server-programs
               '((python-mode python-ts-mode) . ("pyright"))))

;; Folding Configuration
;;
;; Disable comment folding in Go
(advice-add #'ts-fold-parsers-go
            ;; Removes comment parser from the pre-registered Go parsers.
            :filter-return (lambda (parsers) (list (car parsers))))

;; Load Project-specific ELisp module
;;
;; I'm using this to implement RunConfigurations with Emacs integration.
;; Issues:
;;   - If projects are switched using persp-mode, the hooks are not triggered.
;;   - If buffers are not closed using projectile functions, the hooks are not
;;     triggered.
(defun arg/project-module-filename ()
  (concat (projectile-project-root)
          (string-replace "_" "" (concat (projectile-project-name) ".el"))))

(defun arg/project-module-name ()
  (intern (string-replace "_" "" (projectile-project-name))))

(defun arg/load-project-module ()
  (let ((name (arg/project-module-filename)))
    (when (file-exists-p! name (projectile-project-root))
      (message "loading %s now" name)
      (load-file name))))

(defun arg/unload-project-module ()
  (let ((name (arg/project-module-name)))
    (when (featurep name)
      (message "unloading %s" name)
      (unload-feature name))))

;; Auto-load module when I switch projects using Projectile.
(add-hook 'projectile-after-switch-project-hook
          #'arg/load-project-module)

;; Unload module when I switch to a different project.
(add-hook 'projectile-before-switch-project-hook
          #'arg/unload-project-module)

;; Unload module when I kill all projectile buffers.
(advice-add #'projectile-kill-buffers :before #'arg/unload-project-module)


;; Restclient Hacks
;;
;; The upstream restclient does not support substituting variables in ELisp
;; S-expressions for variables. These advices add the support for it.

;; To get all the variables, it needs to parse each variable until the current
;; point.
;;
;; This advice overrides the upstream function with this. It passes the vars
;; alist until now to the restclient-eval-var function as replacements.
(advice-add #'restclient-find-vars-before-point
            :override
            (lambda ()
              (let ((vars nil)
                    (bound (point)))
                (save-excursion
                  (goto-char (point-min))
                  (while (search-forward-regexp restclient-var-regexp bound t)
                    (let ((name (match-string-no-properties 1))
                          (should-eval (> (length (match-string 2)) 0))
                          (value (or (restclient-chop (match-string-no-properties 4)) (match-string-no-properties 3))))
                      (setq vars (cons (cons name (if should-eval (restclient-eval-var value vars) value)) vars))))
                  (append restclient-var-overrides vars)))))

;; Before we try to evaluate the string, replace all the variables with there
;; values from the replacement alist.
(advice-add #'restclient-eval-var
            :override
            (lambda (content vars)
              (let ((input (restclient-replace-all-in-string vars content)))
                (with-output-to-string (princ (eval (read input)))))))
