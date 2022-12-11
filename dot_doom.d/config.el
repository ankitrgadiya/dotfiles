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

;; Use visual to display the line numbers. Visual is like relative numbers, but
;; also works with folds.
(setq display-line-numbers-type 'visual)

;; Language Server - Eglot Configuration
(with-eval-after-load 'eglot
  (add-to-list 'eglot-server-programs
               '((python-mode python-ts-mode) . ("pyright"))))

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

