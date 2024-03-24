;;; Add MELPA Repository
(with-eval-after-load 'package
  (add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t))

(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

(setq user-full-name "Ankit Gadiya"
	  user-mail-address "ankit@argp.in")

;;; UI Enhancements

(setq inhibit-splash-screen t)                        ; No Splash Screen
(blink-cursor-mode -1)                                ; No blinking
(pixel-scroll-precision-mode)                         ; Smooth scrolling
(menu-bar-mode -1)                                    ; Fullscreen, no menu-bar

;; Use the built-in Modus themes.
(use-package emacs
  :ensure t
  :config
  (setq modus-themes-paren-match '(bold)
		modus-themes-bold-constructs t
		modus-themes-italic-constructs t)
  (load-theme 'modus-vivendi))

;; Configure Iosevka Fonts.
(set-face-attribute 'default nil :font "Iosevka" :height 120)
(if (daemonp)
	(add-hook 'after-make-frame-functions
			  (lambda (frame)
				(set-face-attribute 'default nil :font "Iosevka" :height 120))))

(load-file (expand-file-name "modules/arg-base.el" user-emacs-directory))
(load-file (expand-file-name "modules/arg-vi.el" user-emacs-directory))
(load-file (expand-file-name "modules/arg-prog.el" user-emacs-directory))
(load-file (expand-file-name "modules/arg-proj.el" user-emacs-directory))
(load-file (expand-file-name "modules/arg-hypb.el" user-emacs-directory))
(load-file (expand-file-name "modules/arg-pros.el" user-emacs-directory))
(load-file (expand-file-name "modules/arg-pers.el" user-emacs-directory))
(load-file (expand-file-name "modules/arg-work.el" user-emacs-directory))
