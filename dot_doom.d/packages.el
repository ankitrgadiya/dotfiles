;; -*- no-byte-compile: t; -*-
;;; $DOOMDIR/packages.el

;; Emacs GUIX is a Magit-like interface for GNU GUIX package manager.
;;
;; Note: The Github repository is not being updated, I'm pulling directly from
;; Savannah.
(package! guix
  :recipe (:host nil
           :repo "https://git.savannah.gnu.org/git/guix/emacs-guix.git"))

(package! flycheck-golangci-lint :disable t)

(package! dest :recipe
  (:host nil
   :repo "https://git.argc.in/ankit/dest"
   :files ("*.el")))
