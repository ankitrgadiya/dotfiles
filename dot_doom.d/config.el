;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;;; Copyright 2020, Ankit R Gadiya

;; Introducing mysql to Emacs
(setq user-full-name "Ankit R Gadiya"
      user-mail-address "git@argp.in")

;; I like to use Iosevka fonts.
(setq doom-font (font-spec :family "Iosevka Heavy Extended" :size 16))

;; Doom Emacs comes with loads of themes pre-loaded. This sets the theme to
;; Gruvbox.
(setq doom-theme 'doom-gruvbox)

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type 'visual)

;; Configure Fossil backend for VC Mode
(add-to-list 'vc-handled-backends 'Fossil t)

;; Enable interactive JQ for JSON Documents
(use-package! jq-mode
  :defer t
  :init
  (map! :after json-mode
        :map json-mode-map
        :localleader
        "i" #'jq-interactively))

;; Enable JQ integration for Restclient
(use-package! restclient-jq
  :after restclient)

;; Set Org configuration
(setq org-directory "~/Nextcloud/Org"
      org-journal-dir "~/Nextcloud/Org"
      org-journal-file-format "journal-%Y"
      org-journal-file-type 'yearly)
