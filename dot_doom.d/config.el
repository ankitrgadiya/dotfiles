;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;;; Copyright 2020, Ankit R Gadiya

;; Introducing mysql to Emacs
(setq user-full-name "Ankit R Gadiya"
      user-mail-address "git@argp.in")

;; I like to use Iosevka fonts.
(setq doom-font (font-spec :family "Iosevka" :size 16))

;; Doom Emacs comes with loads of themes pre-loaded. This sets the theme to
;; Gruvbox.
(setq doom-theme 'doom-gruvbox)

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type 'relative)
