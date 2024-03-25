;;; arg-pers.el -- Personal Configurations

;; `debbugs' is the GNU + Debian Bug Tracker interface for Emacs.
(use-package debbugs
  :ensure t)

(use-package auth-source
  :config
  (setq epg-gpg-program "gpg2"
		auth-sources '((:source "~/.config/emacs/secrets/authinfo.gpg"))))

(use-package erc
  :config
  (setq erc-autojoin-channels-alist '(("irc.libera.chat"
									   "#emacs" "#emacsconf" "#systemcrafters"
									   "#org-mode" "#guile" "#go-nuts" "#debian"))
        erc-use-auth-source-for-nickserv-password t
		erc-track-shorten-start 9
		erc-kill-buffer-on-part t
		erc-fill-column 120
		erc-fill-function 'erc-fill-static
		erc-fill-static-center 20
		erc-kill-buffer-on-part t
		erc-kill-server-buffer-on-quit t
		erc-track-exclude-types '("JOIN" "NICK" "PART" "QUIT" "MODE" "AWAY")
		erc-track-exclude-server-buffer t
		erc-track-visibility nil)

  (defun arg-erc-libera ()
	(interactive)
	(erc-tls
	 :server "irc.libera.chat"
	 :port 6697
	 :nick "argp"))

  (require 'evil)
  (evil-define-key 'normal 'global
	(kbd "<leader>ic") #'arg-erc-libera
	(kbd "<leader>ib") 'erc-switch-to-buffer))

(use-package mastodon
  :ensure t
  :config
  (setq mastodon-instance-url "https://emacs.ch"
        mastodon-active-user "ankit"))
