;;; arg-pers.el -- Personal Configurations

;; `debbugs' is the GNU + Debian Bug Tracker interface for Emacs.
(use-package debbugs
  :ensure t)

(use-package auth-source
  :config
  (setq epg-gpg-program "gpg2"
		auth-sources '((:source "~/.config/emacs/secrets/authinfo.gpg"))))

;; `erc' is the built-in IRC Client for Emacs.
(use-package erc
  :defer t
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

;; `mastodon' is the Mastodon Client for Emacs.
(use-package mastodon
  :ensure t
  :defer t
  :config
  (setq mastodon-instance-url "https://emacs.ch"
        mastodon-active-user "ankit")
  (evil-set-initial-state 'mastodon-mode 'emacs))

;; `empv' is the Media manager in Emacs (backed by the MPV player).
(use-package empv
  :ensure t
  :init
  (evil-define-key 'normal 'global
	(kbd "<leader>yt") 'empv-toggle
	(kbd "<leader>ys") 'empv-youtube
	(kbd "<leader>yr") 'empv-play-radio
	(kbd "<leader>yq") 'empv-exit)
  :defer t
  :config
  (evil-set-initial-state 'empv-youtube-results-mode 'emacs)
  (setq empv-invidious-instance "https://vid.lilay.dev/api/v1"
		empv-youtube-use-tabulated-results t
		empv-radio-channels
		'(("ghibli music collection 🍉 - beats for work/relax/study" . "https://www.youtube.com/watch?v=Sx4xVyXHl60")
		  ("lofi hip hop radio 📚 - beats for relax/study". "https://www.youtube.com/watch?v=jfKfPfyJRdk")
		  ("lofi hip hop radio 💤 - beats for sleep/chill" . "https://www.youtube.com/watch?v=rUxyKA_-grg")
		  ("synthwave radio 🌌 - beats for chill/game" . "https://www.youtube.com/watch?v=4xDzrJKXOOY")
		  ("dark ambient radio 🌃 - music for escape/dream" . "https://www.youtube.com/watch?v=S_MOd40zlYU"))))

;; `ellama' brings the Large language models in Emacs.
(use-package ellama
  :ensure t)
