;;; arg-pers.el -- Personal Configurations

(use-package auth-source
  :config
  (setq epg-gpg-program "gpg2"
		auth-sources '((:source "~/.config/emacs/secrets/authinfo.gpg"))))


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
  (setq empv-invidious-instance "https://invidious.fdn.fr/api/v1"
		empv-youtube-use-tabulated-results t
		empv-radio-channels
		'(("lofi hip hop radio 📚 - beats to relax/study to" . "https://www.youtube.com/watch?v=jfKfPfyJRdk")
		  ("lofi hip hop radio 💤 - beats to sleep/chill to" . "https://www.youtube.com/watch?v=rUxyKA_-grg")
		  ("synthwave radio 🌌 - beats to chill/game to" . "https://www.youtube.com/watch?v=4xDzrJKXOOY")
		  ("dark ambient radio 🌃 - music to escape/dream to" . "https://www.youtube.com/watch?v=S_MOd40zlYU")
		  ("peaceful piano radio 🎹 - music to focus/study to" . "https://www.youtube.com/watch?v=4oStw0r33so")
		  ("asian lofi radio ⛩️ - beats to relax/study to" . "https://www.youtube.com/watch?v=Na0w3Mz46GA")
		  ("medieval lofi radio 🏰 - beats to scribe manuscripts to" . "https://www.youtube.com/watch?v=_uMuuHk_KkQ"))))


;; `ellama' brings the Large language models in Emacs.
(use-package ellama
  :ensure t)
