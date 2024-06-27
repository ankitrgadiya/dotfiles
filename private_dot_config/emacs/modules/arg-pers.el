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
