;;; arg-font.el -- Font Configurations


;; `fontaine' package allows for easier Font configuration
(use-package fontaine
  :ensure t
  :config
  (setq fontaine-presets
		`((t14
		   :default-height 140
		   :variable-pitch-height 140)
		  (t14-presentation
		   :default-height 180
		   :variable-pitch-height 180)
		  (mac
		   :default-height 160
		   :variable-pitch-height 160)
		  ,(if (eq system-type 'darwin)
			   '(regular :inherit mac)
			 '(regular :inherit t14))
		  ,(if (eq system-type 'darwin)
			   '(presentation :inherit mac-presentation)
			 '(presentation :inherit t14-presentation))
		  (t
		   :default-family "Iosevka SS07"
		   :variable-pitch-family "Iosevka Aile")))
  (fontaine-set-preset 'regular))
