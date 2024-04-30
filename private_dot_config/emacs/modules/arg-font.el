;;; arg-font.el -- Font Configurations


;; `fontaine' package allows for easier Font configuration
(use-package fontaine
  :ensure t
  :config
  (defun arg-font--generate-presets ()
	(let ((height (if (eq system-type 'darwin)
					  160
					140)))
	  `((regular
		 :default-height ,height
		 :variable-pitch ,height)
		(zoomed
		 :default-height ,(+ 20 height)
		 :variable-pitch ,(+ 20 height))
		(presentation
		 :default-height ,(+ 40 height)
		 :variable-pitch ,(+ 40 height))
		(presentation-zoomed
		 :default-height ,(+ 60 height)
		 :variable-pitch ,(+ 60 height))
		(t
		 :default-family "Iosevka SS07"
		 :variable-pitch-family "Iosevka Aile"))))

  (setq fontaine-presets (arg-font--generate-presets))
  (fontaine-set-preset 'regular))
