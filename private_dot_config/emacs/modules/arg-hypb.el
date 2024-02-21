;;; arg-hypb.el -- GNU Hyperbole Configurations

(use-package hyperbole
  :ensure t
  :config
  (hyperbole-mode 1)
  (require 'evil-leader)
  (evil-leader/set-key
	"h" 'hyperbole))
