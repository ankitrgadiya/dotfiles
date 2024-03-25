;;; arg-hypb.el -- GNU Hyperbole Configurations

(use-package hyperbole
  :ensure t
  :defer t
  :config
  (hyperbole-mode 1)
  (require 'evil)
  (evil-define-key 'normal 'global
	(kbd "<leader>h") 'hyperbole))
