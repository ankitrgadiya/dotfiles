;;; arg-work.el -- Work Configurations

(use-package arg-hwil
  :config
  (arg-hwil-setup)
  (evil-leader/set-key
	"he" 'arg-hwil-eshell-device))
