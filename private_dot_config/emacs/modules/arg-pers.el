;;; arg-pers.el -- Personal Configurations

(use-package arg-status
  :config
  (require 'exec-path-from-shell)
  (setq arg-status-path (exec-path-from-shell-getenv "ARG_STATUS_PATH"))
  (evil-leader/set-key
	"ws" 'arg-status))
