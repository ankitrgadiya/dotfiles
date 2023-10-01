;;; arg-hwil.el --- Hardware-in-Loop                     -*- lexical-binding: t; -*-

;;; Commentary:

;; Hardware-in-Loop is a Service used in Rapyuta.io for creating and managing
;; virtual devices for testing the platform. This module enables support for
;; interacting with the virtual devices in Emacs. First and fore-most, this
;; package adds support for HWIL in the tramp-mode. It also defines a
;; convenience function to launch Eshell in the Device.

;;; Code:

(defcustom arg-hwil-program "hwil"
  "Name of the HWIL program to use for interacting with HWIL APIs.")

(defun arg-hwil--list-devices ()
  (mapcar #'arg-hwil--parse-devices (process-lines arg-hwil-program "list-tsv")))

(defun arg-hwil--parse-devices (line)
  (list nil
		(caddr (split-string line "[[:space:]]+" t))))

(defun arg-hwil--tramp-completion (&optional ignored)
  (arg-hwil--list-devices))

;;;###autoload
(defun arg-hwil-eshell-device (device)
  "Opens `eshell' in the HWIL Device."
  (interactive (list (completing-read "Device: "
									  (mapcar #'cadr (arg-hwil--list-devices)))))
  (let ((default-directory (concat "/hwil:" device ":/"))
		(eshell-buffer-name (concat "*arg-hwil " device "*")))
	(eshell)))

;;;###autoload
(defun arg-hwil-setup ()
  "Adds hwil support in `tramp-mode'."
  (progn
	(add-to-list 'tramp-methods
				 `("hwil"
				   (tramp-login-program ,arg-hwil-program)
				   (tramp-login-args (("ssh") ("%h")))
				   (tramp-remote-shell "/bin/bash")
				   (tramp-remote-shell-args ("-i" "-c"))))
	(tramp-set-completion-function "hwil" '((arg-hwil--tramp-completion "")))))

(provide 'arg-hwil)
