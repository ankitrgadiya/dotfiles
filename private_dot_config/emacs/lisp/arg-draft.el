;;; arg-draft.el --- Compose Mode         -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:
(require 'flyspell)

(defcustom arg-draft-major-mode 'text-mode
  "Define the Major mode for the draft buffer."
  :type 'function)

(defcustom arg-draft-buffer-name "*Draft*"
  "Name of the draft buffer."
  :type 'string)

;;;###autoload
(defun arg-draft ()
  "Opens the buffer to quickly draft some text."
  (interactive)
  (let (buffer (get-buffer-create arg-draft-buffer-name))
	(switch-to-buffer arg-draft-buffer-name)
	(funcall arg-draft-major-mode)
	(if (derived-mode-p 'text-mode)
		(turn-on-flyspell)
	  (flyspell-prog-mode 1))))
