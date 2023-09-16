;;; arg-stern.el --- Hardware-in-Loop                     -*- lexical-binding: t; -*-

(defun arg-stern--list-deployments ()
  (process-lines "kubectl"
				 "get"
				 "deployments"
				 "--no-headers"
				 "-o=custom-columns=NAME:.metadata.name"))

;;;###autoload
(defun arg-stern-follow (deployment)
  "Opens Stern Logs in a new buffer."
  (interactive (list (completing-read "Deployment: "
									  (arg-stern--list-deployments))))
  (let ((name (concat "*stern - " deployment " *")))
	(start-process name
				   name
				   "stern"
				   (concat "--selector=app=" deployment)
				   "--output=raw"
				   "--tail=100")
	(switch-to-buffer name)))

(provide 'arg-stern)
