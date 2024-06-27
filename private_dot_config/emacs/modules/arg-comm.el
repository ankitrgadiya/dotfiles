;;; arg-comm.el -- Social & Communication Configurations

;; `gnus' is the built-in news and mail reader in the Emacs.
(use-package gnus
  :defer t
  :config

  ;; Enable GNUS Topic Mode for displaying sections in the Groups buffer.
  (add-hook 'gnus-group-mode-hook #'gnus-topic-mode)
  (setq gnus-select-method '(nnml "")
        gnus-secondary-select-methods '((nntp "news.gmane.io"))

		;; Friendlier date format.
        gnus-user-date-format-alist '(((gnus-seconds-today) . "Today at %R")
                                      ((+ (* 60 60 24) (gnus-seconds-today)) . "Yesterday, %R")
                                      (t . "%Y-%m-%d %R"))

		;; GNUS can use Dribble file to auto-save changes. This can prevent
		;; unwanted data loses.
        gnus-use-dribble-file t
        gnus-always-read-dribble-file t

		;; GNUS Summary buffer configurations.
        gnus-summary-make-false-root 'adopt
        gnus-summary-thread-gathering-function 'gnus-gather-threads-by-subject
        gnus-summary-gather-subject-limit 'fuzzy
        gnus-thread-hide-subtree nil
        gnus-thread-ignore-subject nil

		;; GNUS Summary buffer formatting.
        gnus-summary-to-prefix "To: "
        gnus-summary-line-format "%U%R %-18,18&user-date; %4L:%-25,25f %B%S\n"
        gnus-summary-mode-line-format "[%U] %p"
        gnus-sum-thread-tree-root ""
        gnus-sum-thread-tree-false-root "\\->"))

;; `debbugs' is the GNU + Debian Bug Tracker interface built on top of GNUS.
(use-package debbugs
  :ensure t
  :defer t)
