;; -*- no-byte-compile: t; -*-
;;; $DOOMDIR/packages.el

(package! emacsql)
(package! emacsql-sqlite)
(package! protobuf-mode)
(package! salt-mode)
(package! vc-fossil)

(when (package! restclient)
  (package! jq-mode)
  (package! restclient-jq
    :recipe (:host github
             :repo "pashky/restclient.el"
             :files ("restclient-jq.el"))))
