;;; init.el -*- lexical-binding: t; -*-

(doom! :input
       ;;bidi              ; (tfel ot) thgir etirw uoy gnipleh
       ;;chinese
       ;;japanese
       ;;layout            ; auie,ctsrnm is the superior home row

       :completion
       (company +childframe)   ; the ultimate code completion backend
       (vertico +icons)        ; the search engine of the future

       :ui
       doom                    ; what makes DOOM look the way it does
       ;;doom-dashboard          ; a nifty splash screen for Emacs
       hl-todo                 ; highlight TODO/FIXME/NOTE/DEPRECATED/HACK/REVIEW
       (ligatures +iosevka)    ; ligatures and symbols to make your code pretty again
       modeline                ; snazzy, Atom-inspired modeline, plus API
       ophints                 ; highlight the region an operation acts on
       (popup +all
              +defaults)       ; tame sudden yet inevitable temporary windows
       neotree                 ; a project drawer, like NERDTree for vim
       ;; (treemacs +lsp)      ; a project drawer, like neotree but cooler
       unicode                 ; extended unicode support for various languages
       workspaces              ; tab emulation, persistence & separate workspaces

       :editor
       (evil +everywhere)      ; come to the dark side, we have cookies
       file-templates          ; auto-snippets for empty files
       fold                    ; (nigh) universal code folding
       (format +onsave)        ; automated prettiness
       snippets                ; my elves. They type so I don't have to

       :emacs
       (dired +ranger +icons)   ; making dired pretty [functional]
       electric                ; smarter, keyword-based electric-indent
       (ibuffer +icons)        ; interactive buffer management
       undo                    ; persistent, smarter undo for your inevitable mistakes
       vc                      ; version-control and Emacs, sitting in a tree

       :term
       eshell                  ; the elisp shell that works everywhere
       vterm                   ; the best terminal emulation in Emacs

       :checkers
       syntax                  ; tasing you for every semicolon you forget

       :tools
       (debugger +lsp)        ; FIXME stepping through code, to help you add bugs
       ;;direnv
       (eval +overlay)        ; run code, run (also, repls)
       lookup                 ; navigate your code and its documentation
       lsp                    ; M-x vscode
       magit                  ; a git porcelain for Emacs
       ;;make                 ; run make tasks from Emacs
       ansible                ;

       :os
       (:if IS-MAC macos)     ; improve compatibility with macOS
       tty                    ; improve the terminal Emacs experience

       :lang
       data                   ; config/data formats
       emacs-lisp             ; drown in parentheses
       (go +lsp)              ; the hipster dialect
       (json +lsp)            ; At least it ain't XML
       (lua +lsp)             ; one-based indices? one-based indices
       markdown               ; writing docs for people to ignore
       (python +lsp +pyright) ; beautiful is better than ugly
       rest                   ; Emacs as a REST client
       (sh +lsp +fish)        ; she sells {ba,z,fi}sh shells on the C xor
       (rest +jq)             ; Emacs as a REST client
       (yaml +lsp)            ; JSON, but readable
       (org +journal)         ; organize your plain life in plain text
       (scheme +guile)        ; a fully conniving family of lisps

       :email
       ;;(mu4e +org +gmail)
       ;;notmuch
       ;;(wanderlust +gmail)

       :app
       ;;calendar
       ;;emms
       ;;everywhere        ; *leave* Emacs!? You must be joking
       ;;irc               ; how neckbeards socialize
       ;;(rss +org)        ; emacs as an RSS reader
       ;;twitter           ; twitter client https://twitter.com/vnought

       :config
       (default +bindings +smartparens))
