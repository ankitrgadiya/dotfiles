;;; init.el -*- lexical-binding: t; -*-

(doom! :completion
       (company    +childframe)      ; the ultimate code completion backend
       (vertico    +icons)           ; the search engine of the future

       :ui
       doom                          ; what makes DOOM look the way it does
       hl-todo                       ; highlight TODO/FIXME/NOTE/DEPRECATED/HACK/REVIEW
       (ligatures  +iosevka)         ; ligatures and symbols to make your code pretty again
       (modeline   +light)           ; snazzy, Atom-inspired modeline, plus API
       ophints                       ; highlight the region an operation acts on
       (popup      +all
                   +defaults)        ; tame sudden yet inevitable temporary windows
       neotree                       ; a project drawer, like NERDTree for vim
       unicode                       ; extended unicode support for various languages
       workspaces                    ; tab emulation, persistence & separate workspaces
       (emoji      +github
                   +unicode)         ; 🙂

       :editor
       (evil       +everywhere)      ; come to the dark side, we have cookies
       file-templates                ; auto-snippets for empty files
       fold                          ; (nigh) universal code folding
       (format     +onsave)          ; automated prettiness
       snippets                      ; my elves. They type so I don't have to

       :emacs
       (dired      +ranger
                   +icons)           ; making dired pretty [functional]
       electric                      ; smarter, keyword-based electric-indent
       (ibuffer    +icons)           ; interactive buffer management
       undo                          ; persistent, smarter undo for your inevitable mistakes
       vc                            ; version-control and Emacs, sitting in a tree

       :term
       eshell                        ; the elisp shell that works everywhere
       vterm                         ; the best terminal emulation in Emacs

       :checkers
       syntax                        ; tasing you for every semicolon you forget

       :tools
       (eval       +overlay)         ; run code, run (also, repls)
       lookup                        ; navigate your code and its documentation
       (lsp        +eglot)           ; M-x vscode
       magit                         ; a git porcelain for Emacs
       make                          ; run make tasks from Emacs
       ansible                       ;
       tree-sitter                   ; syntax and parsing, sitting in a tree...

       :os
       (:if IS-MAC macos)            ; improve compatibility with macOS
       tty                           ; improve the terminal Emacs experience

       :lang
       data                          ; config/data formats
       emacs-lisp                    ; drown in parentheses
       common-lisp                   ; if you've seen one lisp, you've seen them all
       (scheme     +guile)           ; a fully conniving family of lisps
       (cc         +lsp
                   +tree-sitter)     ; C > C++ == 1
       (go         +lsp
                   +tree-sitter)     ; the hipster dialect
       (json       +tree-sitter)     ; At least it ain't XML
       lua                           ; one-based indices? one-based indices
       (python     +pyright
                   +tree-sitter)     ; beautiful is better than ugly
       (rest       +jq)              ; Emacs as a REST client
       (sh         +fish
                   +tree-sitter)     ; she sells {ba,z,fi}sh shells on the C xor
       yaml                          ; JSON, but readable
       (javascript +lsp
                   +tree-sitter)     ; all(hope(abandon(ye(who(enter(here))))))
       markdown                      ; writing docs for people to ignore

       :config
       (default    +bindings
                   +smartparens))
