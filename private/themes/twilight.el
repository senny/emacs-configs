;; Twilight Colour Theme for Emacs.
;;
;; Defines a colour scheme resembling that of the original TextMate Twilight colour theme.
;; To use add the following to your .emacs file (requires the color-theme package):
;;
;; (require 'color-theme)
;; (color-theme-initialize)
;; (load-file "~/.emacs.d/twilight-emacs/color-theme-twilight.el")
;;
;; And then (color-theme-twilight) to activate it.
;;
;; Several areas still require improvement such as recognition of code that ruby-mode doesn't
;; yet pick up (eg. parent classes), Rails/Merb keywords, or non Ruby code related areas
;; (eg. dired, HTML, etc). Please feel free to customize further and send in any improvements,
;; patches most welcome.
;;
;; MIT License Copyright (c) 2008 Marcus Crafter <crafterm@redartisan.com>
;; Credits due to the excellent TextMate Twilight theme
;;
(senny-make-theme "Twilight"
                  ((background-color . "#131313")
                   (background-mode . dark)
                   (border-color . "black")
                   (cursor-color . "#A7A7A7")
                   (foreground-color . "#F8F8F8")
                   (mouse-color . "sienna1"))
                  :base-font          (:foreground "#F8F8F8")
                  :selection          (:background "#333333")
                  :brief-highlight    (:background "khaki2" :foreground "black")
                  :durable-highlight  (:background "#000000")
                  :link               (:underline t :foreground "dark-blue")
                  :mode-line          (:background "#CCCCCC" :foreground "black")
                  :minibuffer-prompt  (:background "#141414" :foreground "cyan")
                  :code-variable      (:foreground "#7587A6")
                  :code-constant      (:foreground "#CF6A4C")
                  :code-type          (:foreground "#9B703F")
                  :code-keyword       (:foreground "#CDA869")
                  :code-string        (:foreground "#8F9D6A")
                  :code-comment       (:foreground "#5F5A60" :italic t)
                  :code-documentation (:foreground "#9B859D")
                  :code-function      (:foreground "#F8F8F8")
                  :code-meta          (:foreground "Aquamarine")
                  :code-error         (:background "#420E09")
                  :code-warning       (:background "#4A410D")
                  :code-info          (:background "#0E2231")
                  :code-addition      (:background "#253B22")
                  :code-escape-char   (:foreground "#E9C062"))
