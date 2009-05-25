;; start the emacs-server to use emacsclient
(setq-default tab-width 2)
(setq-default indent-tabs-mode nil)
(setq cua-highlight-region-shift-only t)

(require 'minibuffer-complete-cycle)
(setq minibuffer-complete-cycle t)
(prefer-coding-system 'utf-8)

;; Custom completion functions for C-SPC
(setq *alternate-completion-functions-alist* '((jde-mode . jde-complete-minibuf)
                                      (fundamental-mode . ispell-word)
                                      (lisp-interaction-mode . lisp-complete-symbol)
                                      (emacs-lisp-mode . lisp-complete-symbol)
                                      (text-mode . ispell-word)
                                      (nxml-mode . nxml-complete)))

(setq hippie-expand-try-functions-list
      '(yas/hippie-try-expand try-expand-dabbrev
                              try-expand-dabbrev-all-buffers
                              try-expand-dabbrev-from-kill
                              try-complete-file-name-partially
                              try-complete-file-name
                              try-complete-lisp-symbol-partially
                              try-complete-lisp-symbol
                              try-expand-whole-kill))
(vendor 'pastie)

