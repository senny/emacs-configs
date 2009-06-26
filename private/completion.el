;;; Require
(vendor 'auto-complete)
;; (require 'auto-complete-extension nil t) ;optional
;; (require 'auto-complete-yasnippet nil t) ;optional
(require 'auto-complete-css nil t) ;optional
;; (require 'auto-complete-ruby nil t) ;optional
;; (require 'auto-complete-semantic nil t)  ;optional
;; (require 'auto-complete-gtags nil t)     ;optional

;;; Code:

;; Generic setup.
(global-auto-complete-mode t)           ;enable global-mode
(setq ac-auto-start t)                  ;automatically start
(setq ac-dwim t)                        ;Do what i mean
(setq ac-override-local-map nil)        ;don't override local map

;; The mode that automatically startup.
(setq ac-modes
      '(emacs-lisp-mode lisp-interaction-mode lisp-mode scheme-mode
                        c-mode cc-mode c++-mode java-mode
                        perl-mode cperl-mode python-mode ruby-mode
                        ecmascript-mode javascript-mode php-mode css-mode
                        makefile-mode sh-mode fortran-mode f90-mode ada-mode
                        xml-mode sgml-mode
                        haskell-mode literate-haskell-mode
                        emms-tag-editor-mode
                        asm-mode
                        org-mode))
;; (add-to-list 'ac-trigger-commands 'org-self-insert-command) ; if you want enable auto-complete at org-mode, uncomment this line

;; The sources for common all mode.
(custom-set-variables
 '(ac-sources
   '(
     ;; ac-source-yasnippet ;this source need file `auto-complete-yasnippet.el'
     ;; ac-source-semantic    ;this source need file `auto-complete-semantic.el'
     ;; ac-source-imenu
     ac-source-abbrev
     ac-source-dabbrev
     ac-source-words-in-buffer
     ac-source-files-in-current-dir
     ac-source-filename
     )))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Lisp mode ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(dolist (hook (list
               'emacs-lisp-mode-hook
               'lisp-interaction-mode
               ))
  (add-hook hook '(lambda ()
                    (add-to-list 'ac-sources 'ac-source-symbols))))

(add-hook 'css-mode-hook '(lambda ()
                            (add-to-list 'ac-sources 'ac-source-css-keywords)))

;; (add-hook 'ruby-mode-hook '(lambda ()
;;                              (add-to-list 'ac-sources 'ac-source-ruby)))
