;; use tab to indent and complete
;; (vendor 'tabkey2)
;; (tabkey2-mode 1)
;; (setq tabkey2-completion-functions
;;       '(("autocomplete" ac-complete t)
;;         ("Hippie expand" hippie-expand t)
        ;;        ("Spell check word" flyspell-correct-word-before-point)
        ;;        ("JDE Completion" jde-complete-minibuf)
        ;;        ("Yasnippet" yas/expand (yas/expandable-at-point))
        ;;        ("Semantic Smart Completion" senator-complete-symbol senator-minor-mode)
        ;;        ("Programmable completion" pcomplete)
        ;;        ("nXML completion" nxml-complete)
        ;;        ("Complete Emacs symbol" lisp-complete-symbol)
        ;;        ("Widget complete" widget-complete)
        ;;        ("Comint Dynamic Complete" comint-dynamic-complete)
        ;;        ("PHP completion" php-complete-function)
        ;;        ("Tags completion" complete-symbol)
        ;;        ("Predictive word" complete-word-at-point predictive-mode)
        ;;        ("Predictive abbreviations" pabbrev-expand-maybe)
        ;;        ("Dynamic word expansion" dabbrev-expand nil (setq dabbrev--last-abbrev-location nil))
        ;;        ("Ispell complete word" ispell-complete-word)
        ;;        ("Anything" anything (commandp 'anything))
;;         ))

(add-to-list 'load-path (concat dotfiles-dir "/vendor/company"))
(load "company")

;; only start completio when inserting character
(setq company-begin-commands '(self-insert-command))

(dolist (hook (list
               'emacs-lisp-mode-hook
               'lisp-mode-hook
               'lisp-interaction-mode-hook
               'java-mode-hook
               'nxml-mode-hook
               'ruby-mode-hook
               'css-mode-hook
               'html-mode-hook
               'jde-mode-hook
               'javascript-mode-hook
               ))
  (add-hook hook (lambda ()
                   (company-mode 1))))

(setq company-idle-delay 0)
(setq company-backends '(company-elisp
                         company-nxml
                         company-css
                         company-eclim
                         ;; company-semantic
                         ;; (company-gtags company-etags company-dabbrev-code company-keywords)
                         ;; company-files
                         company-dabbrev))

(setq company-eclim-executable "eclim")

(define-key company-active-map (kbd "M-k") 'company-select-next)
(define-key company-active-map (kbd "M-i") 'company-select-previous)
(define-key company-mode-map (kbd "C-SPC") 'company-complete)