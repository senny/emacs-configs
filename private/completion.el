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
;; ))

(set-default 'senny-intellisense-completion-function 'ac-start)

(defun intelisense-complete ()
  (interactive)
  (if senny-intellisense-completion-function
      (funcall senny-intellisense-completion-function)
    (message "no completion function defined!")))

(defun indent-or-complete ()
  (interactive)
  (if (looking-at "\\_>")
      (company-complete-common)
    (indent-according-to-mode)))

(add-to-list 'load-path (concat dotfiles-dir "/vendor/company"))
(load "company")

;; only start completio when inserting character
(setq company-begin-commands '(self-insert-command))
(setq company-idle-delay nil)
(setq company-show-numbers nil)
(setq company-backends '(company-elisp
                         ;; company-nxml
                         ;; company-css
                         ;; company-eclim
                         ;; company-dabbrev-code
                         ;; company-semantic
                         ;; (company-gtags company-etags company-dabbrev-code company-keywords)
                         company-files
                         company-dabbrev
                         ))

(setq company-eclim-executable "eclim")

(define-key company-active-map (kbd "M-k") 'company-select-next)
(define-key company-active-map (kbd "M-i") 'company-select-previous)
(define-key company-active-map (kbd "TAB") 'company-complete)

;;;; auto-complete
(when (vendor 'auto-complete)
  ;;(require 'auto-complete-yasnippet)
  ;;(require 'auto-complete-ruby)
  (require 'auto-complete-css)
  (require 'ac-dabbrev)

  (define-key ac-complete-mode-map "\t" 'ac-expand)
  (define-key ac-complete-mode-map (kbd "RET") 'ac-complete)
  (define-key ac-complete-mode-map "\M-k" 'ac-next)
  (define-key ac-complete-mode-map "\M-i" 'ac-previous)
  (setq ac-auto-start nil)
  (setq ac-dwim t)
  (set-default 'ac-sources '(ac-source-words-in-buffer ac-source-dabbrev)))
