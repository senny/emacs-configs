;;(load "vendor/nxhtml/util/winsize")
;; use tab to indent and complete
(vendor 'tabkey2)
(tabkey2-mode 1)
(setq tabkey2-completion-functions
      '(("Hippie expand" hippie-expand t)
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
        ))

;; linum
(require 'linum)
;; (global-linum-mode 1)

;; Aspell + Flyspell
(setq ispell-program-name "aspell")
(setq ispell-dictionary "german")

;; Yasnippet
(vendor 'yasnippet)
(yas/initialize)
(yas/load-directory (concat dotfiles-dir "vendor/yasnippet/snippets"))
(yas/load-directory (concat private-config-dir "/snippets"))

;; ido
(ido-mode t)
(setq ido-enable-flex-matching t)
(vendor 'ido-hacks)
(ido-hacks-mode 1)

;; mode-compile
(autoload 'mode-compile "mode-compile"
  "Command to compile current buffer file based on the major mode" t)
(global-set-key "\C-cc" 'mode-compile)

(autoload 'mode-compile-kill "mode-compile"
  "Command to kill a compilation launched by `mode-compile'" t)
(global-set-key "\C-ck" 'mode-compile-kill)

;; Make emacs act like textmate
(vendor 'textmate)
(textmate-mode 1)

;; whitespace mode
(vendor 'whitespace)

;; org mode
(add-to-list 'auto-mode-alist '("\\.org$" . org-mode))
(setq org-log-done t)
(defun org-summary-todo (n-done n-not-done)
  "Switch entry to DONE when all subentries are done, to TODO otherwise."
  (let (org-log-done org-log-states)   ; turn off logging
    (org-todo (if (= n-not-done 0) "DONE" "TODO"))))

(add-hook 'org-after-todo-statistics-hook 'org-summary-todo)

;; nxml
(add-hook 'nxml-completion-hook 'rng-complete nil t)
(setq rng-nxml-auto-validate-flag t)
(add-to-list 'auto-mode-alist '("\\.html$" . nxml-mode))
(add-to-list 'auto-mode-alist '("\\.xml$" . nxml-mode))

;; nXhtml
(add-hook 'html-mode-hook '(lambda ()
                             (make-variable-buffer-local font-lock-function-name-face)
                             (setq font-lock-function-name-face '((t (:inherit keyword))))))

;;js2-mode
(setq js2-mirror-mode nil)

;; this function rebinds M-s and M-S to switch between the different windows
(defun rebind-window-switch-commands ()
  (local-set-key (kbd "M-s") 'move-cursor-next-pane)
  (local-set-key (kbd "M-S") 'move-cursor-previous-pane))

;; fix-bindings
(add-hook 'comint-mode-hook 'rebind-window-switch-commands)
(add-hook 'inf-ruby-mode-hook 'rebind-window-switch-commands)

;;;; Flymake
(require 'flymake-cursor) ;display error-messages when the curosr moves over the line

;;;; Cucumber
(vendor 'cucumber)
