(set-default 'senny-completion-function 'yas/expand)

;;;; Yasnippet
(setq yas/root-directory (concat private-config-dir "/snippets"))
(setq yas/prompt-functions '(yas/dropdown-prompt yas/ido-prompt))
(yas/load-directory yas/root-directory)
(set-default 'yas/fallback-behavior '(apply auto-complete))
;; (yas/initialize)

;;;; auto-complete
(vendor 'auto-complete)
(add-to-list 'ac-dictionary-directories
             (concat vendor-dir "/auto-complete/dict"))
(require 'auto-complete-config)
(ac-config-default)
(setq ac-auto-start nil
      ac-modes '(erlang-mode
                 espresso-mode
                 js2-mode
                 sql-mode
                 ruby-mode
                 haml-mode
                 sass-mode
                 css-mode
                 lisp-interaction-mode
                 emacs-lisp-mode
                 css-mode))

(define-key ac-complete-mode-map (kbd "M-k") 'ac-next)
(define-key ac-complete-mode-map (kbd "M-i") 'ac-previous)
(define-key ac-complete-mode-map (kbd "RET") 'ac-complete)
