(:name senny-auto-complete
       :features (auto-complete auto-complete-config)
       :type git
       :url "http://github.com/m2ym/auto-complete.git"
       :load-path "."
       :after (lambda ()
                (require 'auto-complete)
                (add-to-list 'ac-dictionary-directories (expand-file-name "dict" pdir))
                (require 'auto-complete-config)
                (ac-config-default))

       ;; customization
       :after (lambda ()
                (ac-config-default)
                (set-default 'senny-completion-function 'auto-complete)
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
                                 css-mode
                                 sql-interactive-mode))
                (define-key ac-complete-mode-map (kbd "M-k") 'ac-next)
                (define-key ac-complete-mode-map (kbd "M-i") 'ac-previous)
                (define-key ac-complete-mode-map (kbd "RET") 'ac-complete)))
