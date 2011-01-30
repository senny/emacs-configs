(:name senny-textmate
       :type git
       :url "https://github.com/defunkt/textmate.el.git"
       :features textmate

       ;; customization
       :after (lambda ()
                (textmate-mode t)
                (global-unset-key (kbd "M-t"))
                (global-unset-key (kbd "M-T"))
                (define-key *textmate-mode-map* (kbd "M-<right>") 'textmate-shift-right)
                (define-key *textmate-mode-map* (kbd "M-<left>") 'textmate-shift-left)
                (define-key *textmate-mode-map* (kbd "M-<up>") 'move-text-up)
                (define-key *textmate-mode-map* (kbd "M-<down>") 'move-text-down)
                (define-key *textmate-mode-map* (kbd "M-p") 'textmate-goto-symbol)
                (define-key *textmate-mode-map* (kbd "M-t") 'textmate-goto-file)
                (define-key *textmate-mode-map* (kbd "M-w") 'textmate-goto-symbol)
                (define-key *textmate-mode-map* (kbd "M-T") 'textmate-goto-symbol)
                ))
