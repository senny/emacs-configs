(vendor 'company)
(global-company-mode t)

(setq company-idle-delay 0)
(setq company-begin-commands '(self-insert-command))
(define-key company-active-map (kbd "M-k") 'company-select-next)
(define-key company-active-map (kbd "M-i") 'company-select-previous)

(setq company-backends '(company-elisp
                         company-nxml
                         company-css
                         company-eclim
                         company-semantic
                         company-oddmuse
                         company-files
                         company-dabbrev
                         (company-gtags company-etags company-dabbrev-code company-keywords)))

