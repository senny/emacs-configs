;; Make emacs act like textmate
(eval-after-load 'textmate
  '(progn
     (global-unset-key (kbd "M-t"))
     (global-unset-key (kbd "M-T"))
     (define-key *textmate-mode-map* (kbd "M-<right>") 'textmate-shift-right)
     (define-key *textmate-mode-map* (kbd "M-<left>") 'textmate-shift-left)
     (define-key *textmate-mode-map* (kbd "M-p") 'textmate-goto-symbol)
     (define-key *textmate-mode-map* (kbd "M-t") 'textmate-goto-file)
     (define-key *textmate-mode-map* (kbd "M-w") 'textmate-goto-symbol)
     (define-key *textmate-mode-map* (kbd "M-T") 'textmate-goto-symbol)))

(vendor 'textmate)
(textmate-mode 1)
