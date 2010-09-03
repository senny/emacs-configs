(set-default 'senny-completion-function 'auto-complete)

;;;; Yasnippet
(vendor 'yasnippet)
(setq yas/root-directory (concat private-config-dir "/snippets"))
(setq yas/prompt-functions '(yas/dropdown-prompt yas/ido-prompt))
(yas/load-directory yas/root-directory)
(setq yas/fallback-behavior 'call-other-command)
(yas/initialize)

;;;; auto-complete
(vendor 'auto-complete)
(add-to-list 'ac-dictionary-directories (concat vendor-dir "/auto-complete/dict"))
(require 'auto-complete-config)
(ac-config-default)
(setq ac-auto-start nil)
(add-to-list 'ac-modes 'erlang-mode)
(add-to-list 'ac-modes 'espresso-mode)

(define-key ac-complete-mode-map (kbd "M-k") 'ac-next)
(define-key ac-complete-mode-map (kbd "M-i") 'ac-previous)


(provide 'senny-completion)
