;; nxml
(add-hook 'nxml-completion-hook 'rng-complete nil t)
(setq rng-nxml-auto-validate-flag t)
(add-to-list 'auto-mode-alist '("\\.html$" . nxml-mode))
(add-to-list 'auto-mode-alist '("\\.xml$" . nxml-mode))
(add-to-list 'auto-mode-alist '("\\.tld$" . nxml-mode))

;; Hooks
(defun default-html-mode-hook ()
  (set-pairs '("<" "{" "[" "\"" "\'")))

(add-hook 'html-mode-hook 'default-html-mode-hook)
(add-hook 'nxml-mode-hook 'default-html-mode-hook)
