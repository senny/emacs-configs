;; Hooks
(defun default-html-mode-hook ()
  (set-pairs '("<" "{" "[" "\"" "\'")))

(add-hook 'html-mode-hook 'default-html-mode-hook)
(add-hook 'nxml-mode-hook 'default-html-mode-hook)
