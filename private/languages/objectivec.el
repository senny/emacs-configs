;; Hooks
(defun default-objc-mode-hook ()
  (set-pairs '("(" "{" "[" "\""))
  (setq ac-sources '(ac-source-abbrev ac-source-symbols ac-source-words-in-buffer)))

(add-hook 'objc-mode-hook 'defaut-objc-mode-hook)
