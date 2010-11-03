;; Hooks

(defun default-css-mode-hook ()
  (set-pairs '("(" "[" "\"" "\'"))
  ;; (setq ac-sources '(ac-source-dictionary))
  (setq css-indent-level 2)
  (setq css-indent-offset 2))

(add-hook 'css-mode-hook 'default-css-mode-hook)
