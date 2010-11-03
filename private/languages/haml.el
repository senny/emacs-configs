;; Hooks
(defun default-haml-mode-hook ()
  (set-pairs '("[" "{" "(" "\"" "'")))

(add-hook 'haml-mode-hook 'default-haml-mode-hook)
