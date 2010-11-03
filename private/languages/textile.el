;; Hooks
(defun default-textile-mode-hook ()
  (set-pairs '("[" "{")))

(add-hook 'textile-mode-hook 'default-textile-mode-hook)
