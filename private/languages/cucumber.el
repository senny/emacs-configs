;; Hooks
(defun default-cucumber-hook ()
  (set-pairs '("(" "\"" "\'")))

(add-hook 'feature-mode-hook 'default-cucumber-hook)
