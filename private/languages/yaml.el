;; File Mappings
(add-to-list 'auto-mode-alist '("\\.ya?ml$" . yaml-mode))

;; Hooks
(defun default-yaml-hook ()
  (set-pairs '("(" "\"" "\'" "{")))

(add-hook 'yaml-mode-hook 'default-yaml-hook)
