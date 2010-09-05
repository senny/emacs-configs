(defun default-sql-mode-hook ()
  (setq tab-width 4)
  (sql-set-product 'mysql)
  (setq ac-sources '(ac-source-words-in-same-mode-buffers ac-source-yasnippet))
  (auto-complete-mode t))

(add-hook 'sql-mode-hook 'default-sql-mode-hook)
