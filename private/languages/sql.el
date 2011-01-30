(defun default-sql-mode-hook ()
  (setq tab-width 4)
  (set-pairs '("(" "{" "[" "\"" "\'"))
  (sql-set-product 'postgres)
  (setq ac-sources '(ac-source-words-in-same-mode-buffers ac-source-yasnippet)))

(eval-after-load 'sql
  '(progn
     (define-key sql-mode-map (kbd "C-c M-r") 'sql-send-region)
     (define-key sql-mode-map (kbd "C-c v") 'sql-send-buffer)
     ))

(add-hook 'sql-mode-hook 'default-sql-mode-hook)
