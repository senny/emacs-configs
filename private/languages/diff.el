;; File Mappings
(add-to-list 'auto-mode-alist '("COMMIT_EDITMSG$" . diff-mode))

;; Default to unified diffs
(setq diff-switches "-u")

;;;; Ediff

(setq ediff-window-setup-function 'ediff-setup-windows-plain
      ediff-merge-split-window-function 'split-window-horizontally
      ediff-split-window-function 'split-window-horizontally)
