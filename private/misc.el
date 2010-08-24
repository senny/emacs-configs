(setq-default tab-width 2)
(setq-default indent-tabs-mode nil)
(setq cua-highlight-region-shift-only t)

(require 'minibuffer-complete-cycle)
(setq minibuffer-complete-cycle t)
(prefer-coding-system 'utf-8)

(vendor 'pastie)

;; stolen from http://github.com/febuiles/dotemacs/tree/master/temp_files.el
(defvar user-temporary-file-directory
  (concat temporary-file-directory user-login-name "/"))
(make-directory user-temporary-file-directory t)
(setq backup-by-copying t)
(setq backup-directory-alist
      `(("." . ,user-temporary-file-directory)
        (,tramp-file-name-regexp nil)))
(setq auto-save-list-file-prefix
      (concat user-temporary-file-directory ".auto-saves-"))
(setq auto-save-file-name-transforms
      `((".*" ,user-temporary-file-directory t)))

(add-hook 'before-save-hook 'delete-trailing-whitespace)
