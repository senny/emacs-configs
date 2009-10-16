;; (add-to-list 'load-path (expand-file-name (concat dotfiles-dir "/vendor/jde/lisp")))
;; (add-to-list 'load-path (expand-file-name (concat dotfiles-dir "/vendor/cedet/common")))
;; (load-file (expand-file-name (concat dotfiles-dir "/vendor/cedet/common/cedet.el")))
;; (add-to-list 'load-path (expand-file-name (concat dotfiles-dir "/vendor/elib")))
;; (load (expand-file-name (concat dotfiles-dir "/vendor/decompile")))

;; (defvar *jde-eclipse-flymake-mode* nil)
;; (setq defer-loading-jde nil)
;; (require 'jde)
;; (require 'decompile)

;; (define-key jde-mode-map (kbd "C-c C-v w") 'jde-wiz-get-set-methods)
;; (define-key jde-mode-map (kbd "C-c C-v g") 'jde-gen-get-set-methods)
;; (define-key jde-mode-map (kbd "C-c o") 'jde-senny-organize-imports)

(add-to-list 'auto-mode-alist '("\\.jsp$" . html-mode))
;; (add-to-list 'auto-mode-alist '("\\.java$" . java-mode))

;; (defun jde-senny-organize-imports ()
;;   (interactive)
;;   (save-excursion
;;     (jde-import-all)
;;     (jde-import-organize)
;;     (jde-import-kill-extra-imports)))

;; (defun jde-complete-ido ()
;;   "Custom method completion for JDE using ido-mode and yasnippet."
;;   (interactive)
;;   (let ((completion-list '()))
;;     (dolist (element (jde-complete-find-completion-for-pair (jde-complete-get-pair (jde-parse-java-variable-at-point) nil) nil))
;;       (add-to-list 'completion-list (cdr element)))
;;     (if completion-list
;;         (let ((choise (ido-completing-read "> " completion-list)) (method))
;;           (unless (string-match "^.*()$" choise)
;;             (setq method (replace-regexp-in-string ")" "})"(replace-regexp-in-string ", " "}, ${" (replace-regexp-in-string "(" "(${" choise)))))
;;           (delete-region (point) (re-search-backward "\\." (line-beginning-position)))
;;           (insert ".")
;;           (if method
;;               (yas/expand-snippet (point) (point) method)
;;             (insert choise)))
;;       (message "No completions at this point"))))

;; (setq jde-complete-function 'jde-complete-ido)

;; (defun jde-eclipse-flymake-mode ()
;;   (interactive)
;;   (setq *jde-eclipse-flymake-mode* (not *jde-eclipse-flymake-mode*))
;;   (if (file-exists-p (concat dotfiles-dir "vendor/jde-eclipse-compiler-server.el"))
;;       (progn
;;         (load (concat dotfiles-dir "vendor/jde-eclipse-compiler-server"))
;;         (setq jde-compiler `(("eclipse java compiler server" ,(concat dotfiles-dir "misc/bin/ecj-3.4.1.jar"))))
;;         (setq flymake-allowed-file-name-masks (quote (("\\.java\\'" jde-ecj-server-flymake-init jde-ecj-flymake-cleanup))))
;;         (let ((flymake-hook '(lambda ()
;;                                (flymake-mode 1)
;;                                )))
;;           (if *jde-eclipse-flymake-mode*
;;               (progn
;;                 (add-hook 'jde-mode-hook flymake-hook)
;;                 (flymake-mode 1)
;;                 (message "Eclipse-Flymake-Mode activated"))
;;             (progn
;;               (remove-hook 'jde-mode-hook flymake-hook)
;;               (flymake-mode 0)
;;               (message "Eclipse-Flymake-Mode deactivated")))))))

(provide 'starter-kit-java)