(add-to-list 'auto-mode-alist '("\\.jsp$" . html-mode))
(add-to-list 'auto-mode-alist '("\\.java$" . java-mode))

;; Hooks
(defun default-java-mode-hook ()
  ;; Additional Libraries
  (require 'javadoc-help)
  (require 'java-mode-indent-annotations)

  (set-pairs '("(" "{" "[" "\"" "\'"))
  (setq c-comment-continuation-stars "* ")
  (setq c-basic-offset 2)
  (setq ac-sources '(ac-source-eclim ac-source-words-in-same-mode-buffers))
  ;; (java-mode-indent-annotations-setup)
  )

(defun default-jde-mode-hook ()
  (local-unset-key (kbd "M-j"))
  (local-unset-key (kbd "C-c C-a"))
  (global-unset-key (kbd "C-c C-a"))
  (default-java-mode-hook)
  (setq jde-complete-insert-method-signature nil)
  ;; No "final" when auto creating methods and variables.
  (setq jde-gen-final-arguments nil)
  (setq jde-gen-final-methods nil)

  ;; Don't use JDE's builtin abbrevs.
  (setq jde-enable-abbrev-mode nil))

;; Java
(add-hook 'java-mode-hook 'default-java-mode-hook )
(add-hook 'jde-mode-hook 'default-jde-mode-hook)
