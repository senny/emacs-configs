(make-variable-buffer-local 'company-backends)

(defun default-lisp-mode-hook ()
  (set-pairs '("(" "{" "[" "\""))
  (setq company-backends '(company-elisp
                           company-dabbrev-code))
  (company-mode t))

(defun default-java-mode-hook ()
  (set-pairs '("(" "{" "[" "\"" "\'"))
  (setq company-backends '(company-dabbrev-code))
  (company-mode t)
  (setq c-comment-continuation-stars "* ")
  (setq c-basic-offset 2))

;; Ruby
(add-hook 'ruby-mode-hook
          (lambda ()
            (setq company-backends '(
                                     company-dabbrev-code))
            (company-mode t)
            (set-pairs '("(" "{" "[" "\"" "\'" "|"))
            (local-set-key [return] 'ruby-reindent-then-newline-and-indent)))

;; Java
(add-hook 'java-mode-hook 'default-java-mode-hook)
(add-hook 'jde-mode-hook
          (lambda ()
            (local-unset-key (kbd "M-j"))
            (local-unset-key (kbd "C-c C-a"))
            (global-unset-key (kbd "C-c C-a"))
            (default-java-mode-hook)
            (setq jde-complete-insert-method-signature nil)
            ;; No "final" when auto creating methods and variables.
            (setq jde-gen-final-arguments nil)
            (setq jde-gen-final-methods nil)

            ;; Don't use JDE's builtin abbrevs.
            (setq jde-enable-abbrev-mode nil)))

;; Lisp
(add-hook 'lisp-mode-hook 'default-lisp-mode-hook)
(add-hook 'lisp-interaction-mode-hook 'default-lisp-mode-hook)
(add-hook 'emacs-lisp-mode-hook 'default-lisp-mode-hook)

;; CSS
(add-hook 'css-mode-hook
          (lambda ()
            (setq css-indent-offset 2)
            (set-pairs '("(" "[" "\"" "\'"))))

;; HTML
(add-hook 'nxml-mode-hook
          (lambda ()
            (set-pairs '("<" "{" "[" "\"" "\'"))
            (setq css-indent-offset 2)))

;; Org-mode
(add-hook 'org-mode-hook
          (lambda ()
            (define-key org-mode-map (kbd "C-c a") 'org-agenda)
            (define-key org-mode-map (kbd "C-c t u") 'org-clock-update-time-maybe)
            (define-key org-mode-map (kbd "C-c t g") 'org-clock-goto)
            (set-pairs '("(" "{" "[" "\""))
            (auto-fill-mode 1)))

;; Comint
(add-hook 'comint-mode-hook
          (lambda ()
            ))