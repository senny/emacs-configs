(make-variable-buffer-local 'senny-completion-function)
(make-variable-buffer-local 'senny-intellisense-completion-function)

(defun default-lisp-mode-hook ()
  (set-pairs '("(" "{" "[" "\""))
  (setq ac-sources '(ac-source-symbols ac-source-emacs-lisp-features))
  (setq ac-auto-start t)
  (auto-complete-mode t)
  )

(defun default-java-mode-hook ()
  (set-pairs '("(" "{" "[" "\"" "\'"))
  ;; (setq company-backends '(company-emacs-eclim))
  (setq senny-completion-function 'ac-complete)
  (setq senny-intellisense-completion-function 'eclim-complete)
  ;; (company-mode t)
  (setq c-comment-continuation-stars "* ")
  (setq c-basic-offset 2)
  (setq ac-sources '(ac-source-eclim ac-source-words-in-same-mode-buffers))
  (yas/minor-mode t)
  ;; (java-mode-indent-annotations-setup)
  )

(defun default-css-mode-hook ()
  (set-pairs '("(" "[" "\"" "\'"))
  (setq senny-completion-function 'auto-complete)
  ;; (setq ac-sources '(ac-source-dictionary))
  ;; (setq ac-auto-start t)
  ;; (auto-complete-mode t)

  (setq css-indent-level 2)
  (setq css-indent-offset 2))

(defun default-javascript-mode-hook ()
  (set-pairs '("(" "{" "["))
  (add-to-list 'ac-sources 'ac-source-yasnippet))

(defun default-ruby-mode-hook ()
  (set-pairs '("(" "{" "[" "\"" "\'" "|"))

  (setq ac-sources '(ac-source-words-in-same-mode-buffers ac-source-yasnippet))
  (setq ac-auto-start t)
  (auto-complete-mode t)

  (make-local-variable 'ac-ignores)
  (add-to-list 'ac-ignores "end")

  (local-set-key (kbd "TAB") 'senny-indent-or-complete)
  (local-set-key [return] 'ruby-reindent-then-newline-and-indent))

(defun default-html-mode-hook ()
  (set-pairs '("<" "{" "[" "\"" "\'"))
  (setq company-backends '(company-dabbrev)))

(defun default-org-mode-hook ()
  (set-pairs '("(" "{" "[" "\""))
  (define-key org-mode-map (kbd "C-c a") 'org-agenda)
  (define-key org-mode-map (kbd "C-c t u") 'org-clock-update-time-maybe)
  (define-key org-mode-map (kbd "C-c t g") 'org-clock-goto)
  (auto-fill-mode 1)
  ;; (setq ac-sources '(ac-source-org))
  (auto-complete-mode t))

(defun default-textile-mode-hook ()
  (set-pairs '("[" "{")))

(defun default-sql-mode-hook ()
  (setq tab-width 4)
  (sql-set-product 'oracle)
  (setq ac-sources '(ac-source-words-in-same-mode-buffers ac-source-yasnippet))
  (auto-complete-mode t)
  (setq ac-auto-start t))

;; Objective C
(add-hook 'objc-mode-hook
          (lambda ()
            (set-pairs '("(" "{" "[" "\""))
            (setq company-backends '(
                                     company-dabbrev-code))
            ;; (company-mode t)
            ;; (setq ac-omni-completion-sources '(("\\.\\=" ac-source-rcodetools)))
            (auto-complete-mode t)
            (setq ac-sources '(ac-source-abbrev ac-source-symbols ac-source-words-in-buffer))) )

;; Java
(add-hook 'java-mode-hook 'default-java-mode-hook )
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
            (setq jde-enable-abbrev-mode nil)) )

;; Ruby
(add-hook 'ruby-mode-hook 'default-ruby-mode-hook)

;; Lisp
(add-hook 'lisp-mode-hook 'default-lisp-mode-hook )
(add-hook 'lisp-interaction-mode-hook 'default-lisp-mode-hook )
(add-hook 'emacs-lisp-mode-hook 'default-lisp-mode-hook )

;; CSS
(add-hook 'css-mode-hook 'default-css-mode-hook)

;; JavaScript
(add-hook 'javascript-mode-hook 'default-javascript-mode-hook)
(add-hook 'espresso-mode-hook 'default-javascript-mode-hook)

;; HTML
(add-hook 'html-mode-hook 'default-html-mode-hook )
(add-hook 'nxml-mode-hook
          (lambda ()
            (default-html-mode-hook)) )

;; Org-mode
(add-hook 'org-mode-hook 'default-org-mode-hook)

;; Comint
(add-hook 'comint-mode-hook
          (lambda ()
            ))

(add-hook 'textile-mode-hook 'default-textile-mode-hook)

(add-hook 'sql-mode-hook 'default-sql-mode-hook)
