(make-variable-buffer-local 'senny-completion-function)

(defun default-lisp-mode-hook ()
  (set-pairs '("(" "{" "[" "\""))
  (setq ac-sources '(ac-source-symbols ac-source-emacs-lisp-features ac-source-dictionary))
  (auto-complete-mode t)
  )

(defun default-java-mode-hook ()
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

(defun default-css-mode-hook ()
  (set-pairs '("(" "[" "\"" "\'"))
  ;; (setq ac-sources '(ac-source-dictionary))
  (setq css-indent-level 2)
  (setq css-indent-offset 2))

(defun default-javascript-mode-hook ()
  (set-pairs '("(" "{" "[" "\"" "'"))
  (add-to-list 'ac-sources 'ac-source-yasnippet))

(defun default-ruby-mode-hook ()
  (set-pairs '("(" "{" "[" "\"" "\'" "|"))

  (setq ac-sources '(ac-source-words-in-same-mode-buffers ac-source-yasnippet))
  (auto-complete-mode t)

  (make-local-variable 'ac-ignores)
  (make-local-variable 'ac-auto-start)
  (add-to-list 'ac-ignores "end")
  (setq ac-auto-start nil)

  (local-set-key (kbd "TAB") 'senny-indent-or-complete)
  (local-set-key [return] 'ruby-reindent-then-newline-and-indent))

(defun default-html-mode-hook ()
  (set-pairs '("<" "{" "[" "\"" "\'")))

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
  (sql-set-product 'mysql)
  (setq ac-sources '(ac-source-words-in-same-mode-buffers ac-source-yasnippet))
  (auto-complete-mode t))

(defun default-haml-mode-hook ()
  (set-pairs '("[" "{" "(" "\"" "'")))

(defun default-objc-mode-hook ()
  (set-pairs '("(" "{" "[" "\""))
  (auto-complete-mode t)
  (setq ac-sources '(ac-source-abbrev ac-source-symbols ac-source-words-in-buffer)))

;; (defun default-erlang-mode-hook ()
;;   (setq ac-sources '(ac-source-dictionary ac-source-words-in-same-mode-buffers ac-source-yasnippet)))

;; Objective C
(add-hook 'objc-mode-hook 'defaut-objc-mode-hook)

;; Java
(add-hook 'java-mode-hook 'default-java-mode-hook )
(add-hook 'jde-mode-hook 'default-jde-mode-hook)

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
(add-hook 'nxml-mode-hook 'default-html-mode-hook)

;; Org-mode
(add-hook 'org-mode-hook 'default-org-mode-hook)

(add-hook 'textile-mode-hook 'default-textile-mode-hook)

(add-hook 'sql-mode-hook 'default-sql-mode-hook)

(add-hook 'haml-mode-hook 'default-haml-mode-hook)
;; (add-hook 'erlang-mode-hook 'default-erlang-mode-hook)
