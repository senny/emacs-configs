(make-variable-buffer-local 'senny-completion-function)

(defun default-css-mode-hook ()
  (set-pairs '("(" "[" "\"" "\'"))
  ;; (setq ac-sources '(ac-source-dictionary))
  (setq css-indent-level 2)
  (setq css-indent-offset 2))

(defun default-html-mode-hook ()
  (set-pairs '("<" "{" "[" "\"" "\'")))

(defun default-org-mode-hook ()
  (set-pairs '("(" "{" "[" "\""))
  (define-key org-mode-map (kbd "C-c a") 'org-agenda)
  (define-key org-mode-map (kbd "C-c t u") 'org-clock-update-time-maybe)
  (define-key org-mode-map (kbd "C-c t g") 'org-clock-goto)
  (auto-fill-mode 1)
)

(defun default-textile-mode-hook ()
  (set-pairs '("[" "{")))

(defun default-haml-mode-hook ()
  (set-pairs '("[" "{" "(" "\"" "'")))

(defun default-objc-mode-hook ()
  (set-pairs '("(" "{" "[" "\""))
  (setq ac-sources '(ac-source-abbrev ac-source-symbols ac-source-words-in-buffer)))

;; Objective C
(add-hook 'objc-mode-hook 'defaut-objc-mode-hook)

;; CSS
(add-hook 'css-mode-hook 'default-css-mode-hook)

;; HTML
(add-hook 'html-mode-hook 'default-html-mode-hook )
(add-hook 'nxml-mode-hook 'default-html-mode-hook)

(add-hook 'org-mode-hook 'default-org-mode-hook)

(add-hook 'textile-mode-hook 'default-textile-mode-hook)

(add-hook 'haml-mode-hook 'default-haml-mode-hook)
