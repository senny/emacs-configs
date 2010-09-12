(autoload 'espresso-mode "espresso" "Start espresso-mode" t)
(add-to-list 'auto-mode-alist '("\\.js$" . espresso-mode))
(add-to-list 'auto-mode-alist '("\\.json$" . espresso-mode))
(setq espresso-indent-level 2)

(defun senny-js-send-buffer ()
  (interactive)
  (moz-send-region (point-min) (point-max)))

(eval-after-load 'espresso
  '(progn
     (define-key espresso-mode-map (kbd "C-c v") 'senny-js-send-buffer)
     ;; fixes problem with pretty function font-lock
     (define-key espresso-mode-map (kbd ",") 'self-insert-command)
     (font-lock-add-keywords 'espresso-mode
                             '(("\\<\\(FIX\\|TODO\\|FIXME\\|HACK\\|REFACTOR\\):"
                                1 font-lock-warning-face t)))
     (font-lock-add-keywords
      'espresso-mode `(("\\(function *\\)("
                        (0 (progn (compose-region (match-beginning 1)
                                                  (match-end 1) "ƒ")
                                  nil)))))))

;; Hooks
(defun default-javascript-mode-hook ()
  (set-pairs '("(" "{" "[" "\"" "'"))
  (add-to-list 'ac-sources 'ac-source-yasnippet))

(add-hook 'javascript-mode-hook 'default-javascript-mode-hook)
(add-hook 'espresso-mode-hook 'default-javascript-mode-hook)
(add-hook 'espresso-mode-hook 'moz-minor-mode)
(add-hook 'espresso-mode-hook 'run-coding-hook)
