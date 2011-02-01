;; File Mappings
(add-to-list 'auto-mode-alist '("\\.js\\(on\\)?$" . js-mode))

(defvar *jslint-executable* (or (executable-find "jsl")
                                (concat dotfiles-dir "misc/jsl-0.3.0-mac/jsl")))

(setq js-indent-level 2
      javascript-indent-level 2
      js-auto-indent-flag nil)

(eval-after-load 'mode-compile
  '(progn
     (add-to-list 'mode-compile-modes-alist '(js-mode . (senny-jslint-compile kill-compilation)))))

;; Defuns
(defun senny-js-send-buffer ()
  (interactive)
  (moz-send-region (point-min) (point-max)))

(defun senny-jslint-compile ()
  (interactive)
  (compile (format "%s -process %s" *jslint-executable* (buffer-file-name))))

(eval-after-load 'js
  '(progn
     ;; Libraries
     (require 'flymake)

     (defun flymake-jslint-init ()
       (let* ((temp-file (flymake-init-create-temp-buffer-copy
                          'flymake-create-temp-inplace))
              (local-file (file-relative-name
                           temp-file
                           (file-name-directory buffer-file-name))))
         (list *jslint-executable* (list "-process" local-file))))

     (setq flymake-allowed-file-name-masks
           (cons '(".+\\.js$"
                   flymake-jslint-init
                   flymake-simple-cleanup
                   flymake-get-real-file-name)
                 flymake-allowed-file-name-masks))

     (setq flymake-err-line-patterns
           (cons '("^Lint at line \\([[:digit:]]+\\) character \\([[:digit:]]+\\): \\(.+\\)$"
                   nil 1 2 3)
                 flymake-err-line-patterns))

     (add-hook 'js-mode-hook
               (lambda () (flymake-mode t)))))

(eval-after-load 'js
  '(progn
     (define-key js-mode-map (kbd "C-c v") 'senny-js-send-buffer)
     ;; fixes problem with pretty function font-lock
     (define-key js-mode-map (kbd ",") 'self-insert-command)
     (font-lock-add-keywords 'js-mode
                             '(("\\<\\(FIX\\|TODO\\|FIXME\\|HACK\\|REFACTOR\\):"
                                1 font-lock-warning-face t)))
     (font-lock-add-keywords
      'js-mode `(("\\(function *\\)("
                        (0 (progn (compose-region (match-beginning 1)
                                                  (match-end 1) "ƒ")
                                  nil)))))))

;; Hooks
(defun default-javascript-mode-hook ()
  (set-pairs '("(" "{" "[" "\"" "'"))
  (add-to-list 'ac-sources 'ac-source-yasnippet))

(add-hook 'js-mode-hook 'default-javascript-mode-hook)
(add-hook 'js-mode-hook 'run-coding-hook)
