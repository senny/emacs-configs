;; File Mappings
(add-to-list 'auto-mode-alist '("\\.js\\(on\\)?$" . espresso-mode))

(defvar *jslint-executable* (or (executable-find "jsl")
                                (concat dotfiles-dir "misc/jsl-0.3.0-mac/jsl")))

(autoload 'espresso-mode "espresso" "Start espresso-mode" t)
(setq espresso-indent-level 2
      javascript-indent-level 2)

(eval-after-load 'mode-compile
  '(progn
     (add-to-list 'mode-compile-modes-alist '(espresso-mode . (senny-jslint-compile kill-compilation)))))

;; Defuns
(defun senny-js-send-buffer ()
  (interactive)
  (moz-send-region (point-min) (point-max)))

(defun senny-jslint-compile ()
  (interactive)
  (compile (format "%s -process %s" *jslint-executable* (buffer-file-name))))

(eval-after-load 'espresso
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

     (add-hook 'espresso-mode-hook
               (lambda () (flymake-mode t)))))

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
