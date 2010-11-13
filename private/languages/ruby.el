;; File Mappings
(add-to-list 'auto-mode-alist '("\\.rb$" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.rake$" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.thor$" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.gemspec$" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.ru$" . ruby-mode))
(add-to-list 'auto-mode-alist '("Rakefile$" . ruby-mode))
(add-to-list 'auto-mode-alist '("Gemfile$" . ruby-mode))
(add-to-list 'auto-mode-alist '("Capfile$" . ruby-mode))
(add-to-list 'auto-mode-alist '("Vagrantfile$" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.js.rjs$" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.xml.builder$" . ruby-mode))

;; We never want to edit Rubinius bytecode
(add-to-list 'completion-ignored-extensions ".rbc")

;;;; Defuns
(defun senny-ruby-compilation-this-buffer ()
  (interactive)
  (save-buffer)
  (let ((origin (current-buffer)))
    (ruby-compilation-this-buffer)
    (pop-to-buffer origin)))

(defun senny-open-spec-other-buffer ()
  (interactive)
  (when (featurep 'rspec-mode)
    (let ((source-buffer (current-buffer))
          (other-buffer (progn
                          (rspec-toggle-spec-and-target)
                          (current-buffer))))
      (switch-to-buffer source-buffer)
      (pop-to-buffer other-buffer))))

(defun senny-ruby-eval-buffer ()
  (interactive)
  (ruby-send-region-and-go (point-min) (point-max)))

(defun ruby-interpolate ()
  "In a double quoted string, interpolate."
  (interactive)
  (insert "#")
  (when (and
         (looking-back "\".*")
         (looking-at ".*\""))
    (insert "{}")
    (backward-char 1)))

;;;; Flymake
(eval-after-load 'ruby-mode
  '(progn
     ;; Libraries
     (require 'flymake)

     ;; Invoke ruby with '-c' to get syntax checking
     (defun flymake-ruby-init ()
       (let* ((temp-file (flymake-init-create-temp-buffer-copy
                          'flymake-create-temp-inplace))
              (local-file (file-relative-name
                           temp-file
                           (file-name-directory buffer-file-name))))
         (list "ruby" (list "-c" local-file))))

     (push '(".+\\.rb$" flymake-ruby-init) flymake-allowed-file-name-masks)
     (push '("Rakefile$" flymake-ruby-init) flymake-allowed-file-name-masks)

     (push '("^\\(.*\\):\\([0-9]+\\): \\(.*\\)$" 1 2 nil 3)
           flymake-err-line-patterns)

     (add-hook 'ruby-mode-hook
               (lambda ()
                 (when (and buffer-file-name
                            (file-writable-p
                             (file-name-directory buffer-file-name))
                            (file-writable-p buffer-file-name)
                            (if (fboundp 'tramp-list-remote-buffers)
                                (not (subsetp
                                      (list (current-buffer))
                                      (tramp-list-remote-buffers)))
                              t))
                   (local-set-key (kbd "C-c d")
                                  'flymake-display-err-menu-for-current-line)
                   (flymake-mode t))))))

(eval-after-load 'ruby-mode
  '(progn
     ;; work around possible elpa bug
     (ignore-errors (require 'ruby-compilation))
     (setq ruby-use-encoding-map nil)
     (add-hook 'ruby-mode-hook 'inf-ruby-keys)

     ;;;; Additional Libraries
     (vendor 'rspec-mode)
     (vendor 'rhtml-mode)
     (vendor 'rvm)

     ;; active the default ruby configured with rvm
     (when (fboundp 'rvm-use-default)
       (rvm-use-default))

     ;;;; Bindings
     (define-key ruby-mode-map (kbd "C-c v") 'senny-ruby-eval-buffer)
     (define-key ruby-mode-map (kbd "C-M-r") 'senny-ruby-compilation-this-buffer)
     (define-key ruby-mode-map (kbd "C-c C-r g") 'rvm-open-gem)
     (define-key ruby-mode-map (kbd "RET") 'reindent-then-newline-and-indent)
     (define-key ruby-mode-map (kbd "C-M-h") 'backward-kill-word)
     (define-key ruby-mode-map (kbd "C-c l") "lambda")
     (define-key ruby-mode-map (kbd "#") 'ruby-interpolate)

     ;; fix syntax highlighting for Cucumber Step Definition regexps
     (add-to-list 'ruby-font-lock-syntactic-keywords
                  '("\\(\\(\\)\\(\\)\\|Given\\|When\\|Then\\)\\s *\\(/\\)[^/\n\\\\]*\\(\\\\.[^/\n\\\\]*\\)*\\(/\\)"
                    (4 (7 . ?/))
                    (6 (7 . ?/))))
     ))

;; Clear the compilation buffer between test runs.
(eval-after-load 'ruby-compilation
  '(progn
     (defadvice ruby-do-run-w/compilation (before kill-buffer (name cmdlist))
       (let ((comp-buffer-name (format "*%s*" name)))
         (when (get-buffer comp-buffer-name)
           (with-current-buffer comp-buffer-name
             (delete-region (point-min) (point-max))))))
     (ad-activate 'ruby-do-run-w/compilation)))

(eval-after-load 'rspec-mode
  '(progn
     (define-key ruby-mode-map (kbd "C-c , o") 'senny-open-spec-other-buffer)
     (define-key ruby-mode-map (kbd "C-c , ,") 'senny-open-spec-other-buffer)))


;; Rinari (Minor Mode for Ruby On Rails)
(setq rinari-major-modes
      (list 'mumamo-after-change-major-mode-hook 'dired-mode-hook 'ruby-mode-hook
            'css-mode-hook 'yaml-mode-hook 'javascript-mode-hook))

;; Hooks
(defun default-ruby-mode-hook ()
  (ruby-electric-mode t)
  (set-pairs '("(" "{" "[" "\"" "\'" "|"))

  (setq ac-sources '(ac-source-words-in-same-mode-buffers ac-source-yasnippet))

  (make-local-variable 'ac-ignores)
  (make-local-variable 'ac-auto-start)
  (add-to-list 'ac-ignores "end")
  (setq ac-auto-start nil)

  (local-set-key (kbd "TAB") 'yas/expand)
  (local-set-key [return] 'ruby-reindent-then-newline-and-indent))

(add-hook 'ruby-mode-hook 'run-coding-hook)
(add-hook 'ruby-mode-hook 'default-ruby-mode-hook)

;; TODO Temporary addition
(defun ruby-reindent-then-newline-and-indent ()
  (interactive "*")
  (newline)
  (save-excursion
    (end-of-line 0)
    (indent-according-to-mode)
    (delete-region (point) (progn (skip-chars-backward " \t") (point))))
  (indent-according-to-mode))
