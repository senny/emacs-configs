;;;; MISC
(global-hl-line-mode t)
(column-number-mode t)

;; Browse Kill Ring
(require 'browse-kill-ring)
(require 'browse-kill-ring+)

(require 'iedit)

;; Aspell + Flyspell
(setq ispell-program-name "aspell")
(setq ispell-dictionary "german")

;; ido
(ido-mode t)
(setq ido-enable-flex-matching t)
(setq ido-use-filename-at-point nil)
(vendor 'ido-hacks)
(ido-hacks-mode 1)

;; mode-compile
(autoload 'mode-compile "mode-compile"
  "Command to compile current buffer file based on the major mode" t)
(global-set-key "\C-cc" 'mode-compile)

(autoload 'mode-compile-kill "mode-compile"
  "Command to kill a compilation launched by `mode-compile'" t)
(global-set-key "\C-ck" 'mode-compile-kill)

;; Make emacs act like textmate
(vendor 'textmate)
(textmate-mode 1)
(eval-after-load 'textmate-mode
  '(progn
     (global-unset-key (kbd "M-t"))
     (global-unset-key (kbd "M-T"))
     (define-key *textmate-mode-map* (kbd "M-p") 'textmate-goto-symbol)
     (define-key *textmate-mode-map* (kbd "M-t") 'textmate-goto-file)
     (define-key *textmate-mode-map* (kbd "M-T") 'textmate-goto-symbol)))

;; whitespace mode
(vendor 'whitespace)

;; org mode
(add-to-list 'auto-mode-alist '("\\.org$" . org-mode))
(setq org-log-done t)
(defun org-summary-todo (n-done n-not-done)
  "Switch entry to DONE when all subentries are done, to TODO otherwise."
  (let (org-log-done org-log-states)    ; turn off logging
    (org-todo (if (= n-not-done 0) "DONE" "TODO"))))

(org-clock-persistence-insinuate)
(add-hook 'org-after-todo-statistics-hook 'org-summary-todo)
(setq org-agenda-clockreport-parameter-plist (quote (:link nil :maxlevel 4 :emphasize t)))
(setq org-clock-persist t)
(setq org-clock-out-when-done nil)

;; nxml
(add-hook 'nxml-completion-hook 'rng-complete nil t)
(setq rng-nxml-auto-validate-flag t)
(add-to-list 'auto-mode-alist '("\\.html$" . nxml-mode))
(add-to-list 'auto-mode-alist '("\\.xml$" . nxml-mode))
(add-to-list 'auto-mode-alist '("\\.tld$" . nxml-mode))

;; this function rebinds M-s and M-S to switch between the different windows
(defun rebind-commands ()
  (interactive)
  (local-unset-key (kbd "M-a"))
  (local-unset-key (kbd "M-s"))
  (local-unset-key (kbd "M-S"))
  (local-unset-key (kbd "M-h"))
  (local-unset-key (kbd "M-H"))
  (local-unset-key (kbd "M-0"))
  (local-unset-key (kbd "M-1"))
  (local-unset-key (kbd "M-2"))
  (local-unset-key (kbd "M-3"))
  (local-unset-key (kbd "M-i"))
  (local-unset-key (kbd "M-I"))
  (local-unset-key (kbd "M-j"))
  (local-unset-key (kbd "M-k"))
  (local-unset-key (kbd "M-K"))
  (local-unset-key (kbd "M-l"))
  (local-unset-key (kbd "M-J"))
  (local-unset-key (kbd "M-L")))

(add-hook 'after-change-major-mode-hook 'rebind-commands)
(add-hook 'sr-start-hook 'rebind-commands)
(add-hook 'comint-mode-hook 'rebind-commands)
(add-hook 'inf-ruby-mode-hook 'rebind-commands)

;;;; Flymake
(require 'flymake-cursor) ;display error-messages when the curosr moves over the line

;;;; Ediff
(setq ediff-merge-split-window-function 'split-window-horizontally)
(setq ediff-split-window-function 'split-window-horizontally)

;;;; Sunrise Commander
(require 'sunrise-commander)
(define-key sr-mode-map (kbd "<backtab>") 'sr-follow-file-other)

;;;; Cucumber
(add-to-list 'load-path (concat dotfiles-dir "vendor/cucumber.el"))
(load "feature-mode")

;;;; Better Registers
(require 'better-registers)
(better-registers t)
(better-registers-install-save-registers-hook)
(load better-registers-save-file)

;;;; Batch
(vendor 'batch-mode)

;;;; CSS
;; (defconst css-validator (concat "java"))

;; (defun flymake-css-init ()
;;   (let* ((temp-file   (flymake-init-create-temp-buffer-copy
;;                        'flymake-create-temp-inplace))
;;          (local-file  (file-relative-name
;;                        temp-file
;;                        (file-name-directory buffer-file-name))))
;;     (list css-validator (list "-jar C:\\Users\\senny\\coding\\java\\libraries\\css-validator\\css-validator.jar" "-output gnu" (concat "file:" local-file)))))

;; (push '(".+\\.css$" flymake-css-init) flymake-allowed-file-name-masks)

;; (push '("^file:\\([^:]+\\):\\([^:]+\\):\\(.*\\)" 1 2 nil 3) flymake-err-line-patterns)
