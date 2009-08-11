;;(load "vendor/nxhtml/util/winsize")

;; linum
(require 'linum)
;; (global-linum-mode 1)

;; Aspell + Flyspell
(setq ispell-program-name "aspell")
(setq ispell-dictionary "german")

;; Yasnippet
(vendor 'yasnippet)
(yas/initialize)
(yas/load-directory (concat dotfiles-dir "vendor/yasnippet/snippets"))
(yas/load-directory (concat private-config-dir "/snippets"))

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

;; whitespace mode
(vendor 'whitespace)

;; org mode
(add-to-list 'auto-mode-alist '("\\.org$" . org-mode))
(setq org-log-done t)
(defun org-summary-todo (n-done n-not-done)
  "Switch entry to DONE when all subentries are done, to TODO otherwise."
  (let (org-log-done org-log-states)    ; turn off logging
    (org-todo (if (= n-not-done 0) "DONE" "TODO"))))

(add-hook 'org-after-todo-statistics-hook 'org-summary-todo)

;; nxml
(add-hook 'nxml-completion-hook 'rng-complete nil t)
(setq rng-nxml-auto-validate-flag t)
(add-to-list 'auto-mode-alist '("\\.html$" . nxml-mode))
(add-to-list 'auto-mode-alist '("\\.xml$" . nxml-mode))

;; nXhtml
(add-hook 'html-mode-hook '(lambda ()
                             (make-variable-buffer-local font-lock-function-name-face)
                             (setq font-lock-function-name-face '((t (:inherit keyword))))))

;;js2-mode
(setq js2-mirror-mode nil)

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
  (local-unset-key (kbd "M-j"))
  (local-unset-key (kbd "M-k"))
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

;;;; RSpec
(vendor 'rspec-mode)

;;;; Cucumber
(add-to-list 'load-path (concat dotfiles-dir "vendor/cucumber.el"))
(load "feature-mode")

(vendor 'magit)