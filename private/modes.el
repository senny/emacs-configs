;; Browse Kill Ring
(require 'browse-kill-ring)
(require 'browse-kill-ring+)

;; Aspell + Flyspell
(setq ispell-program-name "aspell")
(setq ispell-dictionary "german")

;; ido
(when (> emacs-major-version 21)
  (ido-mode t)
  (setq ido-enable-prefix nil
        ido-enable-flex-matching t
        ido-create-new-buffer 'always
        ido-use-filename-at-point nil
        ido-max-prospects 10)
  (vendor 'ido-hacks)
  (ido-hacks-mode 1))

;; mode-compile
(autoload 'mode-compile "mode-compile"
  "Command to compile current buffer file based on the major mode" t)
(global-set-key "\C-cc" 'mode-compile)

(autoload 'mode-compile-kill "mode-compile"
  "Command to kill a compilation launched by `mode-compile'" t)

;; Make emacs act like textmate
(eval-after-load 'textmate
  '(progn
     (global-unset-key (kbd "M-t"))
     (global-unset-key (kbd "M-T"))
     (define-key *textmate-mode-map* (kbd "M-<right>") 'textmate-shift-right)
     (define-key *textmate-mode-map* (kbd "M-<left>") 'textmate-shift-left)
     (define-key *textmate-mode-map* (kbd "M-p") 'textmate-goto-symbol)
     (define-key *textmate-mode-map* (kbd "M-t") 'textmate-goto-file)
     (define-key *textmate-mode-map* (kbd "M-w") 'textmate-goto-symbol)
     (define-key *textmate-mode-map* (kbd "M-T") 'textmate-goto-symbol)))

(vendor 'textmate)
(textmate-mode 1)

;; nxml
(add-hook 'nxml-completion-hook 'rng-complete nil t)
(setq rng-nxml-auto-validate-flag t)
(add-to-list 'auto-mode-alist '("\\.html$" . nxml-mode))
(add-to-list 'auto-mode-alist '("\\.xml$" . nxml-mode))
(add-to-list 'auto-mode-alist '("\\.tld$" . nxml-mode))

(defun rebind-commands ()
  (interactive)
  (local-unset-key (kbd "C-c C-k"))
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
  (local-unset-key (kbd "M-o"))
  (local-unset-key (kbd "M-u"))
  (local-unset-key (kbd "M-J"))
  (local-unset-key (kbd "M-L")))

(add-hook 'after-change-major-mode-hook 'rebind-commands)
(add-hook 'org-mode-hook 'rebind-commands)
(add-hook 'comint-mode-hook 'rebind-commands)
(add-hook 'inf-ruby-mode-hook 'rebind-commands)
(add-hook 'erlang-mode-hook 'rebind-commands)
(add-hook 'diff-mode 'rebind-commands)

;;;; Flymake
(require 'flymake-cursor) ;;display error-messages without mouse

;;;; Ediff
(setq ediff-merge-split-window-function 'split-window-horizontally)
(setq ediff-split-window-function 'split-window-horizontally)

(require 'ibuffer)
(require 'switch-window)
