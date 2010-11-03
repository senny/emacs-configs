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

;;;; Flymake
(require 'flymake-cursor) ;;display error-messages without mouse

(require 'ibuffer)
(require 'switch-window)
