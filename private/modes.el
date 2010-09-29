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
        ido-use-filename-at-point 'guess
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

;; org mode
(add-to-list 'auto-mode-alist '("\\.org$" . org-mode))
(setq org-log-done t)
(defun org-summary-todo (n-done n-not-done)
  "Switch entry to DONE when all subentries are done, to TODO otherwise."
  (let (org-log-done org-log-states)    ; turn off logging
    (org-todo (if (= n-not-done 0) "DONE" "TODO"))))

(org-clock-persistence-insinuate)
(add-hook 'org-after-todo-statistics-hook 'org-summary-todo)
(setq org-agenda-clockreport-parameter-plist
      (quote (:link nil :maxlevel 4 :emphasize t)))
(setq org-clock-persist t)
(setq org-clock-out-when-done nil)

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
  (local-unset-key (kbd "M-J"))
  (local-unset-key (kbd "M-L")))

(add-hook 'after-change-major-mode-hook 'rebind-commands)
(add-hook 'org-mode-hook 'rebind-commands)
(add-hook 'comint-mode-hook 'rebind-commands)
(add-hook 'inf-ruby-mode-hook 'rebind-commands)
(add-hook 'erlang-mode-hook 'rebind-commands)

;;;; Flymake
(require 'flymake-cursor) ;;display error-messages without mouse

;;;; Ediff
(setq ediff-merge-split-window-function 'split-window-horizontally)
(setq ediff-split-window-function 'split-window-horizontally)

;;;; Cucumber
(add-to-list 'load-path (concat vendor-dir "/cucumber.el"))
(require 'feature-mode)


(require 'ibuffer)

(add-to-list 'load-path (concat dotfiles-dir "/vendor/emacs-jabber-0.8.0"))
(require 'jabber-autoloads)
(setq jabber-account-list
      '(("yves.senn@gmail.com"
         (:network-server . "talk.google.com")
         (:connection-type . ssl))
        ("senny.restorm@gmail.com"
         (:network-server . "talk.google.com")
         (:connection-type . ssl))))


;;;; rvm.el
(add-to-list 'load-path (concat vendor-dir "/rvm.el"))
(require 'rvm)

;;;; oddmuse
;; Get around the emacswiki spam protection
(add-hook 'oddmuse-mode-hook
          (lambda ()
            (unless (string-match "question" oddmuse-post)
              (setq oddmuse-post (concat "uihnscuskc=1;" oddmuse-post)))))

;;;; whitespace-mode
(setq whitespace-line-column 80
      whitespace-style '(tabs trailing lines-tail))

(global-whitespace-mode t)
