;; Yasnippet
(vendor 'yasnippet)
(yas/load-directory (concat dotfiles-dir "vendor/yasnippet/snippets"))
(yas/load-directory (concat private-config-dir "/snippets"))

(set-default 'senny-completion-function 'auto-complete)
(set-default 'senny-intellisense-completion-function 'auto-complete)

(defun senny-intelisense-complete ()
  (interactive)
  (if senny-intellisense-completion-function
      (funcall senny-intellisense-completion-function)
    (message "no intellisense completion function defined!")))

(defun senny-complete ()
  (interactive)
  (if senny-completion-function
      (funcall senny-completion-function)
    (message "no completion function defined!")))

(defun indent-or-complete ()
  (interactive)
  (if (looking-at "\\_>")
      (senny-complete)
    (indent-according-to-mode)))

(vendor 'company)

;; only start completio when inserting character
(setq company-begin-commands '(self-insert-command))
(setq company-idle-delay nil)
(setq company-show-numbers nil)
(setq company-backends '(company-elisp
                         ;; company-nxml
                         ;; company-css
                         ;; company-eclim
                         ;; company-dabbrev-code
                         ;; company-semantic
                         ;; (company-gtags company-etags company-dabbrev-code company-keywords)
                         company-files
                         company-dabbrev
                         ))

(setq company-eclim-executable "eclim")

(define-key company-active-map (kbd "M-k") 'company-select-next)
(define-key company-active-map (kbd "M-i") 'company-select-previous)
(define-key company-active-map (kbd "TAB") 'company-complete)

;;;; auto-complete
(when (vendor 'auto-complete)
  (require 'auto-complete-config)
  ;;(require 'auto-complete-yasnippet)
  ;;(require 'auto-complete-ruby)
  ;;(require 'auto-complete-css)
  ;;(require 'ac-dabbrev)

  (define-key ac-complete-mode-map (kbd "TAB") 'ac-complete)
  (define-key ac-complete-mode-map (kbd "RET") 'ac-complete)
  (define-key ac-complete-mode-map (kbd "M-k") 'ac-next)
  (define-key ac-complete-mode-map (kbd "M-i") 'ac-previous)
  (setq ac-auto-start nil)
  (setq ac-dwim t)
  (set-default 'ac-sources '(ac-source-words-in-buffer ac-source-dabbrev)))
