(set-default 'senny-completion-function 'auto-complete)
(set-default 'senny-intellisense-completion-function 'auto-complete)

;;;; Yasnippet
(vendor 'yasnippet)
(setq yas/root-directory (concat private-config-dir "/snippets"))
(setq yas/prompt-functions '(yas/dropdown-prompt yas/ido-prompt))
(yas/load-directory yas/root-directory)
(setq yas/fallback-behavior 'call-other-command)
(yas/initialize)


;;;; Company Mode
;; (vendor 'company)
;; ;; only start completio when inserting character
;; (setq company-begin-commands '(self-insert-command))
;; (setq company-idle-delay nil)
;; (setq company-show-numbers nil)
;; (setq company-backends '(company-elisp
;;                          ;; company-nxml
;;                          ;; company-css
;;                          ;; company-eclim
;;                          ;; company-dabbrev-code
;;                          ;; company-semantic
;;                          ;; (company-gtags company-etags company-dabbrev-code company-keywords)
;;                          company-files
;;                          company-dabbrev
;;                          ))

;; (define-key company-active-map (kbd "M-k") 'company-select-next)
;; (define-key company-active-map (kbd "M-i") 'company-select-previous)
;; (define-key company-active-map (kbd "TAB") 'company-complete)

;;;; auto-complete
(vendor 'auto-complete)
(add-to-list 'ac-dictionary-directories (concat vendor-dir "/auto-complete/dict"))
(require 'auto-complete-config)
(ac-config-default)
(setq ac-auto-start nil)

(define-key ac-complete-mode-map (kbd "M-k") 'ac-next)
(define-key ac-complete-mode-map (kbd "M-i") 'ac-previous)
