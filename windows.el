;; start the server to use emacsclient from the console
(server-start)

;; custom place to save customizations
(setq custom-file (concat private-config-dir "/custom/w32-custom.el"))
(load custom-file)

;; use dos-encoding in windows environments
(set-default-coding-systems 'iso-latin-1-dos)


(setq default-frame-alist '((font ."-outline-Monaco-normal-normal-normal-*-13-*-*-*-p-*-iso8859-1")))

;;use replace the dos environment with cygwin
;; (setenv "PATH" (getenv "PATH"))
;;(setq exec-path (cons "c:/cygwin/bin/" exec-path))
;;(vendor 'cygwin-mount)
;;(cygwin-mount-activate)

(vendor 'clearcase)
;;(add-to-list 'load-path (concat dotfiles-dir "vendor/vc-clearcase"))
;;(load "vc-clearcase-auto")

;;;; bindings
(add-hook 'after-init-hook
          (lambda ()
            (define-prefix-command 'windows-key-map)
            (global-set-key (kbd "M-w") 'windows-key-map)
            (define-key windows-key-map (kbd "e") 'senny-open-file-browser)
            (define-key windows-key-map (kbd "c") 'w32shell-cmd-here)))


(defun senny-open-file-browser ()
  "Launch the windows explorer in the current directory and selects current file"
  (interactive)
  (w32-shell-execute
   "open"
   "explorer"
   (concat "/e,/select," (convert-standard-filename buffer-file-name))))
