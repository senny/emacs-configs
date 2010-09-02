;; start the server to use emacsclient from the console
(server-start)

(setenv "PATH" (concat (getenv "PATH") ":/usr/local/bin"))
(setq exec-path (cons "/usr/local/bin" exec-path))
(setq custom-file (concat private-config-dir
                          (cond ((string-match "aquamacs" (emacs-version)) "/custom/aquamacs-custom.el")
                                ((string-match "carbon" (emacs-version)) "/custom/carbon-emacs-custom.el")
                                (t "/custom/cocoa-emacs-custom.el"))))
(load custom-file)

;; use emacs keybindings.
(setq mac-pass-command-to-system nil)

;; custom place to save customizations
(set-default-font "-apple-consolas-medium-r-normal--13-130-72-72-m-130-iso10646-1")

;; set maximize-frame padding for OS-X doc
(setq mf-display-padding-height 120)

(setq mac-emulate-three-button-mouse nil)

;; (vendor 'growl)
(defun senny-open-file-browser (directory)
  (interactive (list (file-name-directory (or (buffer-file-name) "~/"))))
  (shell-command (concat "open " directory)))

;;TODO: change the working directory of the new Terminal to the current directory.
(defun senny-open-terminal (directory)
  (interactive (list (file-name-directory (or (buffer-file-name) "~/"))))
  (shell-command (concat "open -a Terminal /bin/zsh")))

;; Platform-specific stuff
(when (eq system-type 'darwin)
  ;; Work around a bug on OS X where system-name is FQDN
  (setq system-name (car (split-string system-name "\\."))))
