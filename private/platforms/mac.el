;; start the server to use emacsclient from the console
(server-start)

(setenv "PATH" (concat (getenv "PATH") ":/usr/local/bin"))
(setq exec-path (cons "/usr/local/bin" exec-path))

;; use emacs keybindings.
(setq mac-pass-command-to-system nil)

;; custom place to save customizations
(set-default-font "-apple-consolas-medium-r-normal--13-130-72-72-m-130-iso10646-1")

;; set maximize-frame padding for OS-X doc
(setq mf-display-padding-height 120)

(setq mac-emulate-three-button-mouse nil)

(defun platform-open-file-browser (directory)
  (interactive (list (file-name-directory (or (buffer-file-name) "~/"))))
  (shell-command (concat "open " directory)))

;;TODO: change the working directory of the new Terminal to the current directory.
(defun platform-open-terminal (directory)
  (interactive (list (file-name-directory (or (buffer-file-name) "~/"))))
  (shell-command (concat "open -a Terminal /bin/zsh")))

(defun mac-use-shell-path ()
  (let ((path-from-shell
         (replace-regexp-in-string
          "[[:space:]\n]*$" ""
          (shell-command-to-string "$SHELL -l -c 'echo $PATH'"))))
    (setenv "PATH" path-from-shell)
    (setq exec-path (split-string path-from-shell path-separator))))

;; Platform-specific stuff
(when (eq system-type 'darwin)
  ;; Work around a bug on OS X where system-name is FQDN
  (setq system-name (car (split-string system-name "\\."))))
