;; start the server to use emacsclient from the console
(server-start)

(setenv "PATH" (concat (getenv "PATH") ":/usr/local/git/bin:/opt/local/bin/find"))
(setq exec-path (append exec-path '("/usr/local/git/bin" "/opt/local/bin/find")))
(setq custom-file (concat private-config-dir
                          (if (string-match "aquamacs" (emacs-version))
                              "/custom/aquamacs-custom.el"
                            "/custom/carbon-emacs-custom.el")))
(load custom-file)

;; use emacs keybindings.
(setq mac-pass-command-to-system nil)

;; custom place to save customizations
(set-default-font "-apple-consolas-medium-r-normal--13-130-72-72-m-130-iso10646-1")

;; set maximize-frame padding for OS-X doc
(setq mf-display-padding-height 120)

(setq mac-emulate-three-button-mouse nil)

;; (vendor 'growl)