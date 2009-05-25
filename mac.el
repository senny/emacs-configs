;; start the server to use emacsclient from the console
(server-start)

(setq custom-file (concat private-config-dir "/custom/carbon-emacs-custom.el"))
(load custom-file)

;; use emacs keybindings.
(setq mac-pass-command-to-system nil)

; custom place to save customizations
(set-default-font "-apple-consolas-medium-r-normal--13-130-72-72-m-130-iso10646-1")

; set maximize-frame padding for OS-X doc
(setq mf-display-padding-height 120)

(setq mac-emulate-three-button-mouse nil)

;(vendor 'growl)