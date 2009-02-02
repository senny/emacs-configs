(setq custom-file (concat dotfiles-dir "senny/custom/carbon-emacs-custom.el"))
(load custom-file)

; custom place to save customizations
(set-default-font "-apple-consolas-medium-r-normal--13-130-72-72-m-130-iso10646-1")

; set maximize-frame padding for OS-X doc
(setq mf-display-padding-height 120)

(setq mac-emulate-three-button-mouse nil)

;(vendor 'growl)