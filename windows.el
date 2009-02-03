; custom place to save customizations
(setq custom-file (concat private-config-dir "/custom/w32-custom.el"))
(load custom-file)

; use dos-encoding in windows environments
(set-default-coding-systems 'iso-latin-1-dos)


(setq default-frame-alist '((font ."-outline-Monaco-normal-normal-normal-*-13-*-*-*-p-*-iso8859-1")))

;use replace the dos environment with cygwin
;(setenv "PATH" (concat "c:/cygwin/bin;" (getenv "PATH")))
;(setq exec-path (cons "c:/cygwin/bin/" exec-path))
;(vendor 'cygwin-mount)
;(cygwin-mount-activate)

(vendor 'clearcase)
;(add-to-list 'load-path (concat dotfiles-dir "vendor/vc-clearcase"))
;(load "vc-clearcase-auto")

(load (concat dotfiles-dir "vendor/fipo"))
(fipo-mode 1)
