;;; init.el --- Where all the magic begins
;;
;; Part of the Emacs Starter Kit
;;
;; This is the first thing to get loaded.
;;
;; "Emacs outshines all other editing software in approximately the
;; same way that the noonday sun does the stars. It is not just bigger
;; and brighter; it simply makes everything else vanish."
;; -Neal Stephenson, "In the Beginning was the Command Line"

;; Turn off mouse interface early in startup to avoid momentary display
;; You really don't need these; trust me.
(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))

;; Load path etc.
(setq dotfiles-dir (file-name-directory (or (buffer-file-name) load-file-name))
      private-config-dir (concat dotfiles-dir "private")
      vendor-dir (concat dotfiles-dir "vendor"))

(add-to-list 'load-path dotfiles-dir)
(add-to-list 'load-path vendor-dir)

;; Load up ELPA, the package manager
(require 'package)
(package-initialize)
(load "private/elpa")

(add-to-list 'load-path (concat dotfiles-dir "/elpa-to-submit"))

(setq autoload-file (concat dotfiles-dir "loaddefs.el"))
(setq package-user-dir (concat dotfiles-dir "elpa"))

;; These should be loaded on startup rather than autoloaded on demand
;; since they are likely to be used in every session
(require 'cl)
(require 'saveplace)
(require 'ffap)
(require 'uniquify)
(require 'ansi-color)
(require 'recentf)

(load "private/customize")
(load "private/defun")
(load "private/bindings")
(load "private/registers")
(load "private/completion")
(load "private/display")
(load "private/modes")
(load "private/misc")
(load "private/git")
(load "private/perspectives")
(load "private/coding-hooks")

(load "private/languages/lisp")
(load "private/languages/ruby")
(load "private/languages/javascript")
(load "private/languages/java")
(load "private/languages/erlang")
(load "private/languages/perl")

(regen-autoloads)

(cond
 ((string-match "nt" system-configuration)
  (load "private/platforms/windows"))
 ((string-match "apple" system-configuration)
  (load "private/platforms/mac")))

;; You can keep system- or user-specific customizations here
(setq system-specific-config (concat dotfiles-dir "machines/" system-name ".el")
      user-specific-config (concat dotfiles-dir user-login-name ".el"))

(if (file-exists-p (concat dotfiles-dir "local.el")) (load "local") )
(if (file-exists-p system-specific-config) (load system-specific-config))
(if (file-exists-p user-specific-config) (load user-specific-config))


;; activate disabled features
(put 'narrow-to-region 'disabled nil)
(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)
