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

;; Platform-specific stuff
(when (eq system-type 'darwin)
  ;; Work around a bug on OS X where system-name is FQDN
  (setq system-name (car (split-string system-name "\\."))))

;; Load path etc.
(setq dotfiles-dir (file-name-directory
                    (or (buffer-file-name) load-file-name)))
(setq private-config-dir (concat dotfiles-dir "private"))
(setq vendor-dir (concat dotfiles-dir "vendor"))

(add-to-list 'load-path dotfiles-dir)
(add-to-list 'load-path private-config-dir)
(add-to-list 'load-path vendor-dir)

;; for loading libraries in from the vendor directory
(defun vendor (library)
  (let* ((file (symbol-name library))
         (normal (concat dotfiles-dir "vendor/" file))
         (suffix (concat normal ".el")))
    (cond
     ((file-directory-p normal) (add-to-list 'load-path normal) (require library))
     ((file-directory-p suffix) (add-to-list 'load-path suffix) (require library))
     ((file-exists-p suffix) (require library)))))

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
(load custom-file 'noerror)

;; You can keep system- or user-specific customizations here
(setq system-specific-config (concat dotfiles-dir "machines/" system-name ".el")
      user-specific-config (concat dotfiles-dir user-login-name ".el"))

(cond
 ((string-match "nt" system-configuration)
  (load "windows")
  )
 ((string-match "apple" system-configuration)
  (load "mac")
  ))

(if (file-exists-p (concat dotfiles-dir "local.el")) (load "local") )
(if (file-exists-p system-specific-config) (load system-specific-config))
(if (file-exists-p user-specific-config) (load user-specific-config))

(put 'narrow-to-region 'disabled nil)
(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)
