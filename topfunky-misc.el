;; DESCRIPTION: topfunky settings

(add-to-list 'load-path (concat dotfiles-dir "/vendor"))

;; Snippets
(add-to-list 'load-path "~/.emacs.d/vendor/yasnippet.el")
(require 'yasnippet)
(yas/initialize)
(yas/load-directory "~/.emacs.d/vendor/yasnippet.el/snippets")

;; Commands
(require 'unbound)

;; Minor Modes
(add-to-list 'load-path "~/.emacs.d/vendor/textmate.el")
(require 'textmate)
(textmate-mode)

;; Major Modes
(require 'textile-mode)
(add-to-list 'auto-mode-alist '("\\.textile\\'" . textile-mode))

(autoload 'markdown-mode "markdown-mode.el"
   "Major mode for editing Markdown files" t)

(require 'haml-mode)
(add-to-list 'auto-mode-alist '("\\.haml$" . haml-mode))

(require 'sass-mode)
(add-to-list 'auto-mode-alist '("\\.sass$" . sass-mode))

;; Keyboard
(require 'mac-bindings)

;; Font
(set-face-font 'default "-apple-inconsolata-medium-r-normal--20-0-72-72-m-0-iso10646-1")

;; Color Themes
(add-to-list 'load-path (concat dotfiles-dir "/vendor/color-theme"))
(require 'color-theme)
(color-theme-initialize)
(color-theme-charcoal-black)
;; (color-theme-gray30)

;; Functions

;; Full screen toggle
(defun toggle-fullscreen () 
  (interactive) 
  (set-frame-parameter nil 'fullscreen (if (frame-parameter nil 'fullscreen) 
                                           nil 
                                         'fullboth)))
;; (global-set-key [(meta return)] 'toggle-fullscreen)

;; Other

(prefer-coding-system 'utf-8)

(server-start)

(provide 'topfunky-misc)

