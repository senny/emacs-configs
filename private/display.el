;; remove ui components
;(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'menu-bar-mode) (menu-bar-mode 1))

;; start maximized
(vendor 'maxframe)
(add-hook 'window-setup-hook 'maximize-frame t)

;; Package to interactivly resize split screens
(require 'winsize)

;; highlight the selected region
(transient-mark-mode 1)

;; Delete the region when typing
(delete-selection-mode 1)

;; highlight
(show-paren-mode 1)

;; Use a vertical bar as cursor
(blink-cursor-mode 1)
(setq-default cursor-type '(bar . 2))


(require 'color-theme)
(when (fboundp 'color-theme-initialize)
  (color-theme-initialize))
(setq color-theme-is-global t)

(require 'color-theme-twilight)
(color-theme-twilight)