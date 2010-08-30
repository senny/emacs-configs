;; remove ui components
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'menu-bar-mode) (menu-bar-mode 1))

;; start maximized
(vendor 'maxframe)
(add-hook 'window-setup-hook 'x-maximize-frame t)

;; open new files in the same window
(setq gnuserv-frame (car (frame-list)))

;; highlight the selected region
(transient-mark-mode 1)

;; Delete the region when typing
(delete-selection-mode 1)

;; highlight
(show-paren-mode 1)
(global-hl-line-mode t)


(line-number-mode nil)
(column-number-mode nil)

;; Use a vertical bar as cursor
(blink-cursor-mode 1)
(setq-default cursor-type '(bar . 2))


(require 'color-theme)
(when (fboundp 'color-theme-initialize)
  (color-theme-initialize))
(setq color-theme-is-global t)
(color-theme-twilight)
