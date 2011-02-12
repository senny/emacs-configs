;; remove ui components
(when window-system
  (setq frame-title-format '(buffer-file-name "%f" ("%b")))
  (tooltip-mode -1)
  (mouse-wheel-mode t))

(setq use-dialog-box nil
      visible-bell t
      echo-keystrokes 0.1
      inhibit-startup-message t
      truncate-partial-width-windows nil)

(defalias 'yes-or-no-p 'y-or-n-p)

;; start maximized
(vendor 'maxframe)
(add-hook 'window-setup-hook 'x-maximize-frame t)

;; open new files in the same window
(setq gnuserv-frame (car (frame-list)))

;; highlight the selected region
(transient-mark-mode 1)

;; Delete the region when typing
(delete-selection-mode 1)

;; Highlight matching parentheses when the point is on them.
(show-paren-mode 1)

;; Highlight the current line
(global-hl-line-mode t)

(line-number-mode t)
(column-number-mode t)

;; Use a vertical bar as cursor
(blink-cursor-mode 1)
(setq-default cursor-type '(bar . 2))
(setq-default indicate-empty-lines t)

;; theme-roller.el setup
;;(vendor 'theme-roller)
;;(setq theme-roller-active-themes '(color-theme-lazy color-theme-monokai))
;;(theme-roller-activate)
