;; remove ui components
(when window-system
  (setq frame-title-format '(buffer-file-name "%f" ("%b")))
  (tooltip-mode -1)
  (mouse-wheel-mode t)
  (blink-cursor-mode -1))

(setq use-dialog-box nil)

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


(line-number-mode nil)
(column-number-mode nil)

;; Use a vertical bar as cursor
(blink-cursor-mode 1)
(setq-default cursor-type '(bar . 2))
(setq-default indicate-empty-lines t)


(global-font-lock-mode t)
(require 'color-theme)
(when (fboundp 'color-theme-initialize)
  (color-theme-initialize))
(setq color-theme-is-global t)
(color-theme-twilight)

(eval-after-load 'diff-mode
  '(progn
     (set-face-foreground 'diff-added "green4")
     (set-face-foreground 'diff-removed "red3")))

(eval-after-load 'magit
  '(progn
     (set-face-foreground 'magit-diff-add "green3")
     (set-face-foreground 'magit-diff-del "red3")))

(eval-after-load 'mumamo
  '(eval-after-load 'zenburn
     '(ignore-errors (set-face-background
                      'mumamo-background-chunk-submode "gray22"))))


(provide 'senny-display)
