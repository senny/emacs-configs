;; Adopting the ergonomic keybindings
;; (load (concat dotfiles-dir "vendor/ergonomic_keybinding_qwerty"))
(global-unset-key (kbd "C-b")) ; backward-char
(global-unset-key (kbd "C-f")) ; forward-char
(global-unset-key (kbd "C-p")) ; previous-line
(global-unset-key (kbd "C-n")) ; next-line
(global-set-key (kbd "M-j") 'backward-char)
(global-set-key (kbd "M-l") 'forward-char)
(global-set-key (kbd "M-i") 'previous-line)
(global-set-key (kbd "M-I") 'scroll-down)
(global-set-key (kbd "M-k") 'next-line)
(global-set-key (kbd "M-K") 'scroll-up)
(global-set-key (kbd "M-L") 'end-of-line)
(global-set-key (kbd "M-J") 'beginning-of-line)


(global-unset-key (kbd "M-b")) ; backward-word
(global-unset-key (kbd "M-f")) ; forward-word
(global-set-key (kbd "M-u") 'backward-word)
(global-set-key (kbd "M-o") 'forward-word)

(global-set-key (kbd "M-U") 'backward-paragraph)
(global-set-key (kbd "M-O") 'forward-paragraph)

(global-unset-key (kbd "C-<backspace>")) ; backward-kill-word
(global-unset-key (kbd "M-d")) ; kill-word
(global-set-key (kbd "M-e") 'backward-kill-word)
(global-set-key (kbd "M-r") 'kill-word)

(global-unset-key (kbd "C-d")) ; delete-char
(global-set-key (kbd "M-d") 'delete-backward-char)
(global-set-key (kbd "M-f") 'delete-char)
(global-set-key (kbd "<delete>") 'delete-char)

(global-unset-key (kbd "M-<")) ; beginning-of-buffer
(global-unset-key (kbd "M->")) ; end-of-buffer
(global-set-key (kbd "M-h") 'beginning-of-buffer)
(global-set-key (kbd "M-H") 'end-of-buffer)

(global-unset-key (kbd "C-x 1")) ; delete-other-windows
(global-unset-key (kbd "C-x 0")) ; delete-window
(global-set-key (kbd "M-1") 'delete-other-windows)
(global-set-key (kbd "M-0") 'delete-window)
(global-set-key (kbd "M-2") 'split-window-vertically)
(global-set-key (kbd "M-3") 'split-window-horizontally)

(global-unset-key (kbd "M-x")) ; execute-extended-command
(global-set-key (kbd "M-a") 'execute-extended-command)

(global-set-key (kbd "M-s") 'move-cursor-next-pane)
(global-set-key (kbd "M-S") 'move-cursor-previous-pane)
(defun move-cursor-next-pane ()
  "Move cursor to the next pane."
  (interactive)
  (other-window 1)
  )

(defun move-cursor-previous-pane ()
  "Move cursor to the previous pane."
  (interactive)
  (other-window -1)
  )

(global-unset-key (kbd "C-/")) ; undo
(global-unset-key (kbd "C-_")) ; undo
(global-set-key (kbd "M-Z") 'redo)
(global-set-key (kbd "M-z") 'undo)

(global-unset-key (kbd "C-SPC")) ; set-mark-command
(global-set-key (kbd "M-SPC") 'set-mark-command)
(global-set-key (kbd "M-S-SPC") 'mark-paragraph)

(global-unset-key (kbd "C-w")) ; kill-region
(global-unset-key (kbd "M-w")) ; kill-ring-save
(global-unset-key (kbd "C-y")) ; yank
(global-unset-key (kbd "M-y")) ; yank-pop
(global-set-key (kbd "M-x") 'kill-region)
(global-set-key (kbd "M-c") 'kill-ring-save)
(global-set-key (kbd "M-v") 'yank)
(global-set-key (kbd "M-V") 'yank-pop)

(global-unset-key (kbd "C-x C-f")) ; find-file
(global-unset-key (kbd "C-x h")) ; mark-whole-buffer
(global-unset-key (kbd "C-x C-w")) ; write-file
(global-set-key (kbd "C-o") 'find-file)
(global-set-key (kbd "C-S-o") 'senny-open-file-at-point)
(global-set-key (kbd "C-w") 'close-current-buffer)
(global-set-key (kbd "C-S-n") 'write-file)
(global-set-key (kbd "C-S-a") 'mark-whole-buffer)

;; general
(global-set-key (kbd "C-x C-b") 'buffer-menu)
(global-set-key (kbd "C-x C-m") 'execute-extended-command)
(global-set-key (kbd "C-c C-m") 'execute-extended-command)
(global-set-key (kbd "C-x g") 'magit-status)
(global-set-key (kbd "C-x p") 'senny-ido-find-project)
(global-set-key (kbd "C-c p") 'senny-ido-find-config)
(global-set-key (kbd "C-f p") 'senny-open-task-file)
(global-set-key [C-return] 'defunkt-duplicate-line)
(global-set-key "\C-x\C-g" 'github-ido-find-file)
(global-set-key "\C-R" 'replace-string)
(global-set-key (kbd "C-$") 'senny-kill-buffer)
(global-set-key (kbd "C-c i") 'indent-buffer)

;; Window management
(global-set-key [f1] 'resize-windows)
(global-set-key (kbd "C-+") 'increase-font-size) ; increase the font-size
(global-set-key (kbd "C-_") 'decrease-font-size) ; decrease the font-size

;; Buffer cycling
(global-set-key (kbd "C-ä") 'forward-buffer)
(global-set-key (kbd "C-ö") 'backward-buffer)

;; Completion
(global-set-key (kbd "C-SPC") 'alternate-completion)

;; vim emulation
(global-set-key (kbd "C-*") 'isearch-forward-at-point)

;; no printing!
(when (boundp 'osx-key-mode-map)
  (define-key osx-key-mode-map (kbd "A-p") 
    '(lambda () (interactive) (message "noop"))))

;; no mailing!
(global-unset-key (kbd "C-x m"))
(global-unset-key "\C-z")

(add-hook 'css-mode-hook '(lambda ()
                            (define-key css-mode-map [tab] 'defunkt-indent)))

;; diff
(global-set-key (kbd "C-c d b") 'ediff-buffers)
(global-set-key (kbd "C-c d b") 'ediff-files)

;; Textmate
(global-set-key (kbd "M-t") 'textmate-goto-file)
