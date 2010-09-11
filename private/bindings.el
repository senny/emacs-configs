;; Adopting the ergonomic keybindings
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

(global-set-key [\M-down] 'move-text-down)
(global-set-key [\M-up] 'move-text-up)

;; (define-key ido-common-completion-map (kbd "M-l") 'ido-next-match)
;; (define-key ido-common-completion-map (kbd "M-j") 'ido-prev-match)
;; (define-key ido-common-completion-map (kbd "M-i") 'ido-prev-match)
;; (define-key ido-common-completion-map (kbd "M-k") 'ido-next-match)
;; (define-key ido-file-dir-completion-map (kbd "C-x C-r") 'senny-ido-rgrep)

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
(global-set-key (kbd "M-1") 'senny-toggle-window-configuration-and-enlarged-window)
(global-set-key (kbd "M-0") 'delete-window)
(global-set-key (kbd "M-2") 'split-window-vertically)
(global-set-key (kbd "M-3") 'split-window-horizontally)
(global-set-key (kbd "M-4") 'delete-other-windows)

(global-unset-key (kbd "M-x")) ; execute-extended-command
(global-set-key (kbd "M-a") 'execute-extended-command)
(global-set-key (kbd "M-e") 'shell-command)

(global-set-key (kbd "M-s") 'move-cursor-next-pane)
(global-set-key (kbd "M-S") 'move-cursor-previous-pane)
(windmove-default-keybindings) ;; Shift+direction

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
(global-set-key (kbd "M-b") 'browse-kill-ring)

(global-unset-key (kbd "C-x C-f")) ; find-file
(global-unset-key (kbd "C-x h")) ; mark-whole-buffer
(global-unset-key (kbd "C-x C-w")) ; write-file
(global-set-key (kbd "C-o") 'find-file)
(global-set-key (kbd "C-S-o") 'senny-open-file-at-point)
(global-set-key (kbd "C-w") 'close-current-buffer)
(global-set-key (kbd "C-w") 'close-current-buffer)
(global-set-key (kbd "C-S-n") 'write-file)
(global-set-key (kbd "C-S-a") 'mark-whole-buffer)

;; Help should search more than just commands
(global-set-key (kbd "C-h a") 'apropos)

;; general
;; (global-set-key (kbd "C-x C-b") 'buffer-menu)
(global-set-key (kbd "C-c e") 'eval-and-replace)
(global-set-key (kbd "C-x C-b") 'ibuffer)
(global-set-key (kbd "C-x C-m") 'execute-extended-command)
(global-set-key (kbd "C-c C-m") 'execute-extended-command)
(global-set-key (kbd "C-x g") 'magit-status)
(global-set-key (kbd "C-x p") 'senny-ido-find-project)
(global-set-key (kbd "C-x w") 'senny-ispell-buffer)
(global-set-key (kbd "C-c p") 'senny-ido-find-config)
(global-set-key (kbd "C-f p") 'senny-open-task-file)
(global-set-key [C-return] 'defunkt-duplicate-line)
(global-set-key "\C-x\C-g" 'github-ido-find-file)
(global-set-key "\C-R" 'replace-string)
(global-set-key (kbd "C-$") 'senny-kill-buffer)
(global-set-key (kbd "C-c i") 'indent-buffer)
(global-set-key (kbd "C-c g") 'grep)
(global-set-key (kbd "C-x n r") 'indirect-region)
(global-set-key (kbd "C-c n") 'cleanup-buffer)
(global-set-key (kbd "C-h r") 'yari)

;; Window management
(global-set-key (kbd "C-+") 'increase-font-size) ; increase the font-size
(global-set-key (kbd "C-_") 'decrease-font-size) ; decrease the font-size
;; (define-key global-map (kbd "C-+") 'text-scale-increase)
;; (define-key global-map (kbd "C--") 'text-scale-decrease)


;; Buffer cycling
(global-set-key (kbd "C-ä") 'forward-buffer)
(global-set-key (kbd "C-ö") 'backward-buffer)

;; Completion
(global-set-key (kbd "TAB") 'senny-indent-or-complete)

;;;; Searching

;; Use regex searches by default.
(global-set-key (kbd "C-s") 'isearch-forward-regexp)
(global-set-key (kbd "\C-r") 'isearch-backward-regexp)
(global-set-key (kbd "C-M-s") 'isearch-forward)
(global-set-key (kbd "C-M-r") 'isearch-backward)
(global-unset-key (kbd "C-M-r")) ;; isearch-backwards
(global-set-key (kbd "C-*") 'isearch-forward-at-point)
(define-key isearch-mode-map (kbd "M-s") 'move-cursor-next-pane)
(define-key isearch-mode-map (kbd "M-v") 'isearch-yank-kill)

;; Activate occur easily inside isearch
(define-key isearch-mode-map (kbd "C-o")
  (lambda () (interactive)
    (let ((case-fold-search isearch-case-fold-search))
      (occur (if isearch-regexp isearch-string (regexp-quote isearch-string))))))

;; File finding
(global-set-key (kbd "C-x M-f") 'ido-find-file-other-window)
(global-set-key (kbd "C-x f") 'recentf-ido-find-file)
(global-set-key (kbd "C-c r") 'revert-buffer)
(global-set-key (kbd "M-`") 'file-cache-minibuffer-complete)
(global-set-key (kbd "C-x C-b") 'ibuffer)

;; no mailing!
(global-unset-key (kbd "C-x m"))
(global-unset-key "\C-z")

;; ediff
(global-set-key (kbd "C-c d b") 'ediff-buffers)
(global-set-key (kbd "C-c d f") 'ediff-files)
(global-set-key (kbd "C-c d d") 'ediff-directories)

(global-set-key (kbd "C-c C-k") 'comment-or-uncomment-region-or-line)

(global-set-key (kbd "C-c k") 'kill-compilation)


;;;; Mode Maps
(define-key isearch-mode-map (kbd "M-w") 'isearch-query-replace)

;; perspective
(global-set-key (kbd "C-p e") 'senny-persp/emacs)
(global-set-key (kbd "C-p p") 'senny-persp-last)
(global-set-key (kbd "C-p m") 'senny-persp/main)
(global-set-key (kbd "C-p j") 'senny-persp/jabber)
(global-set-key (kbd "C-p i") 'senny-persp/irc)
(global-set-key (kbd "C-p s") 'persp-switch)
