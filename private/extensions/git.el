;;;; Magit
(eval-after-load 'magit '(progn
                           (define-key magit-mode-map (kbd "C-SPC") 'magit-show-item-or-scroll-down)
                           (define-key magit-mode-map (kbd "C-1") 'magit-show-level-1-all)
                           (define-key magit-mode-map (kbd "C-2") 'magit-show-level-2-all)
                           (define-key magit-mode-map (kbd "C-3") 'magit-show-level-3-all)
                           (define-key magit-mode-map (kbd "C-4") 'magit-show-level-4-all)))

;; This is a little hacky since VC doesn't support git add internally
(eval-after-load 'vc
  (define-key vc-prefix-map "i" '(lambda () (interactive)
                                   (if (not (eq 'Git (vc-backend buffer-file-name)))
                                       (vc-register)
                                     (shell-command (format "git add %s" buffer-file-name))
                                     (message "Staged changes.")))))


;; A monkeypatch to cause annotate to ignore whitespace
(defun vc-git-annotate-command (file buf &optional rev)
  (let ((name (file-relative-name file)))
    (vc-git-command buf 0 name "blame" "-w" rev)))
