;; term
(require 'multi-term)
(setq multi-term-program "/bin/zsh")

(defun senny-terminal ()
  (interactive)
  (multi-term)
  (jone-term-binding-fix))

(defun jone/term-kill-word ()
  "Kills the word by sending C-d to the term (zsh config)"
  (interactive)
  (term-send-raw-string "\C-d"))


(defun jone/term-cursor-at-command-line ()
  "Returns t if the cursor is somewhere at the last line of the current buffer."
  (eq (buffer-end 1) (line-end-position 2)))


(defun jone/term-paste ()
  "Fixed paste function in terminal mode"
  (interactive)
  (setq arg nil)
  (term-send-raw-string
   (current-kill
    (cond
     ((listp arg) 0)
     ((eq arg '-) -2)
     (t (1- arg))))))


(defun jone-term-binding-fix ()
  "Fixed the paste binding for term-mode"
  (interactive)

  (message "REBING KEYS")
  (local-unset-key (kbd "C-p"))

  ;; my zshell has a different configuration for moving, so
  ;; I'm using here my own shortcuts but the mult-term mode
  ;; provides default functions such as term-send-forward-word
  (local-set-key (kbd "M-v") 'jone/term-paste)
  (local-set-key (kbd "M-<backspace>") 'jone/term-kill-word)

  ;; M-o moves one word forward
  (local-set-key (kbd "M-o") (lambda ()
                               (interactive)
                               (if (jone/term-cursor-at-command-line)
                                   (term-send-raw-string "\^[[1;2C")
                                 (forward-word))))

  ;; M-u moves one word backward
  (local-set-key (kbd "M-u") (lambda ()
                               (interactive)
                               (if (jone/term-cursor-at-command-line)
                                   (term-send-raw-string "\^[[1;2D")
                                 (backward-word))))

  ;; M-l moves to the right
  (local-set-key (kbd "M-l") (lambda ()
                               (interactive)
                               (if (jone/term-cursor-at-command-line)
                                   (term-send-right)
                                 (forward-char))))

  ;; M-j moves to the left
  (local-set-key (kbd "M-j") (lambda ()
                               (interactive)
                               (if (jone/term-cursor-at-command-line)
                                   (term-send-left)
                                 (backward-char))))

  ;; M-L moves to the end of the line (END)
  (local-set-key (kbd "M-L") (lambda ()
                               (interactive)
                               (if (jone/term-cursor-at-command-line)
                                   (term-send-end)
                                 (end-of-line))))

  ;; M-J moves to the beginning of the line (HOME)
  (local-set-key (kbd "M-J") (lambda ()
                               (interactive)
                               (if (jone/term-cursor-at-command-line)
                                   (term-send-home)
                                 (beginning-of-line))))

  ;; C-<ESC> sends <ESC>, important within vim
  (local-set-key (kbd "C-<escape>") (lambda ()
                                      (interactive)
                                      (term-send-raw-string "\e033")))

  (local-set-key (kbd "M-U") 'term-send-up)
  (local-set-key (kbd "M-O") 'term-send-down))


(add-hook 'term-mode-hook 'jone-term-binding-fix)
