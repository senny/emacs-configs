(defun senny-intelisense-complete ()
  (interactive)
  (if senny-intellisense-completion-function
      (funcall senny-intellisense-completion-function)
    (message "no intellisense completion function defined!")))

(defun senny-complete ()
  (interactive)
  (if senny-completion-function
      (funcall senny-completion-function)
    (message "no completion function defined!")))

(defun senny-indent-or-complete ()
  (interactive)
  (if (looking-at "\\_>")
      (senny-complete)
    (indent-according-to-mode)))

(defun senny-kill-buffer ()
  (interactive)
  (kill-buffer (buffer-name)))

(defun insert-soft-tab ()
  (interactive)
  (insert "  "))

(defun defunkt-indent ()
  (interactive)
  (insert "  "))

(defadvice zap-to-char (after dont-zap-char (arg char))
  "Doesn't include the char - zaps to the char before it (like vim)."
  (insert char)
  (backward-char))
(ad-activate 'zap-to-char)

(defun senny-ido-find-config ()
  (interactive)
  (find-file
   (concat dotfiles-dir "/" (ido-completing-read "Config file: "
                                                 (append
                                                  (mapcar (lambda (file)(concat "private/" file))
                                                          (directory-files private-config-dir nil "^[^.]"))
                                                  (directory-files dotfiles-dir nil "^[^.]"))))))

(defun senny-ido-find-project ()
  (interactive)
  (find-file (ido-open-find-directory-files
              (concat "~/Projects/" (ido-completing-read "Project: "
                                                         (directory-files "~/Projects/" nil "^[^.]"))))))

(defun senny-open-task-file ()
  (interactive)
  (find-file (ido-open-find-directory-files "~/tasks")))

;; fix kill-word
(defun defunkt-kill-word (arg)
  "Special version of kill-word which swallows spaces separate from words"
  (interactive "p")

  (let ((whitespace-regexp "\\s-+"))
    (kill-region (point)
                 (cond
                  ((looking-at whitespace-regexp) (re-search-forward whitespace-regexp) (point))
                  ((looking-at "\n") (kill-line) (defunkt-kill-word arg))
                  (t (forward-word arg) (point))))))

(defun defunkt-backward-kill-word (arg)
  "Special version of backward-kill-word which swallows spaces separate from words"
  (interactive "p")
  (if (looking-back "\\s-+")
      (kill-region (point) (progn (re-search-backward "\\S-") (forward-char 1) (point)))
    (backward-kill-word arg)))

                                        ;duplicate the current line
(defun defunkt-duplicate-line ()
  (interactive)
  (beginning-of-line)
  (copy-region-as-kill (point) (progn (end-of-line) (point)))
  (textmate-next-line)
  (yank)
  (beginning-of-line)
  (indent-according-to-mode))

(defun ido-open-find-directory-files (directory)
  (concat directory "/" (ido-completing-read (concat directory ":")
                                             (split-string
                                              (shell-command-to-string
                                               (concat
                                                "find \"" (expand-file-name directory)
                                                "\" -type f -printf \"%P\\n\" | grep -v \"^\.git\""))))))


(defun url-fetch-into-buffer (url)
  (interactive "sURL:")
  (insert (concat "\n\n" ";; " url "\n"))
  (insert (url-fetch-to-string url)))

(defun url-fetch-to-string (url)
  (with-current-buffer (url-retrieve-synchronously url)
    (beginning-of-buffer)
    (search-forward-regexp "\n\n")
    (delete-region (point-min) (point))
    (buffer-string)))

;; from http://platypope.org/blog/2007/8/5/a-compendium-of-awesomeness
;; I-search with initial contents
(defvar isearch-initial-string nil)

(defun isearch-set-initial-string ()
  (remove-hook 'isearch-mode-hook 'isearch-set-initial-string)
  (setq isearch-string isearch-initial-string)
  (isearch-search-and-update))

(defun isearch-forward-at-point (&optional regexp-p no-recursive-edit)
  "Interactive search forward for the symbol at point."
  (interactive "P\np")
  (if regexp-p (isearch-forward regexp-p no-recursive-edit)
    (let* ((end (progn (skip-syntax-forward "w_") (point)))
           (begin (progn (skip-syntax-backward "w_") (point))))
      (if (eq begin end)
          (isearch-forward regexp-p no-recursive-edit)
        (setq isearch-initial-string (buffer-substring begin end))
        (add-hook 'isearch-mode-hook 'isearch-set-initial-string)
        (isearch-forward regexp-p no-recursive-edit)))))

(defun backward-buffer () (interactive)
  "Switch to previously selected buffer."
  (let* ((list (cdr (buffer-list)))
         (buffer (car list)))
    (while (and (cdr list) (string-match "\\*" (buffer-name buffer)))
      (progn
        (setq list (cdr list))
        (setq buffer (car list))))
    (bury-buffer)
    (switch-to-buffer buffer)))

(defun forward-buffer () (interactive)
  "Opposite of backward-buffer."
  (let* ((list (reverse (buffer-list)))
         (buffer (car list)))
    (while (and (cdr list) (string-match "\\*" (buffer-name buffer)))
      (progn
        (setq list (cdr list))
        (setq buffer (car list))))
    (switch-to-buffer buffer)))

(defun alternate-completion ()
  "this function is used for secondary completion beside tabkey2"
  (interactive)
  (let ((current-completion (assoc major-mode *alternate-completion-functions-alist*)))
    (when (consp current-completion)
      (funcall (cdr current-completion)))))

(defun comment-or-uncomment-line (&optional lines)
  "Comment current line. Argument gives the number of lines
forward to comment"
  (interactive "P")
  (comment-or-uncomment-region
   (line-beginning-position)
   (line-end-position lines)))

(defun comment-or-uncomment-region-or-line (&optional lines)
  "If the line or region is not a comment, comments region
if mark is active, line otherwise. If the line or region
is a comment, uncomment."
  (interactive "P")
  (if mark-active
      (if (< (mark) (point))
          (comment-or-uncomment-region (mark) (point))
        (comment-or-uncomment-region (point) (mark))
        )
    (comment-or-uncomment-line lines)))

(defun senny-flyspell-prog-mode ()
  (interactive)
  (ispell-change-dictionary "english")
  (flyspell-prog-mode)
  (flyspell-buffer))

(defun senny-ispell-buffer ()
  (interactive)
  (call-interactively 'ispell-change-dictionary)
  (ispell-buffer))

(defun senny-flyspell-mode ()
  (interactive)
  (call-interactively 'ispell-change-dictionary)
  (call-interactively 'flyspell-mode))

(defun increase-font-size ()
  (interactive)
  (set-face-attribute 'default
                      nil
                      :height
                      (ceiling (* 1.10
                                  (face-attribute 'default :height))))
  (restore-frame)
  (maximize-frame))

(defun decrease-font-size ()
  (interactive)
  (set-face-attribute 'default
                      nil
                      :height
                      (floor (* 0.9
                                (face-attribute 'default :height))))
  (restore-frame)
  (maximize-frame))

(defun senny-open-file-at-point ()
  (interactive)
  (let ((ido-use-filename-at-point t))
    (ido-find-file)))

(defun set-pairs (pairs)
  "Sets up handling of pair characters."
  (mapcar (lambda (pair)
            (local-set-key pair 'skeleton-pair-insert-maybe))
          ;; (cond ((string= pair "\"") (local-set-key pair 'move-over-dbl-quote))
          ;;       ((string= pair "\'") (local-set-key pair 'move-over-quote))
          ;;       ((string= pair "|") (local-set-key pair 'move-over-pipe))
          ;;       ((string= pair "[") (local-set-key "\]" 'move-over-square))
          ;;       ((string= pair "(") (local-set-key "\)" 'move-over-bracket))
          ;;       ((string= pair "{") (local-set-key "\}" 'move-over-curly))))
          pairs)
  (setq skeleton-pair t))

(defun senny-ido-rgrep ()
  (interactive)
  (let ((enable-recursive-minibuffers t))
    (grep (concat "grep -rnH " (read-string "Pattern: ") " " (directory-file-name ido-current-directory)))
    (switch-to-buffer "*grep*" nil t)
    (keyboard-escape-quit)))

(defun senny-w32-explorer-open ()
  "Launch the windows explorer in the current directory and selects current file"
  (interactive)
  (w32-shell-execute
   "open"
   "explorer"
   (concat "/e,/select," (convert-standard-filename buffer-file-name))))

(defun move-cursor-next-pane ()
  "Move cursor to the next pane."
  (interactive)
  (other-window 1))

(defun move-cursor-previous-pane ()
  "Move cursor to the previous pane."
  (interactive)
  (other-window -1))

(defun move-text-internal (arg) 
   (cond 
    ((and mark-active transient-mark-mode) 
     (if (> (point) (mark)) 
        (exchange-point-and-mark)) 
     (let ((column (current-column)) 
          (text (delete-and-extract-region (point) (mark)))) 
       (forward-line arg) 
       (move-to-column column t) 
       (set-mark (point)) 
       (insert text) 
       (exchange-point-and-mark) 
       (setq deactivate-mark nil))) 
    (t 
     (beginning-of-line) 
     (when (or (> arg 0) (not (bobp))) 
       (forward-line) 
       (when (or (< arg 0) (not (eobp))) 
        (transpose-lines arg)) 
       (forward-line -1))))) 

(defun move-text-down (arg) 
   "Move region (transient-mark-mode active) or current line 
  arg lines down." 
   (interactive "*p") 
   (move-text-internal arg)) 

(defun move-text-up (arg) 
   "Move region (transient-mark-mode active) or current line 
  arg lines up." 
   (interactive "*p") 
   (move-text-internal (- arg))) 


(defun senny-toggle-window-configuration-and-enlarged-window ()
  (interactive)
  (if (= (count-windows nil) 1)
      (better-registers-jump-to-register ?w)
    (window-configuration-to-register ?w)
    (delete-other-windows)))

(defun indirect-region (start end)
  "Edit the current region in another buffer. You can choose a new
major mode for the newly created buffer."
  (interactive "r")
  (let ((buffer-name (generate-new-buffer-name "*indirect*"))
        (mode (intern
               (completing-read
                "Mode: "
                (mapcar (lambda (e)
                          (list (symbol-name e)))
                        (apropos-internal "-mode$" 'commandp))
                nil t nil))))
    (pop-to-buffer (clone-indirect-buffer buffer-name nil))
    (funcall mode)
    (narrow-to-region start end)
    (goto-char (point-min))
    (shrink-window-if-larger-than-buffer)))