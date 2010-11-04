(add-to-list 'auto-mode-alist '("\\.org$" . org-mode))
(setq org-log-done t)
(defun org-summary-todo (n-done n-not-done)
  "Switch entry to DONE when all subentries are done, to TODO otherwise."
  (let (org-log-done org-log-states)    ; turn off logging
    (org-todo (if (= n-not-done 0) "DONE" "TODO"))))

(org-clock-persistence-insinuate)
(add-hook 'org-after-todo-statistics-hook 'org-summary-todo)
(setq org-agenda-clockreport-parameter-plist
      (quote (:link nil :maxlevel 4 :emphasize t)))
(setq org-clock-persist t)
(setq org-clock-out-when-done nil)

(defun default-org-mode-hook ()
  (set-pairs '("(" "{" "[" "\""))
  (define-key org-mode-map (kbd "C-c a") 'org-agenda)
  (define-key org-mode-map (kbd "C-c t u") 'org-clock-update-time-maybe)
  (define-key org-mode-map (kbd "C-c t g") 'org-clock-goto)
  (auto-fill-mode 1)
)

(add-hook 'org-mode-hook 'default-org-mode-hook)
