(require 'recentf)
(setq recentf-exclude '(".emacsregisters.el" ".ido.last")
      recentf-max-saved-items 200)

(defun recentf-ido-find-file ()
  "Find a recent file using ido."
  (interactive)
  (let ((file (ido-completing-read "Choose recent file: " recentf-list nil t)))
    (when file
      (find-file file))))

(recentf-mode t)
