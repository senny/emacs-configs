;;;; Defuns

(defun senny-ruby-compilation-this-buffer ()
  (interactive)
  (save-buffer)
  (let ((origin (current-buffer)))
    (ruby-compilation-this-buffer)
    (pop-to-buffer origin)))


(defun senny-ruby-open-gem ()
  (interactive)
  (let ((gem-dir "/Library/Ruby/Gems/1.8/gems/"))
    (find-file (ido-open-find-directory-files
                (concat gem-dir (ido-completing-read "Gem: "
                                                     (directory-files gem-dir nil "^[^.]")))))))

(eval-after-load 'ruby-mode
  '(progn
     ;;;; Additional Libraries
     (vendor 'rspec-mode)

     ;;;; Bindings
     (define-key ruby-mode-map (kbd "C-M-r") 'senny-ruby-compilation-this-buffer)
     (define-key ruby-mode-map (kbd "C-h r") 'ri)
     (define-key ruby-mode-map (kbd "C-c C-r g") 'senny-ruby-open-gem)
  ))

;;;; Ruby Block Mode
;; (vendor 'ruby-block)
;; (setq ruby-block-highlight-toggle t)
;; (setq ruby-block-delay nil)

(provide 'language-ruby)