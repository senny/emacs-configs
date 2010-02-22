;;;; Defuns

(defun senny-ruby-compilation-this-buffer ()
  (interactive)
  (save-buffer)
  (let ((origin (current-buffer)))
    (ruby-compilation-this-buffer)
    (pop-to-buffer origin)))


;;;; Bindings
(eval-after-load 'ruby-mode
  '(progn
     (define-key ruby-mode-map (kbd "C-M-r") 'senny-ruby-compilation-this-buffer)))

;;;; Ruby Block Mode
;; (vendor 'ruby-block)
;; (setq ruby-block-highlight-toggle t)
;; (setq ruby-block-delay nil)

;;;; RSpec
(vendor 'rspec-mode)

(provide 'language-ruby)