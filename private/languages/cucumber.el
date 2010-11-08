;;;; Cucumber
(add-to-list 'load-path (concat vendor-dir "/cucumber.el"))
(require 'feature-mode)


;; Hooks
(defun default-cucumber-hook ()
  (set-pairs '("(" "\"" "\'")))

(add-hook 'feature-mode-hook 'default-cucumber-hook)
