(add-to-list 'auto-mode-alist '("\\.jsp$" . html-mode))
(add-to-list 'auto-mode-alist '("\\.java$" . java-mode))

(add-hook 'java-mode-hook
          (lambda ()
            ;; Additional Libraries
            (require 'javadoc-help)
            (require 'java-mode-indent-annotations)
            ))

(provide 'senny-java)
