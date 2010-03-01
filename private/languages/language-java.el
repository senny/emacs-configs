(add-to-list 'auto-mode-alist '("\\.jsp$" . html-mode))
(add-to-list 'auto-mode-alist '("\\.java$" . java-mode))

(eval-after-load 'java-mode-hook
  '(progn
     ;;;; Additional Libraries
     (require 'javadoc-help)
     (require 'java-mode-indent-annotations)

     ;;;; Bindings

     ))



(provide 'language-java)