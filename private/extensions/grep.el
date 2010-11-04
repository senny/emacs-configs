;; grep
(eval-after-load 'grep
  '(when (boundp 'grep-find-ignored-files)
     (add-to-list 'grep-find-ignored-files "target")
     (add-to-list 'grep-find-ignored-files "*.class")))
