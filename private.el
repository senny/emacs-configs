(setq vendor-dir (concat dotfiles-dir "vendor"))
(add-to-list 'load-path vendor-dir)

; for loading libraries in from the vendor directory
(defun vendor (library)
  (let* ((file (symbol-name library)) 
         (normal (concat dotfiles-dir "vendor/" file)) 
         (suffix (concat normal ".el")))
    (cond 
     ((file-directory-p normal) (add-to-list 'load-path normal) (require library))
     ((file-directory-p suffix) (add-to-list 'load-path suffix) (require library))
     ((file-exists-p suffix) (require library)))))

(cond 
  ((string-match "nt" system-configuration) 
   (load "windows")
  )
  ((string-match "apple" system-configuration)
    (load "mac")
  ))

(if (file-exists-p (concat dotfiles-dir "local.el")) (load "local") )

(vendor 'sunrise-commander)