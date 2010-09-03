(cond 
  ((string-match "nt" system-configuration) 
   (load "windows")
  )
  ((string-match "apple" system-configuration)
    (load "mac")
  ))

(if (file-exists-p (concat dotfiles-dir "local.el")) (load "local") )

(vendor 'sunrise-commander)