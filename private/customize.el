;; load base configuration
(load (concat private-config-dir "/custom/custom.el") 'noerror)

;; load specific configuration
(cond
 ((string-match "nt" system-configuration)
  (setq custom-file (concat private-config-dir "/custom/w32-custom.el")))
 ((string-match "apple" system-configuration)
  (setq custom-file (concat private-config-dir
                            (cond ((string-match "aquamacs" (emacs-version)) "/custom/aquamacs-custom.el")
                                  ((string-match "carbon" (emacs-version)) "/custom/carbon-emacs-custom.el")
                                  (t "/custom/cocoa-emacs-custom.el"))))))

(load custom-file 'noerror)
