(defvar elpa-packages (list 'perspective
                            'ruby-compilation
                            'rspec-mode
                            'rinari
                            'rvm
                            'yari)
  "Libraries that should be installed by default.")

(defun senny-elpa-install ()
  "Install all packages that aren't installed."
  (interactive)
  (dolist (package elpa-packages)
    (unless (or (member package package-activated-list)
                (functionp package))
      (message "Installing %s" (symbol-name package))
      (package-install package))))
