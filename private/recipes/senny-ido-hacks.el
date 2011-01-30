(:name senny-ido-hacks
       :type http
       :url "http://www0.fh-trier.de/~politza/emacs/ido-hacks.el.gz"
       :build ("gunzip -c ido-hacks.el.gz > ido-hacks.el")
       :load    "ido-hacks.el"
       :compile "ido-hacks.el"

       ;; customization
       :after (lambda ()
                (ido-hacks-mode 1)
                (global-unset-key (kbd "M-x"))
                (global-unset-key (kbd "M-a"))
                (global-set-key (kbd "M-a") 'ido-hacks-execute-extended-command)
                (global-set-key (kbd "M-x") 'kill-region)))
