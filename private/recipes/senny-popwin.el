(:name senny-popwin
       :features popwin
       :type git
       :url "https://github.com/m2ym/popwin-el.git"
       :load-path "."
       :after (lambda ()
                (setq display-buffer-function 'popwin:display-buffer)
                (setq popwin:special-display-config '(("*Ido Completions*")))))
