(:name senny-rspec-mode
       :type git
       :url "https://github.com/pezra/rspec-mode.git"
       :compile "rspec-mode.el"
       :features rspec-mode
       :after (lambda ()
                (define-key ruby-mode-map (kbd "C-c , o") 'senny-open-spec-other-buffer)
                (define-key ruby-mode-map (kbd "C-c , ,") 'senny-open-spec-other-buffer)))
