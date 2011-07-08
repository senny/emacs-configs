(:name senny-magit
       :type git
       :url "http://github.com/philjackson/magit.git"
       :info "magit.info"
       ;; that used to be how to build it :build ("./autogen.sh" "./configure" "make")
       :build ("make all")
       :build/darwin `(,(concat "PATH=" invocation-directory ":$PATH make all"))
       :features magit

       ;; customization
       :after (lambda () (global-set-key (kbd "C-x g") 'magit-status)))
