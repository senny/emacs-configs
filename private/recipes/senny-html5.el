(:name senny-html5
       :type git
       :after (lambda ()
                (eval-after-load "rng-loc"
                  '(add-to-list 'rng-schema-locating-files "~/.emacs.d/el-get/senny-html5/schemas.xml"))
                (require 'whattf-dt))
       :url "https://github.com/hober/html5-el.git")
