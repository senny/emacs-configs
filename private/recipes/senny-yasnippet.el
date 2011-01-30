(:name senny-yasnippet
       :type svn
       :url "http://yasnippet.googlecode.com/svn/trunk/"
       :features "yasnippet"

       ;; customization
       :after (lambda ()
                (set-default 'yas/fallback-behavior '(apply senny-indent-or-complete))))
