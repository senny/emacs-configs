(add-to-list 'el-get-recipe-path (concat private-config-dir "/recipes"))

(setq el-get-sources
      '(el-get
        gist
        yaml-mode
        rinari
        maxframe
        erc-highlight-nicknames
        java-mode-indent-annotations
        flymake-point
        ;; switch-window
        haml-mode
        sass-mode
        yari
        rvm
        theme-roller

        senny-textmate
        senny-ido-hacks
        senny-yasnippet
        senny-auto-complete
        senny-magit
        senny-minibuffer-complete-cycle
        senny-rspec-mode
        senny-perspective
        senny-mode-compile
        senny-ibuffer
        senny-browse-kill-ring
        senny-browse-kill-ring-plus
        senny-better-registers
        senny-ert
        senny-csv-mode
        senny-textile-mode
        senny-markdown-mode
        senny-rhtml-mode
        senny-cucumber
        senny-popwin

        (:name idle-highlight   :type elpa)
        (:name ruby-mode        :type elpa)
        (:name ruby-compilation :type elpa)
        (:name inf-ruby         :type elpa)

        ))

(el-get 'sync)
