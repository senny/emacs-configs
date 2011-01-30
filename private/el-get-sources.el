(add-to-list 'load-path (concat dotfiles-dir "el-get/el-get"))
(require 'el-get)

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
        switch-window

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

        (:name idle-highlight   :type elpa)
        (:name ruby-mode        :type elpa)
        (:name ruby-compilation :type elpa)
        (:name inf-ruby         :type elpa)
        (:name css-mode         :type elpa)
        (:name haml-mode        :type elpa)
        (:name sass-mode        :type elpa)
        (:name yari             :type elpa)

        ))

(el-get 'sync)
