(add-to-list 'load-path (concat dotfiles-dir "el-get/el-get"))

(defun senny-el-get-init ()
  (interactive)
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
  )

(if (require 'el-get nil t)
    (progn
      (message "el-get is already installed, try M-x el-get-update")
      (senny-el-get-init))
  (url-retrieve
   "https://github.com/dimitri/el-get/raw/master/el-get-install.el"
   (lambda (s)
     (end-of-buffer)
     (eval-print-last-sexp)
     (senny-el-get-init))))
