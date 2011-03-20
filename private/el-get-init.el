(add-to-list 'load-path (concat dotfiles-dir "el-get/el-get"))

(if (require 'el-get nil t)
    (progn
      (message "el-get is already installed, try M-x el-get-update")
      (load "private/el-get-sources"))
  (url-retrieve
   "https://github.com/dimitri/el-get/raw/master/el-get-install.el"
   (lambda (s)
     (end-of-buffer)
     (eval-print-last-sexp)
     (load "private/el-get-sources"))))

(setq package-archives
      '(("original" . "http://tromey.com/elpa/")
        ("gnu" . "http://elpa.gnu.org/packages/")))
