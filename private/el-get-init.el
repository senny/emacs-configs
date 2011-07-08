(add-to-list 'load-path "~/.emacs.d/el-get/el-get")

(unless (require 'el-get nil t)
  (url-retrieve
   "https://github.com/dimitri/el-get/raw/master/el-get-install.el"
   (lambda (s)
     (end-of-buffer)
     (eval-print-last-sexp))))

(setq package-archives
      '(("original" . "http://tromey.com/elpa/")
        ("gnu" . "http://elpa.gnu.org/packages/")))

(load "private/el-get-sources")
