;; DESCRIPTION:
;;   Nasty hack to add Sinatra blocks to ruby-mode imenu.
;;
;;   Basically, makes it easy to jump between Sinatra URL handlers
;;   with Chris Wanstrath's textmate.el or the normal imenu.
;;
;; AUTHOR:
;;   Geoffrey Grosenbach http://peepcode.com
;;
;; Matches things like:
;;
;;   get "/foo" do
;;   put /eat\/(bacon)/ do |meat|
;;
;; USAGE:
;;   (require 'topfunky-sinatra)

(defun ruby-sinatra-imenu-create-index ()
  "Create an imenu index of all methods in the buffer."
  (nreverse (ruby-sinatra-imenu-create-index-in-block nil (point-min) nil)))

;; HACK Copied from ruby-mode.el
(defun ruby-sinatra-imenu-create-index-in-block (prefix beg end)
  "Create an imenu index of methods inside a block."
  (let ((index-alist '()) (case-fold-search nil)
        name next pos decl sing)
    (goto-char beg)
    ;; Nasty
    (while (re-search-forward "^\\s *\\(\\(class\\s +\\|\\(class\\s *<<\\s *\\)\\|module\\s +\\)\\([^\(<\n ]+\\)\\|\\(def\\|alias\\)\\s +\\([^\(\n ]+\\)\\|\\(get\\|post\\|put\\|delete\\)\\s +\\([^ ]+\\)\\)" end t)
      (setq sing (match-beginning 3))
      (setq decl (match-string 5))
      (setq next (match-end 0))
      (setq name (or (match-string 4) (match-string 6) (match-string 8)))
      (setq http-method (match-string 7))
      (setq pos (match-beginning 0))
      (cond
       ;; Adds "get 'foo'" to the list of methods
       (http-method
        (push (cons (concat http-method " " name) pos) index-alist))
       ((string= "alias" decl)
        (if prefix (setq name (concat prefix name)))
        (push (cons name pos) index-alist))
       ((string= "def" decl)
        (if prefix
            (setq name
                  (cond
                   ((string-match "^self\." name)
                    (concat (substring prefix 0 -1) (substring name 4)))
                   (t (concat prefix name)))))
        (push (cons name pos) index-alist)
        (ruby-accurate-end-of-block end))
       (t
        (if (string= "self" name)
            (if prefix (setq name (substring prefix 0 -1)))
          (if prefix (setq name (concat (substring prefix 0 -1) "::" name)))
          (push (cons name pos) index-alist))
        (ruby-accurate-end-of-block end)
        (setq beg (point))
        (setq index-alist
              (nconc (ruby-sinatra-imenu-create-index-in-block
                      (concat name (if sing "." "#"))
                      next beg) index-alist))
        (goto-char beg))))
    index-alist))

(add-hook 'ruby-mode-hook
          (function (lambda ()
                      (setq imenu-create-index-function 'ruby-sinatra-imenu-create-index)
                      )))

(provide 'topfunky-sinatra)
