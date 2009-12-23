;; DESCRIPTION:
;;   Useful patterns for using the ido menu with Javascript files.
;;
;; AUTHOR:
;;   Geoffrey Grosenbach http://peepcode.com
;;
;; Matches things like:
;;
;;   function bacon() {}        // Standard function
;;   getJSON: function () {}    // Function as a key in a hash
;;   this.post = function () {} // Instance method in a function
;;
;; USAGE:
;;   (require 'topfunky-js)

(setq topfunky-js-imenu-generic-expression
      '(("Named Function" "function\\s-+\\(\\w+\\)\\s-*(" 1)
        ("Hash Method"  "^\\s-*\\(\\w+\\):\\s-*function\\s-*(" 1)
        ("Instance Method" "this\.\\(\\w+\\)\\s-*=\\s-*function\\s-*(" 1)
        ))

(add-hook 'javascript-mode-hook
          (lambda ()
            (setq imenu-generic-expression topfunky-js-imenu-generic-expression)))

(provide 'topfunky-js)
