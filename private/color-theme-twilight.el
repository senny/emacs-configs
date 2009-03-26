;; Twilight Colour Theme for Emacs.
;;
;; Defines a colour scheme resembling that of the original TextMate Twilight colour theme.
;; To use add the following to your .emacs file (requires the color-theme package):
;;
;; (require 'color-theme)
;; (color-theme-initialize)
;; (load-file "~/.emacs.d/twilight-emacs/color-theme-twilight.el")
;;
;; And then (color-theme-twilight) to activate it.
;;
;; Several areas still require improvement such as recognition of code that ruby-mode doesn't
;; yet pick up (eg. parent classes), Rails/Merb keywords, or non Ruby code related areas
;; (eg. dired, HTML, etc). Please feel free to customize further and send in any improvements,
;; patches most welcome.
;;
;; MIT License Copyright (c) 2008 Marcus Crafter <crafterm@redartisan.com>
;; Credits due to the excellent TextMate Twilight theme
;;

(defun color-theme-twilight ()
  "Color theme by Marcus Crafter, based off the TextMate Twilight theme, created 2008-04-18"
  (interactive)
  (color-theme-install
   '(color-theme-twilight
     ((background-color . "#141414")
      (background-mode . dark)xb
      (border-color . "black")
      (cursor-color . "#A7A7A7")
      (foreground-color . "#F8F8F8")
      (mouse-color . "sienna1"))
     (border-glyph ((t (nil))))
     (buffers-tab ((t (:background "black" :foreground "white"))))
     
     (preprocessor ((t (:foreground "Aquamarine"))))
     (highlighted ((t (:background "#606060"))))
     (flashy-highlight ((t (:background "#6C6A41"))))
     (default ((t (:foreground "#F8F8F8"))))
     (variable ((t (:foreground "#7587A6"))))
     (constant ((t (:foreground "#CF6A4C"))))
     (type ((t (:foreground "#9B703F"))))
     (keyword ((t (:foreground "#CDA869"))))
     (string ((t (:foreground "#8F9D6A"))))
     (comment ((t (:italic t :foreground "#5F5A60"))))
     (doc ((t (:foreground "#9B859D"))))
     (function-def ((t (:inherit default))))
     (code-error ((t (:underline t :inherit diff-removed))))
     (code-warning ((t (:underline t :inherit diff-changed))))

     (paren-face ((t (:inherit default))))
     (esk-paren-face ((t (:inerhit default))))
     
     (font-lock-builtin-face ((t (:inhert default))))
     (font-lock-comment-face ((t (:inherit comment))))
     (font-lock-constant-face ((t (:inherit constant))))
     (font-lock-doc-face ((t (:inherit string))))
     (font-lock-doc-string-face ((t (:inherit doc))))
     (font-lock-function-name-face ((t (:inherit function-def))))
     (font-lock-keyword-face ((t (:inherit keyword))))
     (font-lock-preprocessor-face ((t (:inherit preprocessor))))
     (font-lock-reference-face ((t (:inherit variable))))

     (font-lock-regexp-grouping-backslash ((t (:foreground "#E9C062"))))
     (font-lock-regexp-grouping-construct ((t (:foreground "red"))))

     (font-lock-string-face ((t (:inherit string))))
     (font-lock-type-face ((t (:inherit type))))
     (font-lock-variable-name-face ((t (:inherit variable))))
     (font-lock-warning-face ((t (:bold t :foreground "Pink"))))
     
     (jde-java-font-lock-doc-tag-face ((t (:inherit doc :bold t))))
     (jde-java-font-lock-constant-face ((t (:inherit constant))))
     (jde-java-font-lock-link-face ((t (:underline t :foreground "dark-blue"))))
     (jde-java-font-lock-modifier-face ((t (:inherit keyword))))
     (jde-java-font-lock-package-face ((t (:inherit keyword))))
     (jde-java-font-lock-number-face ((t (:inherit string))))
     (jde-java-font-lock-code-face ((t (:inherit default))))
     (jde-db-spec-breakpoint-face ((t (:inherit diff-header))))
     (jde-db-requested-breakpoint-face ((t (:inherit diff-changed))))
     (jde-db-active-breakpoint-face ((t (:inherit diff-added))))
     
     (ecb-default-highlight-face ((t (:inherit highlighted))))
     
     (log4j-font-lock-info-face ((t (:inherit variable))))
     (log4j-font-lock-error-face ((t (:inherit constant))))
     (log4j-font-lock-warn-face ((t (:inherit keyword))))
     
     (diff-header ((t (:inherit default :background "#0E2231"))))
     (diff-added ((t (:inherit default :background "#253B22"))))
     (diff-removed ((t (:inherit default :background "#420E09"))))
     (diff-changed ((t (:inherit default :background "#4A410D"))))
     (diff-context ((t (:inherit font-lock-comment))))
     (diff-index ((t (:inherit font-lock-comment))))
     (diff-file-header ((t (:inherit font-lock-comment :bold t))))

     (jde-db-active-breakpoint-face ((t (:inherit diff-removed))))
     (jde-db-requested-breakpoint-face ((t (:inherit diff-changed))))
     (jde-db-spec-breakpoint-face ((t (:inherit diff-added))))

     (flymake-errline ((t (:inherit code-error))))
     (flymake-warnline ((t (:inherit code-warning))))
     
     (ediff-current-diff-A ((((class color) (min-colors 16)) (:background "#01243C" :foreground "white"))))
     (ediff-current-diff-Ancestor ((((class color) (min-colors 16)) (:background "#4D0600" :foreground "white"))))
     (ediff-current-diff-B ((((class color) (min-colors 16)) (:background "#574A00" :foreground "white"))))
     (ediff-current-diff-C ((((class color) (min-colors 16)) (:background "#5C285C" :foreground "white"))))
     (ediff-even-diff-A ((((class color) (min-colors 16)) (:background "#222222"))))
     (ediff-even-diff-Ancestor ((((class color) (min-colors 16)) (:background "#222222"))))
     (ediff-even-diff-B ((((class color) (min-colors 16)) (:background "#222222"))))
     (ediff-even-diff-C ((((class color) (min-colors 16)) (:background "#222222"))))
     (ediff-fine-diff-A ((((class color) (min-colors 16)) (:background "#0B5C00" :foreground "white"))))
     (ediff-fine-diff-Ancestor ((((class color) (min-colors 16)) (:background "#0B5C00" :foreground "white"))))
     (ediff-fine-diff-B ((((class color) (min-colors 16)) (:background "#0B5C00" :foreground "white"))))
     (ediff-fine-diff-C ((((class color) (min-colors 16)) (:background "#0B5C00" :foreground "white"))))
     (ediff-odd-diff-A ((((class color) (min-colors 16)) (:background "#222222"))))
     (ediff-odd-diff-Ancestor ((((class color) (min-colors 16)) (:background "#222222"))))
     (ediff-odd-diff-B ((((class color) (min-colors 16)) (:background "#222222"))))
 
     (js2-error-face ((t (:inherit code-error))))
     (js2-external-variable-face ((t (:foreground))))
     (js2-function-param-face ((t (:inherit variable))))
     (js2-instance-member-face ((t (:inherit variable))))
     (js2-private-function-call-face ((t (:inherit default))))
     (js2-private-member-face ((t (:inherit variable))))
     (js2-warning-face ((t (:inherit code-warning))))
     ;(js2-jsdoc-html-tag-delimiter-face
     ;(js2-jsdoc-html-tag-name-face       
     ;(js2-jsdoc-tag-face                 
     ;(js2-jsdoc-type-face                
     ;(js2-jsdoc-value-face

     (html-tag-face ((t (:inherit keyword))))

     (mumamo-background-chunk-major ((t (:inherit background-color))))
     (mumamo-background-chunk-submode ((t (:inherit background-color))))
     (nxml-delimited-data-face ((t (:inherit string))))
     (nxml-name-face ((t (:inherit variable))))
     (nxml-ref-face ((t (:inherit constant))))
     (nxml-delimiter-face ((t (:inherit keyword))))
     (nxml-tag-delimiter-face ((t (:inherit default))))
     (nxml-tag-delimiter ((t (:inherit default))))
     (nxml-tag-slash-face ((t (:inherit keyword))))
     (nxml-text-face ((t (:inherit default))))
     (nxml-attribute-local-name-face ((t (:inherit variable))))
     (nxml-attribute-value-face ((t (:inherit string))))
     (nxml-attribute-value-delimiter-face ((t (:inherit string))))
     (nxml-comment-content-face ((t (:inherit comment))))
     (nxml-comment-delimiter-face ((t (:inherit comment))))
     (nxml-processing-instruction-delimiter-face ((t (:inherit preprocessor))))
     (nxml-cdata-section-CDATA-face ((t (:inherit constant))))
     (nxml-cdata-section-delimiter-face ((t (:inherit keyword))))
     (nxml-element-local-name-face ((t (:inherit keyword))))
     (nxml-element-local-name ((t (:inherit keyword))))


     (sgml-namespace ((t (:inherit type))))
     (css-selector ((t (:inherit keyword))))
     
     (yas/mirror-highlight-face ((t (:background "#4E404F"))))
     (yas/field-highlight-face ((t (:background "#2E2E2E"))))

     (org-done ((t (:inherit string :bold t))))
     (org-todo ((t (:inherit constant :bold t))))
     (org-level-1 ((t (:inherit default :underline t :bold t))))
     (org-level-2 ((t (:inherit variable))))
     (org-level-3 ((t (:inherit keyword))))
     (org-level-4 ((t (:inherit type))))
     (org-special-keyword ((t (:inherit doc))))

     (ido-only-match ((t (:inherit string))))
     (ido-subdir ((t (:inherit constant))))

     (gui-element ((t (:background "#D4D0C8" :foreground "black"))))
     (region ((t (:background "#333333"))))
     (minibuffer-prompt ((t (:background "#141414" :foreground "cyan"))))
     (minibuffer-noticeable-prompt ((t (:inherit minibuffer-prompt))))
     (mode-line ((t (:background "#CCCCCC" :foreground "black"))))
     (highlight ((t (:inherit highlighted))))
     (hl-line ((t (:background "#101010"))))
     (left-margin ((t (nil))))
     (text-cursor ((t (:background "yellow" :foreground "black"))))
     (toolbar ((t (nil)))))))
     
(provide 'color-theme-twilight)