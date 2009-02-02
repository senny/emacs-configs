(require 'company-mode)

(defun company-install-bundled-completions-rules ()
  (company-clear-completion-rules)

  (company-install-lisp-completions)
  (company-install-dabbrev-completions)
  (company-install-file-name-completions)
  (eval-after-load 'nxml-mode
    '(company-install-nxml-completions))
  (eval-after-load 'css-mode
    '(company-install-css-completions))
  (eval-after-load 'semantic
    '(progn (require 'semantic-ia)
            (company-install-semantic-completions)))
  (eval-after-load 'oddmuse
    '(company-install-oddmuse-completions))
  )

;;; file names ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun company-file-name-completion-func (prefix)
  (let ((dir (file-name-directory prefix)))
    (ignore-errors
      (mapcar (lambda (file) (concat dir file))
              (remove-if (lambda (file) (or (equal file "../")
                                            (equal file "./")))
                         (file-name-all-completions
                          (file-name-nondirectory prefix) dir))))))

(setq company-file-name-regexp
  "\\(?:\\`\\|[[:space:]\"]\\)\\(~?/[^[:space:]]*\\)\\=")

(defun company-install-file-name-completions ()
  (company-add-completion-rule
   'otherwise
   (cons company-file-name-regexp 1)
   'company-file-name-completion-func
   t))

;;; lisp ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun company-obarray-completion-func (prefix)
  (all-completions prefix obarray))

(defvar company-lisp-symbol-regexp
  "\\_<\\(\\sw\\|\\s_\\)*[a-zA-Z]\\(\\sw\\|\\s_\\)*\\_>\\=")

(defun company-grab-lisp-symbol ()
  (let ((prefix (or (company-grab company-lisp-symbol-regexp) "")))
    (unless (and (company-in-symbol-or-comment (- (point) (length prefix)))
                 (/= (char-before (- (point) (length prefix))) ?`))
      prefix)))

(defun company-install-lisp-completions ()
  (company-add-completion-rule
   'emacs-lisp-mode
   'company-grab-lisp-symbol
   'company-obarray-completion-func))

;;; dabbrev ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun company-dabbrev-completion-func (prefix)
  (require 'dabbrev)
  (let ((dabbrev-check-other-buffers))
    (dabbrev--reset-global-variables)
    (dabbrev--find-all-expansions prefix nil)))

(defun company-grab-dabbrev-prefix ()
  (ignore-errors (dabbrev--abbrev-at-point)))

(defun company-install-dabbrev-completions ()
  (company-add-completion-rule
   'otherwise
   ""
   'company-dabbrev-completion-func)

  (company-add-completion-rule
   'otherwise
   'company-grab-dabbrev-prefix
   'company-dabbrev-completion-func))

;;; nxml-mode ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(eval-after-load 'nxml-mode '(progn

(defun company-nxml-completion-func (prefix)
  (rng-complete-qname-function prefix t t))

(defvar company-xml-tag-regexp "<[^>]*\\([^\"']+\\)\\=")


(require 'rng-nxml)

(defconst company-xml-in-attribute-value-regexp
  (replace-regexp-in-string
   "w"
   xmltok-ncname-regexp
   "<w\\(?::w\\)?\
\\(?:[ \t\r\n]+w\\(?::w\\)?[ \t\r\n]*=\
[ \t\r\n]*\\(?:\"[^\"]*\"\\|'[^']*'\\)\\)*\
[ \t\r\n]+\\(w\\(:w\\)?\\)[ \t\r\n]*=[ \t\r\n]*\
\\(\"\\([^\"]*\\)\\|'\\([^']*\\)\\)\\="
   t t))

;;; end tag

(defconst company-xml-in-end-tag-name-regexp
  (replace-regexp-in-string
   "w"
   xmltok-ncname-regexp
   "</\\(w\\(?::w?\\)?\\)?\\="
   t t))

(defun company-grab-end-tag-name ()
  "Return the open XML end tag before point, if any.
If `nxml-slash-auto-complete-flag' is non-nil, always returns nil, because in
that case we don't want completion."
  (unless nxml-slash-auto-complete-flag
    (company-grab company-xml-in-end-tag-name-regexp 1)))

;;; start tag

(defconst company-xml-in-start-tag-name-regexp
  (replace-regexp-in-string
   "w"
   xmltok-ncname-regexp
   "<\\(w\\(?::w?\\)?\\)?\\="
   t t))

(defun company-rng-completion-func (prefix)
  (let ((company-completions))
    (flet ((insert (&rest args) args)
           (completing-read (a b &optional c d e f g h) nil)
           (rng-complete-before-point (start table prompt &optional pred hist)
            (setq company-completions
                  (all-completions
                   prefix
                   (if (functionp table)
                       (funcall table
                                (buffer-substring-no-properties start (point))
                                nil t)
                     (mapcar 'car table))))
            (car-safe company-completions)))
      (rng-complete))
    company-completions))

(defun company-install-nxml-completions ()
  (require 'rng-nxml)
  ;; attribute="value"
  (company-add-completion-rule
   'nxml-mode
   (cons company-xml-in-attribute-value-regexp 4)
   'company-rng-completion-func)
  ;; attribute='value'
  (company-add-completion-rule
   'nxml-mode
   (cons company-xml-in-attribute-value-regexp 5)
   'company-rng-completion-func)
  ;; start-tag-name
  (company-add-completion-rule
   'nxml-mode
   (cons company-xml-in-start-tag-name-regexp 1)
   'company-rng-completion-func)
  ;; end-tag-name
  (company-add-completion-rule
   'nxml-mode
   'company-grab-end-tag-name
   'company-rng-completion-func)
  ;; attribute
  (company-add-completion-rule
   'nxml-mode
   (cons rng-in-attribute-regex 1)
   'company-rng-completion-func)
  )

))

;;; css ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(eval-after-load 'css-mode '(progn

(defconst company-css-property-alist
  ;; recursive references must come after the definition
  ;; see from http://www.w3.org/TR/CSS21/propidx.html
  '(("azimuth"
     angle "left-side" "far-left" "left" "center-left" "center" "center-right"
     "right" "far-right" "right-side" "behind" "leftwards" "rightwards")
    ("background-attachment"
     "scroll" "fixed")
    ("background-color"
     color "transparent")
    ("background-image"
     uri "none")
    ("background-position"
     percentage length "left" "center" "right" percentage length "top" "center"
     "bottom" "left" "center" "right" "top" "center" "bottom")
    ("background-repeat"
     "repeat" "repeat-x" "repeat-y" "no-repeat")
    ("background"
     background-color background-image background-repeat background-attachment
     background-position)
    ("border-collapse"
     "collapse" "separate")
    ("border-color"
     color "transparent")
    ("border-spacing"
     length length)
    ("border-style"
     border-style)
    ("border-width"
     border-width)
    (("border-top-color" "border-right-color" "border-bottom-color"
      "border-left-color")
     color "transparent")
    (("border-top" "border-right" "border-bottom" "border-left")
     border-width border-style border-top-color)
    (("border-top-style" "border-right-style" "border-bottom-style"
      "border-left-style")
     border-style)
    (("border-top-width" "border-right-width" "border-bottom-width"
      "border-left-width")
     border-width)
    ("border"
    border-width border-style border-top-color)
    ("bottom"
    length percentage "auto")
    ("caption-side"
    "top" "bottom")
    ("clear"
    "none" "left" "right" "both")
    ("clip"
    shape "auto")
    ("color"
    color)
    ("content"
    "normal" "none" string uri counter "attr()" "open-quote" "close-quote"
    "no-open-quote" "no-close-quote")
    ("counter-increment"
    identifier integer "none")
    ("counter-reset"
    identifier integer "none")
    ("cue-after"
    uri "none")
    ("cue-before"
    uri "none")
    ("cue"
    cue-before cue-after)
    ("cursor"
     uri ,"*" "auto" "crosshair" "default" "pointer" "move" "e-resize"
     "ne-resize" "nw-resize" "n-resize" "se-resize" "sw-resize" "s-resize"
     "w-resize" "text" "wait" "help" "progress")
    ("direction"
     "ltr" "rtl")
    ("display"
     "inline" "block" "list-item" "run-in" "inline-block" "table" "inline-table"
     "table-row-group" "table-header-group" "table-footer-group" "table-row"
     "table-column-group" "table-column" "table-cell" "table-caption" "none")
    ("elevation"
     angle "below" "level" "above" "higher" "lower")
    ("empty-cells"
     "show" "hide")
    ("float"
     "left" "right" "none")
    ("font-family"
     family-name generic-family)
    ("font-size"
     absolute-size relative-size length percentage)
    ("font-style"
     "normal" "italic" "oblique")
    ("font-variant"
     "normal" "small-caps")
    ("font-weight"
     "normal" "bold" "bolder" "lighter" "100" "200" "300" "400" "500" "600"
     "700" "800" "900")
    ("font"
     font-style font-variant font-weight font-size "/" line-height font-family
     "caption" "icon" "menu" "message-box" "small-caption" "status-bar")
    ("height"
     length percentage "auto")
    ("left"
     length percentage "auto")
    ("letter-spacing"
     "normal" length)
    ("line-height"
     "normal" number length percentage)
    ("list-style-image"
     uri "none")
    ("list-style-position"
     "inside" "outside")
    ("list-style-type"
     "disc" "circle" "square" "decimal" "decimal-leading-zero" "lower-roman"
     "upper-roman" "lower-greek" "lower-latin" "upper-latin" "armenian"
     "georgian" "lower-alpha" "upper-alpha" "none")
    ("list-style"
     list-style-type list-style-position list-style-image)
    (("margin-right" "margin-left")
     margin-width)
    (("margin-top" "margin-bottom")
     margin-width)
    ("margin"
     margin-width)
    ("max-height"
     length percentage "none")
    ("max-width"
     length percentage "none")
    ("min-height"
     length percentage)
    ("min-width"
     length percentage)
    ("orphans"
     integer)
    ("outline-color"
     color "invert")
    ("outline-style"
     border-style)
    ("outline-width"
     border-width)
    ("outline"
     outline-color outline-style outline-width)
    ("overflow"
     "visible" "hidden" "scroll" "auto")
    (("padding-top" "padding-right" "padding-bottom" "padding-left")
     padding-width)
    ("padding"
     padding-width)
    ("page-break-after"
     "auto" "always" "avoid" "left" "right")
    ("page-break-before"
     "auto" "always" "avoid" "left" "right")
    ("page-break-inside"
     "avoid" "auto")
    ("pause-after"
     time percentage)
    ("pause-before"
     time percentage)
    ("pause"
     time percentage)
    ("pitch-range"
     number)
    ("pitch"
     frequency "x-low" "low" "medium" "high" "x-high")
    ("play-during"
     uri "mix" "repeat" "auto" "none")
    ("position"
     "static" "relative" "absolute" "fixed")
    ("quotes"
     string string "none")
    ("richness"
     number)
    ("right"
     length percentage "auto")
    ("speak-header"
     "once" "always")
    ("speak-numeral"
     "digits" "continuous")
    ("speak-punctuation"
     "code" "none")
    ("speak"
     "normal" "none" "spell-out")
    ("speech-rate"
     number "x-slow" "slow" "medium" "fast" "x-fast" "faster" "slower")
    ("stress"
     number)
    ("table-layout"
     "auto" "fixed")
    ("text-align"
     "left" "right" "center" "justify")
    ("text-decoration"
     "none" "underline" "overline" "line-through" "blink")
    ("text-indent"
     length percentage)
    ("text-transform"
     "capitalize" "uppercase" "lowercase" "none")
    ("top"
     length percentage "auto")
    ("unicode-bidi"
     "normal" "embed" "bidi-override")
    ("vertical-align"
     "baseline" "sub" "super" "top" "text-top" "middle" "bottom" "text-bottom"
     percentage length)
    ("visibility"
    "visible" "hidden" "collapse")
    ("voice-family"
    specific-voice generic-voice ,"*" specific-voice generic-voice)
    ("volume"
    number percentage "silent" "x-soft" "soft" "medium" "loud" "x-loud")
    ("white-space"
    "normal" "pre" "nowrap" "pre-wrap" "pre-line")
    ("widows"
    integer)
    ("width"
    length percentage "auto")
    ("word-spacing"
    "normal" length)
    ("z-index"
    "auto" integer))
  "A list of CSS properties and their possible values.")


(defconst company-css-value-classes
  '((absolute-size
     "xx-small" "x-small" "small" "medium" "large" "x-large" "xx-large")
    (border-style "none" "hidden" "dotted" "dashed" "solid" "double" "groove"
                  "ridge" "inset" "outset")
    (color "aqua" "black" "blue" "fuchsia" "gray" "green" "lime" "maroon" "navy"
           "olive" "orange" "purple" "red" "silver" "teal" "white" "yellow")
    (counter "counter(,)")
    (family-name "Courier" "Helvetica" "Times")
    (generic-family "serif" "sans-serif" "cursive" "fantasy" "monospace")
    (generic-voice "male" "female" "child")
    (margin-width "auto") ;; length percentage
    (relative-size "larger" "smaller")
    (shape "rect(,,,)")
    (uri "url()"))
  "A list of CSS property value classes and their contents.")
;; missing, because not completable
;; <angle><frequency><identifier><integer><length><number><padding-width>
;; <percentage><specific-voice><string><time><uri>

(defconst company-css-property-hash
  (let ((hash (make-hash-table :size 115 :test 'equal)))
    (dolist (pair company-css-property-alist)
      (let ((val (list "inherit")))
        (dolist (el (cdr pair))
          (if (stringp el)
              (unless (member el val)
                (push el val))
            (let ((vals (or (gethash (symbol-name el) hash)
                            (cdr (assoc el company-css-value-classes)))))
              (dolist (e vals)
                (unless (member e val)
                  (push e val))))))
        (setq val (sort* val 'string<))
        (if (listp (car pair))
            (dolist (property (car pair))
              (puthash property val hash))
          (puthash (car pair) val hash))))
      hash)
  "A hash map created from `company-css-property-alist' for fast reference.")

(defconst company-css-html-tags
  '("a" "abbr" "acronym" "address" "applet" "area" "b" "base" "basefont" "bdo"
    "big" "blockquote" "body" "br" "button" "caption" "center" "cite" "code"
    "col" "colgroup" "dd" "del" "dfn" "dir" "div" "dl" "dt" "em" "fieldset"
    "font" "form" "frame" "frameset" "h1" "h2" "h3" "h4" "h5" "h6" "head" "hr"
    "html" "i" "iframe" "img" "input" "ins" "isindex" "kbd" "label" "legend"
    "li" "link" "map" "menu" "meta" "noframes" "noscript" "object" "ol"
    "optgroup" "option" "p" "param" "pre" "q" "s" "samp" "script" "select"
    "small" "span" "strike" "strong" "style" "sub" "sup" "table" "tbody" "td"
    "textarea" "tfoot" "th" "thead" "title" "tr" "tt" "u" "ul" "var")
  "A list of HTML tags for use in CSS completion.")

(defconst company-css-pseudo-classes
  '("active" "after" "before" "first" "first-child" "first-letter" "first-line"
    "focus" "hover" "lang" "left" "link" "right" "visited")
  "Identifiers for CSS pseudo-elements and pseudo-classes.")

;;; bracket detection

(defconst company-css-bracket-syntax-table
  (let ((table (make-syntax-table)))
    (setf (aref table ?{) '(4 . 125))
    (setf (aref table ?}) '(5 . 123))
    table)
  "A syntax table giving { and } paren syntax.")

(defun company-css-inside-braces-p ()
  "Return non-nil, if point is within matched { and }."
  (ignore-errors
    (with-syntax-table company-css-bracket-syntax-table
      (let ((parse-sexp-ignore-comments t))
        (scan-lists (point) -1 1)))))

;;; tags
(defconst company-css-tag-regexp
  (concat "\\(?:\\`\\|}\\)[[:space:]]*"
          ;; multiple
          "\\(?:"
          ;; previous tags:
          "\\(?:#\\|\\_<[[:alpha:]]\\)[[:alnum:]-#]*\\(?:\\[[^]]*\\]\\)?"
          ;; space or selectors
          "\\(?:[[:space:]]+\\|[[:space:]]*[+,>][[:space:]]*\\)"
          "\\)*"
          "\\(\\(?:#\\|\\_<[[:alpha:]]\\)\\(?:[[:alnum:]-#]*\\_>\\)?\\|\\)\\=")
  "A regular expression matching CSS tags")

(defun company-css-complete-tag (prefix)
  "Complete the CSS tag at point starting with PREFIX."
  (all-completions prefix company-css-html-tags))

;;; pseudo id
(defconst company-css-pseudo-regexp
  (concat "\\(?:\\`\\|}\\)[[:space:]]*"
          ;; multiple
          "\\(?:"
          ;; previous tags:
          "\\(?:#\\|\\_<[[:alpha:]]\\)[[:alnum:]-#]*\\(?:\\[[^]]*\\]\\)?"
          ;; space or delimiters
          "\\(?:[[:space:]]+\\|[[:space:]]*[+,>][[:space:]]*\\)"
          "\\)*"
          "\\(?:\\(?:\\#\\|\\_<[[:alpha:]]\\)[[:alnum:]-#]*\\):"
          "\\([[:alpha:]-]+\\_>\\|\\)\\=")
  "A regular expression matching CSS pseudo classes")

(defun company-css-complete-pseudo-class (prefix)
  "Complete the CSS pseudo-class at point starting with PREFIX."
  (all-completions prefix company-css-pseudo-classes))

;;; properties

(defun company-css-grab-property ()
  "Return the CSS property before point, if any.
Returns \"\" if no property found, but feasible at this position."
  (when (company-css-inside-braces-p)
    (or (company-grab "\\_<\\([[:alpha:]-]+\\)\\=" 1) "")))

(defun company-css-complete-property (prefix)
  "Complete the CSS property at point starting with PREFIX."
  (all-completions prefix company-css-property-hash))

;;; values
(defconst company-css-property-value-regexp
  "\\_<\\([[:alpha:]-]+\\):\\(?:[^;]*[[:space:]]+\\)?\\([^;]*\\)\\="
  "A regular expression matching CSS tags")

(defun company-css-complete-property-value (prefix)
  "Complete the CSS property value at point starting with PREFIX."
  (let* ((property (company-grab company-css-property-value-regexp 1))
         (value-list (gethash property company-css-property-hash)))
    (all-completions prefix value-list)))

(defun company-install-css-completions ()
  "Install bundled CSS completion functions."
  (company-add-completion-rule
   'css-mode
   (cons company-css-tag-regexp 1)
   'company-css-complete-tag)

  (company-add-completion-rule
   'css-mode
   (cons company-css-pseudo-regexp 1)
   'company-css-complete-pseudo-class)

  (company-add-completion-rule
   'css-mode
   'company-css-grab-property
   'company-css-complete-property)

  (company-add-completion-rule
   'css-mode
   (cons company-css-property-value-regexp 2)
   'company-css-complete-property-value))

))

;;; semantic ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (semantic-ctxt-current-symbol (point))

;; (defun company-semantic-completion-func (prefix)
;;   "Complete a symbol name by name based on the current context.
;; This is heavily based on `semantic-complete-inline-analyzer'."
;;   (let* ((context (semantic-analyze-current-context))
;;          (collector (semantic-collector-analyze-completions
;; 		     "inline"
;; 		     :buffer (oref context buffer)
;; 		     :context context))
;; 	 (complst nil))
;;     ;; Do a quick calcuation of completions.
;;     (semantic-collector-calculate-completions collector prefix nil)
;;     ;; Get the master list
;;     (setq complst (semanticdb-strip-find-results
;;                    (semantic-collector-all-completions collector prefix)))
;;     ;; Shorten by name
;;     (setq complst (semantic-unique-tag-table-by-name complst))
;;     (mapcar 'semantic-tag-name complst)))

(defun company-semantic-ctxt-current-symbol ()
  "Return the last element of `semantic-ctxt-current-symbol'."
  ;; `semantic-complete-inline-analyzer' returns more than one element, in cases
  ;; like foo->bar
  (or (thing-at-point 'symbol) ""))
;;   (or (car (last (ignore-errors (semantic-ctxt-current-symbol)))) ""))
;;   (let ((symbols (reverse (ignore-errors (semantic-ctxt-current-symbol)))))
;;     (or (car symbols) "")))

;; (defvar company-semantic-context-regexp
;;   "\\(?:\\`\\|\\s \\)\\(\\(?:\\s_\\|\\sw\\|\\.\\|->\\)+\\)\\=")

;; (defun company-grab-semantic-context ()
  ;; We don't actually need a prefix, because `company-semantic-completion-func'
  ;; doesn't need it.  This is just to support conditional completions on prefix
  ;; length
;;   (unless (company-in-symbol-or-comment (point))
;;     (or (company-grab company-semantic-context-regexp 1) "")))

;; semantic-analyze-possible-completions

(defun company-semantic-completion-func (prefix)
  


  ;;   (when (equal prefix "")
;;     (insert "this."))
  (prog1
      (mapcar 'semantic-tag-name
              (ignore-errors
                (or (semantic-ia-get-completions
                     (semantic-analyze-current-context) (point))
                    (senator-find-tag-for-completion (regexp-quote prefix)))))
;;     (when (equal prefix "")
;;       (delete-char -5))
  ))
;; (defun company-semantic-completion-func (prefix)
;;   (when (equal prefix "")
;;     (insert "this->"))
;;   (let ((member (if (string-match ".*\\(\\.\\|->\\)" prefix)
;;                     (match-string 0 prefix)
;;                   ""))
;;         (symbols (ignore-errors
;;                    (or (semantic-ia-get-completions
;;                         (semantic-analyze-current-context) (point))
;;                        (senator-find-tag-for-completion (regexp-quote prefix))))))
;;     (mapcar (lambda (symbol) (concat member symbol))
;;             (mapcar 'semantic-tag-name symbols)))
;;   (when (equal prefix "")
;;     (delete-char -6))
;;   )

(defun company-install-semantic-completions ()
  ;; FIXME: Is there a variable semantic-supported modes
  (dolist (mode '(c++-mode jde-mode java-mode))
    (company-add-completion-rule
     mode
     'company-semantic-ctxt-current-symbol
     'company-semantic-completion-func)))

;; (defun company-semantic-completion-func (prefix)
;;   (let ((member (if (string-match ".*\\(\\.\\|->\\)" prefix)
;;                     (match-string 0 prefix)
;;                   ""))
;;         (symbols (ignore-errors
;;                    (or (semantic-ia-get-completions
;;                         (semantic-analyze-current-context) (point))
;;                        (senator-find-tag-for-completion (regexp-quote prefix))))))
;;     (mapcar (lambda (symbol) (concat member symbol))
;;             (mapcar 'semantic-tag-name symbols))))

;; (defvar company-semantic-context-regexp
;;   "\\(?:\\`\\|\\s \\)\\(\\(?:\\s_\\|\\sw\\|\\.\\|->\\)+\\)\\=")

;; (defun company-grab-semantic-context ()
;;   ;; We don't actually need a prefix, because `company-semantic-completion-func'
;;   ;; doesn't need it.  This is just to support conditional completions on prefix
;;   ;; length
;;   (unless (company-in-symbol-or-comment (point))
;;     (or (company-grab company-semantic-context-regexp 1) "")))

;; (defun company-install-semantic-completions ()
;;   ;; FIXME: Is there a variable semantic-supported modes
;;   (dolist (mode '(c++-mode jde-mode java-mode))
;;     (company-add-completion-rule
;;      mode
;;      'company-grab-semantic-context
;;      'company-semantic-completion-func)))

;;; oddmuse-mode ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun company-oddmuse-page-completion-func (prefix)
  (all-completions prefix
                   (mapcar 'car (oddmuse-make-completion-table oddmuse-wiki))))

(defvar company-oddmuse-link-regexp
  "\\<\\([A-Z]\\|\\[\\[\\)[[:alnum:]]*\\=")

(defun company-install-oddmuse-completions ()
  (company-add-completion-rule
   'oddmuse-mode
   ""
   'company-oddmuse-page-completion-func)

  (company-add-completion-rule
   'oddmuse-mode
   company-oddmuse-link-regexp
   'company-oddmuse-page-completion-func))


(provide 'company-bundled-completions)

