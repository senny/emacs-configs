;; textmate.el --- TextMate minor mode for Emacs

;; Copyright (C) 2008 Chris Wanstrath <chris@ozmm.org>

;; Licensed under the same terms as Emacs.

;; Version: 0.1.0
;; Keywords: textmate osx mac
;; Created: 22 Nov 2008
;; Author: Chris Wanstrath <chris@ozmm.org>

;; This file is NOT part of GNU Emacs.

;; Licensed under the same terms as Emacs.

;;; Commentary:

;; This minor mode exists to mimick TextMate's awesome
;; features.

;;    ⌘T - Go to File
;;  ⇧⌘T - Go to Symbol
;;    ⌘L - Go to Line
;;    ⌘/ - Comment Line (or Selection/Region)
;;    ⌘] - Shift Right
;;    ⌘[ - Shift Left
;;  ⌥⌘] - Align Assignments
;;  ⌥⌘[ - Indent Line
;;  ⌘RET - Insert Newline at Line's End
;;  ⌃C D - Duplicate Line
;;    ⌃K - Delete current line
;;  ⌥⌘T - Reset File Cache (for Go to File, cache unused if using git/hg root)

;; A "project" in textmate-mode is determined by the presence of
;; a .git directory. If no .git directory is found in your current
;; directory, textmate-mode will traverse upwards until one (or none)
;; is found. The directory housing the .git directory is presumed
;; to be the project's root.

;; In other words, calling Go to File from
;; ~/Projects/fieldrunners/app/views/towers/show.html.erb will use
;; ~/Projects/fieldrunners/ as the root if ~/Projects/fieldrunners/.git
;; exists.

;;; Installation

;; $ cd ~/.emacs.d/vendor
;; $ git clone git://github.com/defunkt/textmate.el.git
;;
;; In your emacs config:
;;
;; (add-to-list 'load-path "~/.emacs.d/vendor/textmate.el")
;; (require 'textmate)
;; (textmate-mode)

;;; Depends on imenu
(require 'imenu)

;;; Minor mode

(defvar textmate-use-file-cache t
  "* Should `textmate-goto-file' keep a local cache of files?")

(defvar textmate-completing-library 'ido
  "The library `textmade-goto-symbol' and `textmate-goto-file' should use for completing filenames and symbols (`ido' by default)")

(defvar *textmate-completing-function-alist* '((ido ido-completing-read)
                                               (icicles  icicle-completing-read)
                                               (none completing-read))
  "The function to call to read file names and symbols from the user")

(defvar *textmate-completing-minor-mode-alist*
  `((ido ,(lambda (a) (progn (ido-mode a) (setq ido-enable-flex-matching t))))
    (icicles ,(lambda (a) (icy-mode a)))
    (none ,(lambda (a) ())))
  "The list of functions to enable and disable completing minor modes")

(defvar *textmate-mode-map* (make-sparse-keymap))
(defvar *textmate-project-root* nil)
(defvar *textmate-project-files* '())
(defvar *textmate-gf-exclude*
  "/\\.|vendor|fixtures|tmp|log|build|\\.xcodeproj|\\.nib|\\.framework|\\.app|\\.pbproj|\\.pbxproj|\\.xcode|\\.xcodeproj|\\.bundle")

(defvar *textmate-keybindings-list* `((textmate-next-line
                                     [A-return]    [M-return])
                                     (textmate-clear-cache
                                      ,(kbd "A-M-t") [(control c)(control t)])
                                     (align
                                      ,(kbd "A-M-]") [(control c)(control a)])
                                     (indent-according-to-mode
                                      ,(kbd "A-M-[") nil)
                                     (textmate-shift-right
                                      ,(kbd "A-]")   [(control tab)])
                                     (textmate-shift-left
                                      ,(kbd "A-[")   [(control shift tab)])
                                     (textmate-comment-or-uncomment-region-or-line-or-blank-line
                                      ,(kbd "A-/")   [(control c)(control k)]) ;; FIXME
                                     (textmate-goto-file
                                      ,(kbd "A-t")   [?\A-t])
                                     (textmate-goto-symbol
                                      ,(kbd "A-T")   [?\A-T])
                                     (textmate-duplicate-region-or-line
                                      ,(kbd "C-D")   [(control shift d)])
                                     (textmate-remove-current-line
                                      ,(kbd "C-K")   [(control shift k)])
                                     (upcase-region
                                      ,(kbd "A-u")   [?\A-u])
                                     (downcase-region
                                      ,(kbd "A-U")   [?\A-U])))

(defvar *textmate-project-root-p*
  #'(lambda (coll) (or (member ".git" coll)
                       (member ".hg" coll)
                       (member "prj.el" coll)
                       ))
  "*Lambda that, given a collection of directory entries, returns
  non-nil if it represents the project root.")

(defvar *textmate-find-in-project-default* nil)
(defvar *textmate-find-in-project-type-default* nil)

;;; Bindings

(defun textmate-ido-fix ()
  "Add up/down keybindings for ido."
  (define-key ido-completion-map [up] 'ido-prev-match)
  (define-key ido-completion-map [down] 'ido-next-match))

(defun textmate-bind-keys ()
  (add-hook 'ido-setup-hook 'textmate-ido-fix)

  ; weakness until i figure out how to do this right
  (when (boundp 'osx-key-mode-map)
    (define-key osx-key-mode-map (kbd "A-t") 'textmate-goto-file)
    (define-key osx-key-mode-map (kbd "A-T") 'textmate-goto-symbol))

  (let ((member) (i 0) (access (if (boundp 'aquamacs-version) 'cadr 'caddr)))
    (setq member (nth i *textmate-keybindings-list*))
    (while member
      (if (funcall access member)
       (define-key *textmate-mode-map* (funcall access member) (car member)))
      (setq member (nth i *textmate-keybindings-list*))
      (setq i (+ i 1)))))

(defun textmate-completing-read (&rest args)
  (let ((reading-fn (cadr (assoc textmate-completing-library *textmate-completing-function-alist*))))
  (apply (symbol-function reading-fn) args)))

;;; Commands
(defun comment-or-uncomment-line (&optional lines)
  "Comment current line. Argument gives the number of lines
forward to comment"
  (interactive "P")
  (comment-or-uncomment-region
   (line-beginning-position)
   (line-end-position lines)))

(defun comment-or-uncomment-region-or-line (&optional lines)
  "If the line or region is not a comment, comments region
if mark is active, line otherwise. If the line or region
is a comment, uncomment."
  (interactive "P")
  (if mark-active
      (if (< (mark) (point))
          (comment-or-uncomment-region (mark) (point))
        (comment-or-uncomment-region (point) (mark)))
    (comment-or-uncomment-line lines)))

(defun textmate-comment-or-uncomment-region-or-line-or-blank-line ()
  "If the curent line is blank, add a char, comment line, then delete char"
  (interactive)
  ;; If region is active, carry on
  (if mark-active
      (comment-or-uncomment-region-or-line)
    ;; Otherwise do an ugly hack, add char then delete it
    (if (and (eolp) (bolp))
        (list
         (insert "_")
         (comment-or-uncomment-region-or-line)
         (delete-char -1))
      (comment-or-uncomment-region-or-line))))

;; TextMate-like commenting
;; http://paste.lisp.org/display/42657
(defun comment-or-uncomment-line (&optional lines)
  "Comment current line. Argument gives the number of lines
forward to comment"
  (interactive "P")
  (comment-or-uncomment-region
   (line-beginning-position)
   (line-end-position lines)))
 
(defun comment-or-uncomment-region-or-line (&optional lines)
  "If the line or region is not a comment, comments region
if mark is active, line otherwise. If the line or region
is a comment, uncomment."
  (interactive "P")
  (if mark-active
      (if (< (mark) (point))
          (comment-or-uncomment-region (mark) (point))
  (comment-or-uncomment-region (point) (mark)))
    (comment-or-uncomment-line lines)))
    
(defun textmate-next-line ()
  (interactive)
  (end-of-line)
  (newline-and-indent))

;; http://chopmo.blogspot.com/2008/09/quickly-jumping-to-symbols.html
(defun textmate-goto-symbol ()
  "Will update the imenu index and then use ido to select a symbol to navigate to"
  (interactive)
  (imenu--make-index-alist)
  (let ((name-and-pos '())
        (symbol-names '()))
    (flet ((addsymbols (symbol-list)
                       (when (listp symbol-list)
                         (dolist (symbol symbol-list)
                           (let ((name nil) (position nil))
                             (cond
                              ((and (listp symbol) (imenu--subalist-p symbol))
                               (addsymbols symbol))
                              ((listp symbol)
                               (setq name (car symbol))
                               (setq position (cdr symbol)))

                              ((stringp symbol)
                               (setq name symbol)
                               (setq position (get-text-property 1 'org-imenu-marker symbol))))
                             (unless (or (null position) (null name))
                               (add-to-list 'symbol-names name)
                               (add-to-list 'name-and-pos (cons name position))))))))
      (addsymbols imenu--index-alist))
    (let* ((selected-symbol (textmate-completing-read "Symbol: " symbol-names)))
           (imenu (assoc selected-symbol name-and-pos)))))

;; http://homepages.inf.ed.ac.uk/s0243221/emacs/
(defvar previous-column nil "Save the column position")
(defun textmate-remove-current-line()
  "Kill an entire line, including the trailing newline character"
  (interactive)

  (setq previous-column (current-column))

  (end-of-line)

  (if (= (current-column) 0)
    (delete-char 1)

    (progn
      (beginning-of-line)
      (kill-line)
      (delete-char 1)
      (move-to-column previous-column))))
;;      
(defun textmate-goto-file ()
  (interactive)
  (let ((root (textmate-project-root)))
    (message "FOUND ROOT")
    (when (null root)
      (error "Can't find project root"))
    (find-file
     (concat
      (expand-file-name root) "/"
      (textmate-completing-read
       "Find file: "
       (textmate-project-files root))))))

(defun textmate-find-in-project-type ()
  (interactive)
  (let ((pat (read-string (concat "Suffix"
                                  (if *textmate-find-in-project-type-default*
                                      (format " [\"%s\"]" *textmate-find-in-project-type-default*)
                                    "")
                                  ": "
                                  ) nil nil *textmate-find-in-project-type-default*)))
    (setq *textmate-find-in-project-type-default* pat)
    (textmate-find-in-project (concat "*." pat))))

(defun textmate-find-in-project (&optional pattern)
  (interactive)
  (let ((root (textmate-project-root))
        (default *textmate-find-in-project-default*)
        )
    (message "textmate-find-in-project")
    (when (null root)
      (error "Not in a project area."))
    (let ((re (read-string (concat "Search for "
                         (if (and default (> (length default) 0))
                             (format "[\"%s\"]" default)) ": ")
                 nil 'textmate-find-in-project-history default)
              )
          (incpat (if pattern pattern "*")))
      (append textmate-find-in-project-history (list re))
      (setq *textmate-find-in-project-default* re)
      (let ((type (textmate-project-root-type root)))
        (let ((command
            (cond ((not (string= type "unknown"))
                   (concat "cd "
                           root
                           " ; "
                           (cond ((string= type "git") "git ls-files")
                                 ((string= type "hg") "hg manifest")
                                 ((string= type "ls") "ls"))
                           " | xargs grep -nR "
                           (if pattern (concat " --include='" pattern "' ") "")
                           (shell-quote-argument re)))
                  (t (concat "cd " root "; egrep -nR --exclude='"
                            *textmate-gf-exclude*
                            "' --include='"
                            incpat
                            "' '"
                            re
                            "' . | grep -vE '"
                            *textmate-gf-exclude*
                            "' | sed s:./::"
                            )))))
                  (compilation-start command 'grep-mode))))))

(defun textmate-clear-cache ()
  (interactive)
  (setq *textmate-project-root* nil)
  (setq *textmate-project-files* nil)
  (message "textmate-mode cache cleared."))

(defun textmate-toggle-camel-case ()
  "Toggle current sexp between camelCase and snake_case, like TextMate C-_."
  (interactive)
  (if (thing-at-point 'word)
      (progn
	(unless (looking-at "\\<") (backward-sexp))
	(let ((case-fold-search nil)
	      (start (point))
	      (end (save-excursion (forward-sexp) (point))))
	  (if (and (looking-at "[a-z0-9_]+") (= end (match-end 0))) ; snake-case
	      (progn
		(goto-char start)
		(while (re-search-forward "_[a-z]" end t)
		  (goto-char (1- (point)))
		  (delete-char -1)
		  (upcase-region (point) (1+ (point)))
		  (setq end (1- end))))
	    (downcase-region (point) (1+ (point)))
	    (while (re-search-forward "[A-Z][a-z]" end t)
	      (forward-char -2)
	      (insert "_")
	      (downcase-region (point) (1+ (point)))
	      (forward-char 1)
	      (setq end (1+ end)))
	    (downcase-region start end)
	    )))))

;;; Utilities

(defun textmate-also-ignore (pattern)
  "Also ignore PATTERN in project files."
  (setq *textmate-gf-exclude*
    (concat *textmate-gf-exclude* "|" pattern)))

(defun textmate-project-root-type (root)
  (cond ((member ".git" (directory-files root)) "git")
        ((member ".hg" (directory-files root)) "hg")
        (t "ls")
        (t "unknown")
   ))

(defun textmate-project-files (root)
  (let ((type (textmate-project-root-type root)))
    (message "SEARCHING FILES")
    (cond ((string= type "git") (split-string
                           (shell-command-to-string
                            (concat "cd " root " && git ls-files")) "\n" t))
          ((string= type "hg") (split-string
                                (shell-command-to-string
                           (concat "cd " root " && hg manifest")) "\n" t))
          ((string= type "ls") (split-string
                                (shell-command-to-string
                                 (concat 
                                  "find " root " -type f  | grep -vE '" 
                                  *textmate-gf-exclude* 
                                  "' | sed 's:" 
                                  *textmate-project-root* 
                                  "/::'")) "\n" t))
          ((string= type "unknown") (textmate-cached-project-files-find root)))))

(defun textmate-project-files-find (root)
  (split-string
    (shell-command-to-string
     (concat
      "find "
      root
      " -type f  | grep -vE '"
      *textmate-gf-exclude*
      "' | sed 's:"
      *textmate-project-root*
      "/::'")) "\n" t))

(defun textmate-cached-project-files-find (&optional root)
  (cond
   ((null textmate-use-file-cache) (textmate-project-files root))
   ((equal (textmate-project-root) (car *textmate-project-files*))
    (cdr *textmate-project-files*))
   (t (cdr (setq *textmate-project-files*
                 `(,root . ,(textmate-project-files root)))))))

(defun textmate-project-root ()
  (when (or
         (null *textmate-project-root*)
         (not (string-match *textmate-project-root* default-directory)))
    (let ((root (textmate-find-project-root)))
      (if root
          (setq *textmate-project-root* (expand-file-name (concat root "/")))
        (setq *textmate-project-root* nil))))
  *textmate-project-root*)

(defun textmate-find-project-root (&optional root)
  (when (null root) (setq root default-directory))
  (cond
   ((funcall *textmate-project-root-p* (directory-files root))
    (expand-file-name root))
   ((equal (expand-file-name root) "/") nil)
   (t (textmate-find-project-root (concat (file-name-as-directory root) "..")))))

(defun textmate-shift-left (&optional count)
  "Similar to textmate's unindent.  It reduces the indent by either `tab-width` or as much as it can without reducing the relative indent of the block.

If you really want to remove all indentation including relative indentation, use `indent-rigidly` with a large prefix argument."
  (interactive)
  (let ((deactivate-mark nil)
        (beg (or (and mark-active (region-beginning)) (line-beginning-position)))
        (end (or (and mark-active (region-end)) (line-end-position))))
    (setq min-indentation '())
    (save-excursion
      (goto-char beg)
      (while (< (point) end)
        (if (not (looking-at "[ \t]*$"))
            (add-to-list 'min-indentation (current-indentation)))
        (forward-line)))
    (if (not (< 0 (apply 'min min-indentation)))
        (error "Can't indent any more.  Try `indent-rigidly` with a negative arg."))
    (indent-rigidly beg end (* (or count -1) (min (apply 'min min-indentation) tab-width)))))

(defun textmate-shift-right (&optional arg)
  "Shift the line or region to the ARG places to the right.

A place is considered `tab-width' character columns."
  (interactive)
  (let ((deactivate-mark nil)
        (beg (or (and mark-active (region-beginning))
                 (line-beginning-position)))
        (end (or (and mark-active (region-end)) (line-end-position))))
    (indent-rigidly beg end (* (or arg 1) tab-width))))

(defun textmate-duplicate-region (beginning end)
  (interactive "r")
  (copy-region-as-kill beginning end)
  (yank)
  (back-to-indentation))

(defun textmate-duplicate-region-or-line (beginning end)
  "Checks for a lack of region, otherwise calls duplicate-region"
  (interactive "r")
  (if mark-active
      (textmate-duplicate-region beginning end)
    (list
     (beginning-of-line)
     (kill-line)
     (yank)
     (newline)
     (yank)
     (set-mark (point))
     (end-of-line)
     (back-to-indentation)
     (forward-char))))

(defmacro make-command-to-insert-unicode-char (char name)
  "Creates a function to add a given char"
  `(defun ,(intern (format "textmate-insert-unicode-char-%s" (eval name))) ()
      ,(format "Inserts the character %s" (eval char))
      (interactive)
      (insert ,(eval char))))

(defvar textmate-unicode-pairs 
  '(("⌘" "cmd")
    ("⇧" "shift")
    ("⌥" "alt")
    ("⌃" "ctrl")
    ("⎋" "esc")
    ("⌫" "backspace")
    ("⌦" "forward-delete")
    ("⇆" "tab"))
  "Pairs ofcharacters and their name for easy insertion")

(dolist (pair textmate-unicode-pairs)
  (make-command-to-insert-unicode-char (first pair) (second pair)))

;; auto accept upcase and downcase methods
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)

;; enable skeleton-pair insert globally
;; (setq skeleton-pair t)
;; (setq skeleton-pair-on-word t) ; apply skeleton trick even in front of a word.
;; (global-set-key (kbd "(")  'skeleton-pair-insert-maybe)
;; (global-set-key (kbd "[")  'skeleton-pair-insert-maybe)
;; (global-set-key (kbd "{")  'skeleton-pair-insert-maybe)
;; (global-set-key (kbd "\"") 'skeleton-pair-insert-maybe)
;; (global-set-key (kbd "\'") 'skeleton-pair-insert-maybe)

;;;###autoload
(define-minor-mode textmate-mode "TextMate Emulation Minor Mode"
  :lighter " mate" :global t :keymap *textmate-mode-map*
  (textmate-bind-keys)
  ; activate preferred completion library
  (dolist (mode *textmate-completing-minor-mode-alist*)
    (if (eq (car mode) textmate-completing-library)
        (funcall (cadr mode) t)
      (when (fboundp
             (cadr (assoc (car mode) *textmate-completing-function-alist*)))
        (funcall (cadr mode) -1)))))

(provide 'textmate)
;;; textmate.el ends here
