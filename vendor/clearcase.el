;;; clearcase.el --- ClearCase/Emacs integration.

;; Copyright (C) 1999, 2000, 2001, 2002, 2003, 2004, 2006, 2006, 2007 Kevin Esler

;; Author: Kevin Esler <kaesler@us.ibm.com>
;; Maintainer: Kevin Esler <kaesler@us.ibm.com>
;; Keywords: clearcase tools
;; Web home: http://members.verizon.net/~kevin.a.esler/EmacsClearCase

;; This file is not part of GNU Emacs.
;;
;; This program is free software; you can redistribute it and/or modify it under
;; the terms of the GNU General Public License as published by the Free Software
;; Foundation; either version 2, or (at your option) any later version.

;; This program is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
;; FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
;; details.

;; You should have received a copy of the GNU General Public License along with
;; GNU Emacs; see the file COPYING.  If not, write to the Free Software
;; Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.

;;{{{ Introduction

;; This is a ClearCase/Emacs integration.
;;
;;
;; How to use
;; ==========
;;
;;   0. Make sure you're using Gnu Emacs-20.4 or later or a recent XEmacs.
;;      In general it seems to work better in Gnu Emacs than in XEmacs,
;;      although many XEmacs users have no problems at all with it.
;;
;;   1. Make sure that you DON'T load old versions of vc-hooks.el which contain
;;      incompatible versions of the tq package (functions tq-enqueue and
;;      friends). In particular, Bill Sommerfeld's VC/CC integration has this
;;      problem.
;;
;;   2. Copy the files (or at least the clearcase.elc file) to a directory
;;      on your emacs-load-path.
;;
;;   3. Insert this in your emacs startup file:  (load "clearcase")
;;
;; When you begin editing in any view-context, a ClearCase menu will appear
;; and ClearCase Minor Mode will be activated for you.
;;
;; Summary of features
;; ===================
;;
;;   Keybindings compatible with Emacs' VC (where it makes sense)
;;   Richer interface than VC
;;   Works on NT and Unix
;;   Context sensitive menu (Emacs knows the ClearCase-status of files)
;;   Snapshot view support: update, version comparisons
;;   Can use Emacs Ediff for version comparison display
;;   Dired Mode:
;;     - en masse checkin/out etc
;;     - enhanced display
;;     - browse version tree
;;   Completion of viewnames, version strings
;;   Auto starting of views referenced as /view/TAG/.. (or \\view\TAG\...)
;;   Emacs for editing comments, config specs
;;   Standard ClearCase GUI tools launchable from Emacs menu
;;     - version tree browser
;;     - project browser
;;     - UCM deliver
;;     - UCM rebase
;;   Operations directly available from Emacs menu/keymap:
;;     create-activity
;;     set-activity
;;     mkelem,
;;     checkout
;;     checkin,
;;     unco,
;;     describe
;;     list history
;;     edit config spec
;;     mkbrtype
;;     snapshot view update: file, directory, view
;;     version comparisons using ediff, diff or GUI
;;     find checkouts
;;     annotate version
;;     et al.
;;
;; Acknowledgements
;; ================
;;
;; The help of the following is gratefully acknowledged:
;;
;;   XEmacs support and other bugfixes:
;;
;;     Rod Whitby
;;     Adrian Aichner
;;
;;   This was a result of examining earlier versions of VC and VC/ClearCase
;;   integrations and borrowing freely therefrom.  Accordingly, the following
;;   are ackowledged as contributors:
;;
;;   VC/ClearCase integration authors:
;;
;;     Bill Sommerfeld
;;     Rod Whitby
;;     Andrew Markebo
;;     Andy Eskilsson
;;     Paul Smith
;;     John Kohl
;;     Chris Felaco
;;
;;   VC authors:
;;
;;     Eric S. Raymond
;;     Andre Spiegel
;;     Sebastian Kremer
;;     Richard Stallman
;;     Per Cederqvist
;;     ttn@netcom.com
;;     Andre Spiegel
;;     Jonathan Stigelman
;;     Steve Baur
;;
;;   Other Contributors:
;;
;;     Alastair Rankine
;;     Andrew Maguire
;;     Barnaby Dalton
;;     Christian Savard
;;     David O'Shea
;;     Dee Zsombor
;;     Gabor Zoka
;;     Jason Rumney
;;     Jeff Phillips
;;     Justin Vallon
;;     Mark Collins
;;     Patrik Madison
;;     Ram Bhamidipaty
;;     Reinhard Hahn
;;     Richard Kim
;;     Richard Y. Kim
;;     Simon Graham
;;     Stephen Leake
;;     Steven E. Harris
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;}}}

;;{{{ Version info

(defconst clearcase-version-stamp "ClearCase-version: </main/laptop/166>")
(defconst clearcase-version (substring clearcase-version-stamp 19))

(defun clearcase-maintainer-address ()
  ;; Avoid spam.
  ;;
  (concat "kevin.esler.1989"
          "@"
          "alum.bu.edu"))

(defun clearcase-submit-bug-report ()
  "Submit via mail a bug report on ClearCase Mode"
  (interactive)
  (and (y-or-n-p "Do you really want to submit a report on ClearCase Mode ? ")
       (reporter-submit-bug-report
        (clearcase-maintainer-address)
        (concat "clearcase.el " clearcase-version)
        '(
          system-type
          system-configuration
          emacs-version
          clearcase-clearcase-version-installed
          clearcase-cleartool-path
          clearcase-lt
          clearcase-v3
          clearcase-v4
          clearcase-v5
          clearcase-v6
          clearcase-servers-online
          clearcase-disable-tq
          clearcase-on-cygwin
          clearcase-setview-root
          clearcase-suppress-vc-within-mvfs
          shell-file-name
          w32-quote-process-args
          ))))

;;}}}

;;{{{ Macros

(defmacro clearcase-when-debugging (&rest forms)
  (list 'if 'clearcase-debug (cons 'progn forms)))

(defmacro clearcase-with-tempfile (filename-var &rest forms)
  `(let ((,filename-var (clearcase-utl-tempfile-name)))
     (unwind-protect
         ,@forms

       ;; Cleanup.
       ;;
       (if (file-exists-p ,filename-var)
           (delete-file ,filename-var)))))

;;}}}

;;{{{ Portability

(defvar clearcase-xemacs-p (string-match "XEmacs" emacs-version))

(defvar clearcase-on-mswindows (memq system-type
                                     '(windows-nt ms-windows cygwin cygwin32)))

(defvar clearcase-on-cygwin (memq system-type '(cygwin cygwin32)))

(defvar clearcase-sink-file-name
  (cond
   (clearcase-on-cygwin "/dev/null")
   (clearcase-on-mswindows "NUL")
   (t "/dev/null")))

(defun clearcase-view-mode-quit (buf)
  "Exit from View mode, restoring the previous window configuration."
  (progn
    (cond ((frame-property (selected-frame) 'clearcase-view-window-config)
           (set-window-configuration
            (frame-property (selected-frame) 'clearcase-view-window-config))
           (set-frame-property  (selected-frame) 'clearcase-view-window-config nil))
          ((not (one-window-p))
           (delete-window)))
    (kill-buffer buf)))

(defun clearcase-view-mode (arg &optional camefrom)
  (if clearcase-xemacs-p
      (let* ((winconfig (current-window-configuration))
             (was-one-window (one-window-p))
             (buffer-name (buffer-name (current-buffer)))
             (clearcase-view-not-visible
              (not (and (windows-of-buffer buffer-name) ;shortcut
                        (memq (selected-frame)
                              (mapcar 'window-frame
                                      (windows-of-buffer buffer-name)))))))
        (when clearcase-view-not-visible
          (set-frame-property (selected-frame)
                              'clearcase-view-window-config winconfig))
        (view-mode camefrom 'clearcase-view-mode-quit)
        (setq buffer-read-only nil))
    (view-mode arg)))

(defun clearcase-port-view-buffer-other-window (buffer)
  (if clearcase-xemacs-p
      (switch-to-buffer-other-window buffer)
    (view-buffer-other-window buffer nil 'kill-buffer)))

(defun clearcase-dired-sort-by-date ()
  (if (fboundp 'dired-sort-by-date)
      (dired-sort-by-date)))

;; Copied from emacs-20
;;
(if (not (fboundp 'subst-char-in-string))
    (defun subst-char-in-string (fromchar tochar string &optional inplace)
      "Replace FROMCHAR with TOCHAR in STRING each time it occurs.
Unless optional argument INPLACE is non-nil, return a new string."
      (let ((i (length string))
            (newstr (if inplace string (copy-sequence string))))
        (while (> i 0)
          (setq i (1- i))
          (if (eq (aref newstr i) fromchar)
              (aset newstr i tochar)))
        newstr)))

;;}}}

;;{{{ Require calls

;; nyi: we also use these at the moment:
;;     -view
;;     -ediff
;;     -view
;;     -dired-sort

(require 'cl)
(require 'comint)
(require 'dired)
(require 'easymenu)
(require 'executable)
(require 'reporter)
(require 'ring)
(or clearcase-xemacs-p
    (require 'timer))

;; NT Emacs - doesn't use tq.
;;
(if (not clearcase-on-mswindows)
    (require 'tq))

;;}}}

;;{{{ Debugging facilities

;; Setting this to true will enable some debug code.
;;
(defvar clearcase-debug nil)

(defun clearcase-trace (string)
  (clearcase-when-debugging
   (let ((trace-buf (get-buffer "*clearcase-trace*")))
     (if trace-buf
         (save-excursion
           (set-buffer trace-buf)
           (goto-char (point-max))
           (insert string "\n"))))))

(defun clearcase-enable-tracing ()
  (interactive)
  (setq clearcase-debug t)
  (get-buffer-create "*clearcase-trace*"))

(defun clearcase-disable-tracing ()
  (interactive)
  (setq clearcase-debug nil))

(defun clearcase-dump ()
  (interactive)
  (clearcase-utl-populate-and-view-buffer
   "*clearcase-dump*"
   nil
   (function (lambda ()
               (clearcase-fprop-dump-to-current-buffer)
               (clearcase-vprop-dump-to-current-buffer)))))

(defun clearcase-flush-caches ()
  (interactive)
  (clearcase-fprop-clear-all-properties)
  (clearcase-vprop-clear-all-properties))

;;}}}

;;{{{ Customizable variables

(eval-and-compile
  (condition-case nil
      (require 'custom)
    (error nil))
  (if (and (featurep 'custom)
           (fboundp 'custom-declare-variable))
      nil ;; We've got what we needed
    ;; We have the old custom-library, hack around it!
    (defmacro defgroup (&rest args)
      nil)
    (defmacro defcustom (var value doc &rest args)
      (` (defvar (, var) (, value) (, doc))))
    (defmacro defface (face value doc &rest stuff)
      `(make-face ,face))
    (defmacro custom-declare-variable (symbol value doc &rest args)
      (list 'defvar (eval symbol) value doc))))

(defgroup clearcase () "ClearCase Options" :group 'tools :prefix "clearcase")

(defcustom clearcase-keep-uncheckouts t
  "When true, the contents of an undone checkout will be kept in a file
with a \".keep\" suffix. Otherwise it will be removed."
  :group 'clearcase
  :type 'boolean)

(defcustom clearcase-keep-unhijacks t
  "When true, the contents of an undone hijack will be kept in a file
with a \".keep\" suffix. Otherwise it will be removed."
  :group 'clearcase
  :type 'boolean)

;; nyi: We could also allow a value of 'prompt here
;;
(defcustom clearcase-set-to-new-activity t
  "*If this variable is non-nil when a new activity is created, that activity
will be set as the current activity for the view, otherwise no change is made
to the view's current activity setting."
  :group 'clearcase
  :type 'boolean)

(defcustom clearcase-prompt-for-activity-names t
  "*If this variable is non-nil the user will be prompted for activity names.
Otherwise, activity names will be generated automatically and will typically
have the form \"activity011112.155233\". If the name entered is empty sucn an
internal name will also be generated."
  :group 'clearcase
  :type 'boolean)

(defcustom clearcase-make-backup-files nil
  "*If non-nil, backups of ClearCase files are made as with other files.
If nil (the default), files under ClearCase control don't get backups."
  :group 'clearcase
  :type 'boolean)

(defcustom clearcase-complete-viewtags t
  "*If non-nil, completion on viewtags is enabled. For sites with thousands of view
this should be set to nil."
  :group 'clearcase
  :type 'boolean)

(defcustom clearcase-minimise-menus nil
  "*If non-nil, menus will hide rather than grey-out inapplicable choices."
  :group 'clearcase
  :type 'boolean)

(defcustom clearcase-auto-dired-mode t
  "*If non-nil, automatically enter `clearcase-dired-mode' in dired-mode
for directories in ClearCase."
  :group 'clearcase
  :type 'boolean)

(defcustom clearcase-dired-highlight t
  "If non-nil, highlight reserved files in clearcase-dired buffers."
  :group 'clearcase
  :type 'boolean)

(defcustom clearcase-dired-show-view t
  "If non-nil, show the view tag in dired buffers."
  :group 'clearcase
  :type 'boolean)

(defcustom clearcase-verify-pre-mkelem-dir-checkout nil
  "*If non-nil, prompt before checking out the containing directory
before creating a new ClearCase element."
  :group 'clearcase
  :type 'boolean)

(defcustom clearcase-diff-on-checkin nil
  "Display diff on checkin to help you compose the checkin comment."
  :group 'clearcase
  :type 'boolean)

;; General customization

(defcustom clearcase-suppress-confirm nil
  "If non-nil, treat user as expert; suppress yes-no prompts on some things."
  :group 'clearcase
  :type 'boolean)

(defcustom clearcase-initial-mkelem-comment nil
  "Prompt for initial comment when an element is created."
  :group 'clearcase
  :type 'boolean)

(defcustom clearcase-command-messages nil
  "Display run messages from back-end commands."
  :group 'clearcase
  :type 'boolean)

(defcustom clearcase-checkin-arguments
  ;; For backwards compatibility with old name for this variable:
  ;;
  (if (and (boundp 'clearcase-checkin-switches)
           (not (null clearcase-checkin-switches)))
      (list clearcase-checkin-switches)
    nil)
  "A list of extra arguments passed to the checkin command."
  :group 'clearcase
  :type '(repeat (string :tag "Argument")))

(defcustom clearcase-checkin-on-mkelem nil
  "If t, file will be checked-in when first created as an element."
  :group 'clearcase
  :type 'boolean)

(defcustom clearcase-suppress-checkout-comments nil
  "Suppress prompts for checkout comments for those version control
systems which use them."
  :group 'clearcase
  :type 'boolean)

(defcustom clearcase-checkout-arguments
  ;; For backwards compatibility with old name for this variable:
  ;;
  (if (and (boundp 'clearcase-checkout-arguments)
           (not (null clearcase-checkout-arguments)))
      (list clearcase-checkout-arguments)
    nil)
  "A list of extra arguments passed to the checkout command."
  :group 'clearcase
  :type '(repeat (string :tag "Argument")))

(defcustom clearcase-directory-exclusion-list '("lost+found")
  "Directory names ignored by functions that recursively walk file trees."
  :group 'clearcase
  :type '(repeat (string :tag "Subdirectory")))

(defcustom clearcase-use-normal-diff nil
  "If non-nil, use normal diff instead of cleardiff."
  :group 'clearcase
  :type 'boolean)

(defcustom clearcase-normal-diff-program "diff"
  "*Program to use for generating the differential of the two files
when `clearcase-use-normal-diff' is t."
  :group 'clearcase
  :type 'string)

(defcustom clearcase-normal-diff-arguments
  (if (and (boundp 'clearcase-normal-diff-switches)
           (not (null clearcase-normal-diff-switches)))
      (list clearcase-normal-diff-switches)
    (list "-u"))
  "A list of extra arguments passed to `clearcase-normal-diff-program'
when `clearcase-use-normal-diff' is t.  Usage of the -u switch is
recommended to produce unified diffs, when your
`clearcase-normal-diff-program' supports it."
  :group 'clearcase
  :type '(repeat (string :tag "Argument")))

(defcustom clearcase-vxpath-glue "@@"
  "The string used to construct version-extended pathnames."
  :group 'clearcase
  :type 'string)

(defcustom clearcase-viewroot (if clearcase-on-mswindows
                                  "//view"
                                "/view")
  "The ClearCase viewroot directory."
  :group 'clearcase
  :type 'file)

(defcustom clearcase-viewroot-drive "m:"
  "The ClearCase viewroot drive letter for Windows."
  :group 'clearcase
  :type 'string)

(defcustom clearcase-suppress-vc-within-mvfs t
  "Suppresses VC activity within the MVFS."
  :group 'clearcase
  :type 'boolean)

(defcustom clearcase-hide-rebase-activities t
  "Hide rebase activities from activity selection list."
  :group 'clearcase
  :type 'boolean)

(defcustom clearcase-rebase-id-regexp "^rebase\\."
  "The regexp used to detect rebase actvities."
  :group 'clearcase
  :type 'string)

;;}}}

;;{{{ Global variables

;; Initialize clearcase-pname-sep-regexp according to
;; directory-sep-char.
(defvar clearcase-pname-sep-regexp
  (format "[%s/]"
          (char-to-string directory-sep-char)))

(defvar clearcase-non-pname-sep-regexp
  (format "[^%s/]"
          (char-to-string directory-sep-char)))

;; Matches any viewtag (without the trailing "/").
;;
(defvar clearcase-viewtag-regexp
  (concat "^"
          clearcase-viewroot
          clearcase-pname-sep-regexp
          "\\("
          clearcase-non-pname-sep-regexp "*"
          "\\)"
          "$"
          ))

;; Matches ANY viewroot-relative path
;;
(defvar clearcase-vrpath-regexp
  (concat "^"
          clearcase-viewroot
          clearcase-pname-sep-regexp
          "\\("
          clearcase-non-pname-sep-regexp "*"
          "\\)"
          ))

;;}}}

;;{{{ Minor Mode: ClearCase

;; For ClearCase Minor Mode
;;
(defvar clearcase-mode nil)
(set-default 'clearcase-mode nil)
(make-variable-buffer-local 'clearcase-mode)
(put 'clearcase-mode 'permanent-local t)

;; Tell Emacs about this new kind of minor mode
;;
(if (not (assoc 'clearcase-mode minor-mode-alist))
    (setq minor-mode-alist (cons '(clearcase-mode clearcase-mode)
                                 minor-mode-alist)))

;; For now we override the bindings for VC Minor Mode with ClearCase Minor Mode
;; bindings.
;;
(defvar clearcase-mode-map (make-sparse-keymap))
(defvar clearcase-prefix-map (make-sparse-keymap))
(define-key clearcase-mode-map "\C-xv" clearcase-prefix-map)
(define-key clearcase-mode-map "\C-x\C-q" 'clearcase-toggle-read-only)

(define-key clearcase-prefix-map "b" 'clearcase-browse-vtree-current-buffer)
(define-key clearcase-prefix-map "c" 'clearcase-uncheckout-current-buffer)
(define-key clearcase-prefix-map "e" 'clearcase-edcs-edit)
(define-key clearcase-prefix-map "g" 'clearcase-annotate-current-buffer)
(define-key clearcase-prefix-map "i" 'clearcase-mkelem-current-buffer)
(define-key clearcase-prefix-map "l" 'clearcase-list-history-current-buffer)
(define-key clearcase-prefix-map "m" 'clearcase-mkbrtype)
(define-key clearcase-prefix-map "u" 'clearcase-uncheckout-current-buffer)
(define-key clearcase-prefix-map "v" 'clearcase-next-action-current-buffer)
(define-key clearcase-prefix-map "w" 'clearcase-what-rule-current-buffer)
(define-key clearcase-prefix-map "=" 'clearcase-diff-pred-current-buffer)
(define-key clearcase-prefix-map "?" 'clearcase-describe-current-buffer)
(define-key clearcase-prefix-map "~" 'clearcase-version-other-window)

;; To avoid confusion, we prevent VC Mode from being active at all by
;; undefining its keybindings for which ClearCase Mode doesn't yet have an
;; analogue.
;;
(define-key clearcase-prefix-map "a" 'undefined) ;; vc-update-change-log
(define-key clearcase-prefix-map "d" 'undefined) ;; vc-directory
(define-key clearcase-prefix-map "h" 'undefined) ;; vc-insert-headers
(define-key clearcase-prefix-map "m" 'undefined) ;; vc-merge
(define-key clearcase-prefix-map "r" 'undefined) ;; vc-retrieve-snapshot
(define-key clearcase-prefix-map "s" 'undefined) ;; vc-create-snapshot
(define-key clearcase-prefix-map "t" 'undefined) ;; vc-dired-toggle-terse-mode

;; Associate the map and the minor mode
;;
(or (not (boundp 'minor-mode-map-alist))
    (assq 'clearcase-mode (symbol-value 'minor-mode-map-alist))
    (setq minor-mode-map-alist
          (cons (cons 'clearcase-mode clearcase-mode-map)
                minor-mode-map-alist)))

(defun clearcase-mode (&optional arg)
  "ClearCase Minor Mode"

  (interactive "P")

  ;; Behave like a proper minor-mode.
  ;;
  (setq clearcase-mode
        (if (interactive-p)
            (if (null arg)
                (not clearcase-mode)

              ;; Check if the numeric arg is positive.
              ;;
              (> (prefix-numeric-value arg) 0))

          ;; else
          ;; Use the car if it's a list.
          ;;
          (if (consp arg)
              (setq arg (car arg)))
          (if (symbolp arg)
              (if (null arg)
                  (not clearcase-mode) ;; toggle mode switch
                (not (eq '- arg))) ;; True if symbol is not '-

            ;; else
            ;; assume it's a number and check that.
            ;;
            (> arg 0))))

  (if clearcase-mode
      (easy-menu-add clearcase-menu 'clearcase-mode-map))
  )

;;}}}

;;{{{ Minor Mode: ClearCase Dired

;;{{{ Reformatting the Dired buffer

;; Create a face for highlighting checked out files in clearcase-dired.
;;
(if (not (memq 'clearcase-dired-checkedout-face (face-list)))
    (progn
      (make-face 'clearcase-dired-checkedout-face)
      (set-face-foreground 'clearcase-dired-checkedout-face "red")))

(defun clearcase-dired-insert-viewtag ()
  (save-excursion
    (progn
      (goto-char (point-min))

      ;; Only do this if the buffer is not currently narrowed
      ;;
      (if (= 1 (point))
          (let ((viewtag (clearcase-fprop-viewtag (file-truename default-directory))))
            (if viewtag
                (progn
                  (forward-line 1)
                  (let ((buffer-read-only nil))
                    (insert (format "  [ClearCase View: %s]\n" viewtag))))))))))

(defun clearcase-dired-reformat-buffer ()
  "Reformats the current dired buffer."
  (let* ((checkout-list nil)
         (modified-file-info nil)
         (hijack-list nil)
         (directory default-directory)
         subdir
         fullpath)

    ;; Iterate over each line in the buffer.
    ;;
    ;; Important notes:
    ;;   1. In general, a Dired buffer can contain listings for several
    ;;        directories. We pass though from top to bottom and adjust
    ;;        subdir as we go.
    ;;   2. Since this is called from dired-after-reading-hook, it can get
    ;;      called on a single-line buffer. In this case there is no subdir,
    ;;      and no checkout-list. We need to call clearcase-fprop-checked-out
    ;;      to test for a checkout.
    ;;
    (save-excursion
      (goto-char (point-min))
      (while (not (eobp))
        (cond

         ;; Case 1: Look for directory markers
         ;;
         ((setq subdir (dired-get-subdir))

          ;; We're at a subdirectory line in the dired buffer.
          ;; Go and list all checkouts and hijacks in this subdirectory.
          ;;
          (setq modified-file-info (clearcase-dired-list-modified-files subdir))
          (setq checkout-list (nth 0 modified-file-info))
          (setq hijack-list (nth 1 modified-file-info))

          ;; If no checkouts are found, we don't need to check each file, and
          ;; it's very slow.  The checkout-list should contain something so it
          ;; doesn't attempt to do this.
          ;;
          (if (null checkout-list)
              (setq checkout-list '(nil)))
          (if (null hijack-list)
              (setq hijack-list '(nil)))
          (message "Reformatting %s..." subdir))

         ;; Case 2: Look for files (the safest way to get the filename).
         ;;
         ((setq fullpath (dired-get-filename nil t))

          ;; Expand it to get rid of . and .. entries.
          ;;
          (setq fullpath (expand-file-name fullpath))

	  (setq fullpath (clearcase-path-canonicalise-slashes fullpath))

          ;; Only modify directory listings of the correct format.
          ;; We replace the GID field with a checkout indicator.
          ;;
          (if (looking-at
               ;;     (1)     (2) (3)    (4)
               ;; -rw-rw-rw-   1 esler    5              28 Feb  2 16:02 foo.el
               "..\\([drwxlts-]+ \\) *\\([0-9]+\\) \\([^ ]+\\) *\\([^ ]+ *\\) +[0-9]+\\( [^ 0-9]+ [0-9 ][0-9] .*\\)")

              (let* ((replacement-begin (match-beginning 4))
                     (replacement-end (match-end 4))

                     (replacement-length (- replacement-end replacement-begin))
                     (checkout-replacement-text (format "CHECKOUT"))
                     (hijack-replacement-text (format "HIJACK"))
                     (is-checkout (if checkout-list
                                      (member fullpath checkout-list)
                                    (clearcase-fprop-checked-out fullpath)))
                     (is-hijack (if hijack-list
                                      (member fullpath hijack-list)
                                    (clearcase-fprop-hijacked fullpath))))

                ;; Highlight the line if the file is checked-out.
                ;;
                (if is-checkout
		    (progn
		      ;; Replace the GID field with CHECKOUT.
		      ;;
		      (let ((buffer-read-only nil))
			
			;; Pad with replacement text with trailing spaces if necessary.
			;;
			(if (>= replacement-length (length checkout-replacement-text))
			    (setq checkout-replacement-text
				  (concat checkout-replacement-text
					  (make-string (- replacement-length (length checkout-replacement-text))
						       32))))
			(goto-char replacement-begin)
			(delete-char replacement-length)
			(insert (substring checkout-replacement-text 0 replacement-length)))
		      
		      ;; Highlight the checked out files.
		      ;;
		      (if (fboundp 'put-text-property)
			  (let ((buffer-read-only nil))
			    (put-text-property replacement-begin replacement-end
					       'face 'clearcase-dired-checkedout-face)))
		      )
		  )

                (if is-hijack
		    (progn
		      ;; Replace the GID field with CHECKOUT.
		      ;;
		      (let ((buffer-read-only nil))
			
			;; Pad with replacement text with trailing spaces if necessary.
			;;
			(if (>= replacement-length (length hijack-replacement-text))
			    (setq hijack-replacement-text
				  (concat hijack-replacement-text
					  (make-string (- replacement-length (length hijack-replacement-text))
						       32))))
			(goto-char replacement-begin)
			(delete-char replacement-length)
			(insert (substring hijack-replacement-text 0 replacement-length)))
		      
		      ;; Highlight the checked out files.
		      ;;
		      (if (fboundp 'put-text-property)
			  (let ((buffer-read-only nil))
			    (put-text-property replacement-begin replacement-end
					       'face 'clearcase-dired-checkedout-face)))		
		      )
		  )

                ))))
        (forward-line 1))))
  (message "Reformatting...Done"))


(defun clearcase-path-follow-if-vob-slink (path)
  (if (clearcase-fprop-file-is-vob-slink-p path)

      ;; It's a slink so follow it.
      ;;
      (let ((slink-text (clearcase-fprop-vob-slink-text path)))
        (if (file-name-absolute-p slink-text)
            slink-text
          (concat (file-name-directory path) slink-text)))

    ;; Not an slink.
    ;;
    path))

;;{{{ Searching for modified files

;;{{{ Old code

;; (defun clearcase-dired-list-checkouts (directory)
;;   "Returns a list of files checked-out to the current view in DIRECTORY."

;;   ;; Don't bother looking for checkouts in
;;   ;;  - a history-mode branch-qua-directory
;;   ;;  - a view-private directory
;;   ;;
;;   ;; NYI: For now don't run lsco in root of a snapshot because it gives errors.
;;   ;;      We need to make this smarter.
;;   ;;
;;   ;; NYI: For a pathname which is a slink to a dir, despite the fact that
;;   ;;      clearcase-fprop-file-is-version-p returns true, lsco fails on it,
;;   ;;      with "not an element". Sheesh, surely lsco ought to follow links ?
;;   ;;      Solution: catch the error and check if the dir is a slink then follow
;;   ;;      the link and retry the lsco on the target.
;;   ;;
;;   ;;      For now just ignore the error.
;;   ;;
;;   (if (and (not (clearcase-vxpath-p directory))
;;            (not (eq 'view-private-object (clearcase-fprop-mtype directory)))
;;            (clearcase-fprop-file-is-version-p directory))


;;       (let* ((ignore (message "Listing ClearCase checkouts..."))

;;              (true-dir-path (file-truename directory))

;;              ;; Give the directory as an argument so all names will be
;;              ;; fullpaths. For some reason ClearCase adds an extra slash if you
;;              ;; leave the trailing slash on the directory, so we need to remove
;;              ;; it.
;;              ;;
;;              (native-dir-path (clearcase-path-native (directory-file-name true-dir-path)))

;;              (followed-dir-path (clearcase-path-follow-if-vob-slink native-dir-path))

;;              ;; Form the command:
;;              ;;
;;              (cmd (list
;;                    "lsco" "-cview" "-fmt"
;;                    (if clearcase-on-mswindows
;;                        "%n\\n"
;;                      "'%n\\n'")

;;                    followed-dir-path))

;;              ;; Capture the output:
;;              ;;
;;              (string (clearcase-path-canonicalise-slashes
;;                       (apply 'clearcase-ct-cleartool-cmd cmd)))

;;              ;; Split the output at the newlines:
;;              ;;
;;              (checkout-list (clearcase-utl-split-string-at-char string ?\n)))

;;         ;; Add entries for "." and ".." if they're checked-out.
;;         ;;
;;         (let* ((entry ".")
;;                (path (expand-file-name (concat (file-name-as-directory true-dir-path)
;;                                                entry))))
;;           (if (clearcase-fprop-checked-out path)
;;               (setq checkout-list (cons path checkout-list))))
;;         (let* ((entry "..")
;;                (path (expand-file-name (concat (file-name-as-directory true-dir-path)
;;                                                entry))))
;;           (if (clearcase-fprop-checked-out path)
;;               (setq checkout-list (cons path checkout-list))))

;;         ;; If DIRECTORY is a vob-slink, checkout list will contain pathnames
;;         ;; relative to the vob-slink target rather than to DIRECTORY.  Convert
;;         ;; them back here.  We're making it appear that lsco works on
;;         ;; slinks-to-dirs.
;;         ;;
;;         (if (clearcase-fprop-file-is-vob-slink-p true-dir-path)
;;             (let ((re (regexp-quote (file-name-as-directory followed-dir-path))))
;;               (setq checkout-list
;;                     (mapcar
;;                      (function
;;                       (lambda (path)
;;                         (replace-regexp-in-string re true-dir-path path)))
;;                      checkout-list))))

;;         (message "Listing ClearCase checkouts...done")

;;         ;; Return the result.
;;         ;;
;;         checkout-list)
;;     ))

;; ;; I had believed that this implementation below OUGHT to be faster, having
;; ;; read the code in "ct+lsco". It seemed that "lsco -cview" hit the VOB and
;; ;; listed all checkouts on all elements in the directory, and then filtered by
;; ;; view.  I thought it would probably be quicker to run "ct ls -vob_only" and
;; ;; keep the lines that have "[eclipsed by checkout]".  However this code
;; ;; actually seemed to run slower.  Leave the code here for now so I can test
;; ;; further.
;; ;;
;; (defun clearcase-dired-list-checkouts-experimental (directory)
;;   "Returns a list of files checked-out to the current view in DIRECTORY."

;;   ;; Don't bother looking for checkouts in a history-mode listing
;;   ;; nor in view-private directories.
;;   ;;
;;   (if (and (not (clearcase-vxpath-p directory))
;;            (not (eq 'view-private-object (clearcase-fprop-mtype directory))))

;;       (let* ((ignore (message "Listing ClearCase checkouts..."))

;;              (true-directory (file-truename directory))

;;              ;; Move temporarily to the directory:
;;              ;;
;;              (default-directory true-directory)

;;              ;; Form the command:
;;              ;;
;;              (cmd (list "ls" "-vob_only"))

;;              ;; Capture the output:
;;              ;;
;;              (string (clearcase-path-canonicalise-slashes
;;                       (apply 'clearcase-ct-cleartool-cmd cmd)))

;;              ;; Split the output at the newlines:
;;              ;;
;;              (line-list (clearcase-utl-split-string-at-char string ?\n))

;;              (checkout-list nil))

;;         ;; Look for lines of the form:
;;         ;; FILENAME@@ [eclipsed by checkout]
;;         ;;
;;         (mapcar (function
;;                  (lambda (line)
;;                    (if (string-match "^\\([^ @]+\\)@@ +\\[eclipsed by checkout\\].*" line)
;;                        (setq checkout-list (cons (concat
;;                                                   ;; Add back directory name to get
;;                                                   ;; full pathname.
;;                                                   ;;
;;                                                   default-directory
;;                                                   (substring line
;;                                                              (match-beginning 1)
;;                                                              (match-end 1)))
;;                                                  checkout-list)))))
;;                 line-list)

;;         ;; Add entries for "." and ".." if they're checked-out.
;;         ;;
;;         (let* ((entry ".")
;;                (path (expand-file-name (concat true-directory entry))))
;;           (if (clearcase-fprop-checked-out path)
;;               (setq checkout-list (cons path checkout-list))))
;;         (let* ((entry "..")
;;                (path (expand-file-name (concat true-directory entry))))
;;           (if (clearcase-fprop-checked-out path)
;;               (setq checkout-list (cons path checkout-list))))

;;         (message "Listing ClearCase checkouts...done")

;;         ;; Return the result.
;;         ;;
;;         checkout-list)))

;; (defun clearcase-dired-list-hijacks (directory)
;;   "Returns a list of files hijacked to the current view in DIRECTORY."

;;   ;; Don't bother looking for hijacks in;
;;   ;;   - a history-mode listing
;;   ;;   - a in view-private directory
;;   ;;   - a dynamic view
;;   ;;
;;   (let* ((true-directory (file-truename directory))
;;          (viewtag (clearcase-fprop-viewtag true-directory)))

;;     (if (and viewtag
;;              (not (clearcase-vxpath-p directory))
;;              (not (eq 'view-private-object (clearcase-fprop-mtype directory)))
;;              (clearcase-file-would-be-in-snapshot-p true-directory))

;;         (let* ((ignore (message "Listing ClearCase hijacks..."))

;;                (true-directory (file-truename directory))

;;                ;; Form the command:
;;                ;;
;;                (cmd (list
;;                      "ls"

;;                      ;; Give the directory as an argument so all names will be
;;                      ;; fullpaths. For some reason ClearCase adds an extra slash
;;                      ;; if you leave the trailing slash on the directory, so we
;;                      ;; need to remove it.
;;                      ;;
;;                      (clearcase-path-native (directory-file-name true-directory))))

;;                ;; Capture the output:
;;                ;;
;;                (string (clearcase-path-canonicalise-slashes
;;                         (apply 'clearcase-ct-cleartool-cmd cmd)))

;;                ;; Split the output at the newlines:
;;                ;;
;;                (line-list (clearcase-utl-split-string-at-char string ?\n))

;;                (hijack-list nil))

;;           (mapcar (function
;;                    (lambda (line)
;;                      (if (string-match "^\\([^ @]+\\)@@[^ ]+ \\[hijacked\\].*" line)
;;                          (setq hijack-list (cons (substring line
;;                                                             (match-beginning 1)
;;                                                             (match-end 1))
;;                                                  hijack-list)))))
;;                   line-list)

;;           (message "Listing ClearCase hijacks...done")

;;           ;; Return the result.
;;           ;;
;;           hijack-list))))

;;}}}

(defun clearcase-dired-list-modified-files (directory)
  "Returns a pair of lists of files (checkouts . hijacks) to the current view in DIRECTORY."

  ;; Don't bother looking for hijacks in;
  ;;   - a history-mode listing
  ;;   - a in view-private directory
  ;;   - a dynamic view
  ;;
  (let* ((true-directory (file-truename directory))
         (viewtag (clearcase-fprop-viewtag true-directory))
         (snapshot (clearcase-file-would-be-in-snapshot-p true-directory))
         (result '(() ())))

    (if (and viewtag
             (not (clearcase-vxpath-p directory))
             (not (eq 'view-private-object (clearcase-fprop-mtype directory))))

        (let* ((ignore (message "Listing ClearCase modified files..."))

               (true-directory (file-truename directory))

               ;; Form the command:
               ;;
               (cmd (list
                     "ls"

                     ;; Give the directory as an argument so all names will be
                     ;; fullpaths. For some reason ClearCase adds an extra slash
                     ;; if you leave the trailing slash on the directory, so we
                     ;; need to remove it.
                     ;;
                     (clearcase-path-native (directory-file-name true-directory))))

               ;; Capture the output:
               ;;
               (string (clearcase-path-canonicalise-slashes
                        (apply 'clearcase-ct-cleartool-cmd cmd)))

               ;; Split the output at the newlines:
               ;;
               (line-list (clearcase-utl-split-string-at-char string ?\n))

               (hijack-list nil)
               (checkout-list nil))

          (mapcar (function
                   (lambda (line)
                     (if (string-match "^\\([^ @]+\\)@@[^ ]+ \\[hijacked\\].*" line)
                         (setq hijack-list (cons (substring line
                                                            (match-beginning 1)
                                                            (match-end 1))
                                                 hijack-list)))
                     (if (string-match "^\\([^ @]+\\)@@.+CHECKEDOUT from .*" line)
                         (setq checkout-list (cons (substring line
                                                              (match-beginning 1)
                                                              (match-end 1))
                                                   checkout-list)))))
                  line-list)

          (message "Listing ClearCase modified files...done")

          ;; Return the result.
          ;;
          (setq result (list checkout-list hijack-list))))
    result))

;;}}}

;;}}}

;; For ClearCase Dired Minor Mode
;;
(defvar clearcase-dired-mode nil)
(set-default 'clearcase-dired-mode nil)
(make-variable-buffer-local 'clearcase-dired-mode)

;; Tell Emacs about this new kind of minor mode
;;
(if (not (assoc 'clearcase-dired-mode minor-mode-alist))
    (setq minor-mode-alist (cons '(clearcase-dired-mode clearcase-dired-mode)
                                 minor-mode-alist)))

;; For now we override the bindings for VC Minor Mode with ClearCase Dired
;; Minor Mode bindings.
;;
(defvar clearcase-dired-mode-map (make-sparse-keymap))
(defvar clearcase-dired-prefix-map (make-sparse-keymap))
(define-key clearcase-dired-mode-map "\C-xv" clearcase-dired-prefix-map)

(define-key clearcase-dired-prefix-map "b" 'clearcase-browse-vtree-dired-file)
(define-key clearcase-dired-prefix-map "c" 'clearcase-uncheckout-dired-files)
(define-key clearcase-dired-prefix-map "e" 'clearcase-edcs-edit)
(define-key clearcase-dired-prefix-map "i" 'clearcase-mkelem-dired-files)
(define-key clearcase-dired-prefix-map "g" 'clearcase-annotate-dired-file)
(define-key clearcase-dired-prefix-map "l" 'clearcase-list-history-dired-file)
(define-key clearcase-dired-prefix-map "m" 'clearcase-mkbrtype)
(define-key clearcase-dired-prefix-map "u" 'clearcase-uncheckout-dired-files)
(define-key clearcase-dired-prefix-map "v" 'clearcase-next-action-dired-files)
(define-key clearcase-dired-prefix-map "w" 'clearcase-what-rule-dired-file)
(define-key clearcase-dired-prefix-map "=" 'clearcase-diff-pred-dired-file)
(define-key clearcase-dired-prefix-map "~" 'clearcase-version-other-window)
(define-key clearcase-dired-prefix-map "?" 'clearcase-describe-dired-file)

;; To avoid confusion, we prevent VC Mode from being active at all by
;; undefining its keybindings for which ClearCase Mode doesn't yet have an
;; analogue.
;;
(define-key clearcase-dired-prefix-map "a" 'undefined) ;; vc-update-change-log
(define-key clearcase-dired-prefix-map "d" 'undefined) ;; vc-directory
(define-key clearcase-dired-prefix-map "h" 'undefined) ;; vc-insert-headers
(define-key clearcase-dired-prefix-map "m" 'undefined) ;; vc-merge
(define-key clearcase-dired-prefix-map "r" 'undefined) ;; vc-retrieve-snapshot
(define-key clearcase-dired-prefix-map "s" 'undefined) ;; vc-create-snapshot
(define-key clearcase-dired-prefix-map "t" 'undefined) ;; vc-dired-toggle-terse-mode

;; Associate the map and the minor mode
;;
(or (not (boundp 'minor-mode-map-alist))
    (assq 'clearcase-dired-mode (symbol-value 'minor-mode-map-alist))
    (setq minor-mode-map-alist
          (cons (cons 'clearcase-dired-mode clearcase-dired-mode-map)
                minor-mode-map-alist)))

(defun clearcase-dired-mode (&optional arg)
  "The augmented Dired minor mode used in ClearCase directory buffers.
All Dired commands operate normally.  Users with checked-out files
are listed in place of the file's owner and group. Keystrokes bound to
ClearCase Mode commands will execute as though they had been called
on a buffer attached to the file named in the current Dired buffer line."

  (interactive "P")

  ;; Behave like a proper minor-mode.
  ;;
  (setq clearcase-dired-mode
        (if (interactive-p)
            (if (null arg)
                (not clearcase-dired-mode)

              ;; Check if the numeric arg is positive.
              ;;
              (> (prefix-numeric-value arg) 0))

          ;; else
          ;; Use the car if it's a list.
          ;;
          (if (consp arg)
              (setq arg (car arg)))

          (if (symbolp arg)
              (if (null arg)
                  (not clearcase-dired-mode) ;; toggle mode switch
                (not (eq '- arg))) ;; True if symbol is not '-

            ;; else
            ;; assume it's a number and check that.
            ;;
            (> arg 0))))

  (if (not (eq major-mode 'dired-mode))
      (setq clearcase-dired-mode nil))

  (if (and clearcase-dired-mode clearcase-dired-highlight)
      (clearcase-dired-reformat-buffer))

  (if clearcase-dired-mode
      (easy-menu-add clearcase-dired-menu 'clearcase-dired-mode-map))
  )

;;}}}

;;{{{ Major Mode: for editing comments.

;; The major mode function.
;;
(defun clearcase-comment-mode ()
  "Major mode for editing comments for ClearCase.

These bindings are added to the global keymap when you enter this mode:

\\[clearcase-next-action-current-buffer]  perform next logical version-control operation on current file
\\[clearcase-mkelem-current-buffer]       mkelem the current file
\\[clearcase-toggle-read-only]            like next-action, but won't create elements
\\[clearcase-list-history-current-buffer] display change history of current file
\\[clearcase-uncheckout-current-buffer]   cancel checkout in buffer
\\[clearcase-diff-pred-current-buffer]    show diffs between file versions
\\[clearcase-version-other-window]        visit old version in another window

While you are entering a comment for a version, the following
additional bindings will be in effect.

\\[clearcase-comment-finish]           proceed with check in, ending comment

Whenever you do a checkin, your comment is added to a ring of
saved comments.  These can be recalled as follows:

\\[clearcase-comment-next]             replace region with next message in comment ring
\\[clearcase-comment-previous]         replace region with previous message in comment ring
\\[clearcase-comment-search-reverse]   search backward for regexp in the comment ring
\\[clearcase-comment-search-forward]   search backward for regexp in the comment ring

Entry to the clearcase-comment-mode calls the value of text-mode-hook, then
the value of clearcase-comment-mode-hook.

Global user options:
 clearcase-initial-mkelem-comment      If non-nil, require user to enter a change
                                   comment upon first checkin of the file.

 clearcase-suppress-confirm     Suppresses some confirmation prompts,
                            notably for reversions.

 clearcase-command-messages     If non-nil, display run messages from the
                            actual version-control utilities (this is
                            intended primarily for people hacking clearcase.el
                            itself).
"
  (interactive)

  ;; Major modes are supposed to just (kill-all-local-variables)
  ;; but we rely on clearcase-parent-buffer already having been set
  ;;
  ;;(let ((parent clearcase-parent-buffer))
  ;;  (kill-all-local-variables)
  ;;  (set (make-local-variable 'clearcase-parent-buffer) parent))

  (setq major-mode 'clearcase-comment-mode)
  (setq mode-name "ClearCase/Comment")

  (set-syntax-table text-mode-syntax-table)
  (use-local-map clearcase-comment-mode-map)
  (setq local-abbrev-table text-mode-abbrev-table)

  (make-local-variable 'clearcase-comment-operands)
  (make-local-variable 'clearcase-comment-ring-index)

  (set-buffer-modified-p nil)
  (setq buffer-file-name nil)
  (run-hooks 'text-mode-hook 'clearcase-comment-mode-hook))

;; The keymap.
;;
(defvar clearcase-comment-mode-map nil)
(if clearcase-comment-mode-map
    nil
  (setq clearcase-comment-mode-map (make-sparse-keymap))
  (define-key clearcase-comment-mode-map "\M-n" 'clearcase-comment-next)
  (define-key clearcase-comment-mode-map "\M-p" 'clearcase-comment-previous)
  (define-key clearcase-comment-mode-map "\M-r" 'clearcase-comment-search-reverse)
  (define-key clearcase-comment-mode-map "\M-s" 'clearcase-comment-search-forward)
  (define-key clearcase-comment-mode-map "\C-c\C-c" 'clearcase-comment-finish)
  (define-key clearcase-comment-mode-map "\C-x\C-s" 'clearcase-comment-save)
  (define-key clearcase-comment-mode-map "\C-x\C-q" 'clearcase-comment-num-num-error))

;; Constants.
;;
(defconst clearcase-comment-maximum-ring-size 32
  "Maximum number of saved comments in the comment ring.")

;; Variables.
;;
(defvar clearcase-comment-entry-mode nil)
(defvar clearcase-comment-operation nil)
(defvar clearcase-comment-operands)
(defvar clearcase-comment-ring nil)
(defvar clearcase-comment-ring-index nil)
(defvar clearcase-comment-last-match nil)
(defvar clearcase-comment-window-config nil)

;; In several contexts, this is a local variable that points to the buffer for
;; which it was made (either a file, or a ClearCase dired buffer).
;;
(defvar clearcase-parent-buffer nil)
(defvar clearcase-parent-buffer-name nil)

;;{{{ Commands and functions

(defun clearcase-comment-start-entry (uniquifier
                                      prompt
                                      continuation
                                      operands
                                      &optional parent-buffer comment-seed)

  "Accept a comment by popping up a clearcase-comment-mode buffer
with a name derived from UNIQUIFIER, and emitting PROMPT in the minibuffer.
Set the continuation on close to CONTINUATION, which should be apply-ed to a list
formed by appending OPERANDS and the comment-string.

Optional 5th argument specifies a PARENT-BUFFER to return to when the operation
is complete.

Optional 6th argument specifies a COMMENT-SEED to insert in the comment buffer for
the user to edit."

  (let ((comment-buffer (get-buffer-create (format "*clearcase-comment-%s*" uniquifier)))
        (old-window-config (current-window-configuration))
        (parent (or parent-buffer
                    (current-buffer))))
    (pop-to-buffer comment-buffer)

    ;; Record in buffer-local variables information sufficient to restore
    ;; window context.
    ;;
    (set (make-local-variable 'clearcase-comment-window-config) old-window-config)
    (set (make-local-variable 'clearcase-parent-buffer) parent)

    (clearcase-comment-mode)
    (setq clearcase-comment-operation continuation)
    (setq clearcase-comment-operands operands)
    (if comment-seed
        (insert comment-seed))
    (message "%s  Type C-c C-c when done." prompt)))


(defun clearcase-comment-cleanup ()
  ;; Make sure it ends with newline
  ;;
  (goto-char (point-max))
  (if (not (bolp))
      (newline))

  ;; Remove useless whitespace.
  ;;
  (goto-char (point-min))
  (while (re-search-forward "[ \t]+$" nil t)
    (replace-match ""))

  ;; Remove trailing newlines, whitespace.
  ;;
  (goto-char (point-max))
  (skip-chars-backward " \n\t")
  (delete-region (point) (point-max)))

(defun clearcase-comment-finish ()
  "Complete the operation implied by the current comment."
  (interactive)

  ;;Clean and record the comment in the ring.
  ;;
  (let ((comment-buffer (current-buffer)))
    (clearcase-comment-cleanup)

    (if (null clearcase-comment-ring)
        (setq clearcase-comment-ring (make-ring clearcase-comment-maximum-ring-size)))
    (ring-insert clearcase-comment-ring (buffer-string))

    ;; Perform the operation on the operands.
    ;;
    (if clearcase-comment-operation
        (save-excursion
          (apply clearcase-comment-operation
                 (append clearcase-comment-operands (list (buffer-string)))))
      (error "No comment operation is pending"))

    ;; Return to "parent" buffer of this operation.
    ;; Remove comment window.
    ;;
    (let ((old-window-config clearcase-comment-window-config))
      (pop-to-buffer clearcase-parent-buffer)
      (delete-windows-on comment-buffer)
      (kill-buffer comment-buffer)
      (if old-window-config (set-window-configuration old-window-config)))))

(defun clearcase-comment-save-comment-for-buffer (comment buffer)
  (save-excursion
    (set-buffer buffer)
    (let ((file (buffer-file-name)))
      (if (clearcase-fprop-checked-out file)
          (progn
            (clearcase-ct-do-cleartool-command "chevent"
                                               file
                                               comment
                                               (list "-replace"))
            (clearcase-fprop-set-comment file comment))
        (error "Can't change comment of checked-in version with this interface")))))

(defun clearcase-comment-save ()
  "Save the currently entered comment"
  (interactive)
  (let ((comment-string (buffer-string))
        (parent-buffer clearcase-parent-buffer))
    (if (not (buffer-modified-p))
        (message "(No changes need to be saved)")
      (progn
        (save-excursion
          (set-buffer parent-buffer)
          (clearcase-comment-save-comment-for-buffer comment-string parent-buffer))

        (set-buffer-modified-p nil)))))

(defun clearcase-comment-num-num-error ()
  (interactive)
  (message "Perhaps you wanted to type C-c C-c instead?"))

;; Code for the comment ring.
;;
(defun clearcase-comment-next (arg)
  "Cycle forwards through comment history."
  (interactive "*p")
  (clearcase-comment-previous (- arg)))

(defun clearcase-comment-previous (arg)
  "Cycle backwards through comment history."
  (interactive "*p")
  (let ((len (ring-length clearcase-comment-ring)))
    (cond ((or (not len) (<= len 0))
           (message "Empty comment ring")
           (ding))
          (t
           (erase-buffer)

           ;; Initialize the index on the first use of this command so that the
           ;; first M-p gets index 0, and the first M-n gets index -1.
           ;;
           (if (null clearcase-comment-ring-index)
               (setq clearcase-comment-ring-index
                     (if (> arg 0) -1
                       (if (< arg 0) 1 0))))
           (setq clearcase-comment-ring-index
                 (mod (+ clearcase-comment-ring-index arg) len))
           (message "%d" (1+ clearcase-comment-ring-index))
           (insert (ring-ref clearcase-comment-ring clearcase-comment-ring-index))))))

(defun clearcase-comment-search-forward (str)
  "Searches forwards through comment history for substring match."
  (interactive "sComment substring: ")
  (if (string= str "")
      (setq str clearcase-comment-last-match)
    (setq clearcase-comment-last-match str))
  (if (null clearcase-comment-ring-index)
      (setq clearcase-comment-ring-index 0))
  (let ((str (regexp-quote str))
        (n clearcase-comment-ring-index))
    (while (and (>= n 0) (not (string-match str (ring-ref clearcase-comment-ring n))))
      (setq n (- n 1)))
    (cond ((>= n 0)
           (clearcase-comment-next (- n clearcase-comment-ring-index)))
          (t (error "Not found")))))

(defun clearcase-comment-search-reverse (str)
  "Searches backwards through comment history for substring match."
  (interactive "sComment substring: ")
  (if (string= str "")
      (setq str clearcase-comment-last-match)
    (setq clearcase-comment-last-match str))
  (if (null clearcase-comment-ring-index)
      (setq clearcase-comment-ring-index -1))
  (let ((str (regexp-quote str))
        (len (ring-length clearcase-comment-ring))
        (n (1+ clearcase-comment-ring-index)))
    (while (and (< n len)
                (not (string-match str (ring-ref clearcase-comment-ring n))))
      (setq n (+ n 1)))
    (cond ((< n len)
           (clearcase-comment-previous (- n clearcase-comment-ring-index)))
          (t (error "Not found")))))

;;}}}

;;}}}

;;{{{ Major Mode: for editing config-specs.

;; The major mode function.
;;
(defun clearcase-edcs-mode ()
  (interactive)
  (set-syntax-table text-mode-syntax-table)
  (use-local-map clearcase-edcs-mode-map)
  (setq major-mode 'clearcase-edcs-mode)
  (setq mode-name "ClearCase/edcs")
  (make-variable-buffer-local 'clearcase-parent-buffer)
  (set-buffer-modified-p nil)
  (setq buffer-file-name nil)
  (run-hooks 'text-mode-hook 'clearcase-edcs-mode-hook))

;; The keymap.
;;
(defvar clearcase-edcs-mode-map nil)
(if clearcase-edcs-mode-map
    nil
  (setq clearcase-edcs-mode-map (make-sparse-keymap))
  (define-key clearcase-edcs-mode-map "\C-c\C-c" 'clearcase-edcs-finish)
  (define-key clearcase-edcs-mode-map "\C-x\C-s" 'clearcase-edcs-save))

;; Variables.
;;
(defvar clearcase-edcs-tag-name nil
  "Name of view tag which is currently being edited")

(defvar clearcase-edcs-tag-history ()
  "History of view tags used in clearcase-edcs-edit")

;;{{{ Commands

(defun clearcase-edcs-edit (tag-name)
  "Edit a ClearCase configuration specification"

  (interactive
   (let ((vxname (clearcase-fprop-viewtag default-directory)))
     (if clearcase-complete-viewtags
         (list (directory-file-name
                (completing-read "View Tag: "
                                 (clearcase-viewtag-all-viewtags-obarray)
                                 nil
                                 ;;'fascist
                                 nil
                                 vxname
                                 'clearcase-edcs-tag-history)))
       (read-string "View Tag: "))))

  (let ((start (current-buffer))
        (buffer-name (format "*clearcase-config-spec-%s*" tag-name)))
    (kill-buffer (get-buffer-create buffer-name))
    (pop-to-buffer (get-buffer-create buffer-name))
    (auto-save-mode auto-save-default)
    (erase-buffer)
    (insert (clearcase-ct-cleartool-cmd "catcs" "-tag" tag-name))
    (goto-char (point-min))
    (re-search-forward "^[^#\n]" nil 'end)
    (beginning-of-line)
    (clearcase-edcs-mode)
    (setq clearcase-parent-buffer start)
    (make-local-variable 'clearcase-edcs-tag-name)
    (setq clearcase-edcs-tag-name tag-name)))

(defun clearcase-edcs-save ()
  (interactive)
  (if (not (buffer-modified-p))
      (message "Configuration not changed since last saved")

    (message "Setting configuration for %s..." clearcase-edcs-tag-name)
    (clearcase-with-tempfile
     cspec-text
     (write-region (point-min) (point-max) cspec-text nil 'dont-mention-it)
     (let ((ret (clearcase-ct-cleartool-cmd "setcs"
                                            "-tag"
                                            clearcase-edcs-tag-name
                                            (clearcase-path-native cspec-text))))

       ;; nyi: we could be smarter and retain viewtag info and perhaps some
       ;;      other info. For now invalidate all cached file property info.
       ;;
       (clearcase-fprop-clear-all-properties)

       (set-buffer-modified-p nil)
       (message "Setting configuration for %s...done"
                clearcase-edcs-tag-name)))))

(defun clearcase-edcs-finish ()
  (interactive)
  (let ((old-buffer (current-buffer)))
    (clearcase-edcs-save)
    (bury-buffer nil)
    (kill-buffer old-buffer)))

;;}}}

;;}}}

;;{{{ View browser

;; nyi: Just an idea now.
;;      Be able to present a selection of views at various times
;;        - show me current file in other view
;;        - top-level browse operation

;;  clearcase-viewtag-started-viewtags gives us the dynamic views that are mounted.

;;  How to find local snapshots ?

;; How to find drive-letter mount points for view on NT ?
;;  - parse "subst" output

;;}}}

;;{{{ Commands

;;{{{ Hijack/unhijack

(defun clearcase-hijack-current-buffer ()
  "Hijack the file in the current buffer."
  (interactive)
  (clearcase-hijack buffer-file-name))

(defun clearcase-hijack-dired-files ()
  "Hijack the selected files."
  (interactive)
  (clearcase-hijack-seq (dired-get-marked-files)))

(defun clearcase-unhijack-current-buffer ()
  "Unhijack the file in the current buffer."
  (interactive)
  (clearcase-unhijack buffer-file-name))

(defun clearcase-unhijack-dired-files ()
  "Hijack the selected files."
  (interactive)
  (clearcase-unhijack-seq (dired-get-marked-files)))

;;}}}

;;{{{ Annotate

(defun clearcase-annotate-file (file)
  (let ((relative-name (file-relative-name file)))
    (message "Annotating %s ..." relative-name)
    (clearcase-with-tempfile
     annotation-file
     (clearcase-ct-do-cleartool-command "annotate"
                                        file
                                        'unused
                                        (list "-nco"
                                              "-out"
                                              annotation-file))
     (clearcase-utl-populate-and-view-buffer
      "*clearcase-annotate*"
      nil
      (function
       (lambda ()
         (insert-file-contents annotation-file)))))
    (message "Annotating %s ...done" relative-name)))

(defun clearcase-annotate-current-buffer ()
  (interactive)
  (clearcase-annotate-file buffer-file-name))

(defun clearcase-annotate-dired-file ()
  "Annotate the selected file."
  (interactive)
  (clearcase-annotate-file (dired-get-filename)))

;;}}}

;;{{{ nyi: Find checkouts

;; NYI: Enhance this:
;;  - group by:
;;    - activity name
;;    - checkout comment
;;  - permit unco/checkin
;;
(defun clearcase-find-checkouts-in-current-view ()
  "Find the checkouts in all vobs in the current view."
  (interactive)
  (let ((viewtag (clearcase-fprop-viewtag default-directory))
        (dir default-directory))
    (if viewtag
        (let* ((ignore (message "Finding checkouts..."))
               (text (clearcase-ct-blocking-call "lsco"
                                                 "-cview"
                                                 "-avobs"
                                                 "-short")))
          (if (zerop (length text))
              (message "No checkouts found")
            (progn
              (message "Finding checkouts...done")

              (clearcase-utl-populate-and-view-buffer
               "*clearcase*"
               (list text)
               (function (lambda (s)
                           (insert s))))))))))

;;}}}

;;{{{ UCM operations

;;{{{ Make activity

(defun clearcase-read-new-activity-name ()
  "Read the name of a new activity from the minibuffer.
Return nil if the empty string is entered."

  ;; nyi: Probably should check that the activity doesn't already exist.
  ;;
  (let ((entered-name (read-string "Activity name (optional): " )))
    (if (not (zerop (length entered-name)))
        entered-name
      nil)))

(defun clearcase-read-mkact-args ()
  "Read the name and headline arguments for clearcase-ucm-mkact-current-dir
from the minibuffer."

  (let ((name nil)
        (headline ""))
    (if clearcase-prompt-for-activity-names
        (setq name (clearcase-read-new-activity-name)))
    (setq headline (read-string "Activity headline: " ))
    (list name headline)))

(defun clearcase-make-internally-named-activity (stream-name comment-file)
  "Make a new activity in STREAM-NAME with creation comment in COMMENT-FILE,
and use an internally-generated name for the activity."

  (let ((ret
         (if clearcase-set-to-new-activity
             (clearcase-ct-blocking-call "mkact"
                                         "-cfile" (clearcase-path-native comment-file)
                                         "-in" stream-name
                                         "-force")
           (clearcase-ct-blocking-call "mkact"
                                       "-nset"
                                       "-cfile" (clearcase-path-native comment-file)
                                       "-in" stream-name
                                       "-nset"
                                       "-force"))))
    (if (string-match "Created activity \"\\([^\"]+\\)\"" ret)
        (substring ret (match-beginning 1) (match-end 1))
      (error "Failed to create activity: %s" ret))))

(defun clearcase-ucm-mkact-current-dir (name headline &optional comment)

  "Make an activity with NAME and HEADLINE and optional COMMENT, in the stream
associated with the view associated with the current directory."

  (interactive (clearcase-read-mkact-args))
  (let* ((viewtag (clearcase-fprop-viewtag default-directory))
         (stream  (clearcase-vprop-stream viewtag))
         (pvob    (clearcase-vprop-pvob viewtag)))
    (if (not (clearcase-vprop-ucm viewtag))
        (error "View %s is not a UCM view" viewtag))
    (if (null stream)
        (error "View %s has no stream" viewtag))
    (if (null stream)
        (error "View %s has no PVOB" viewtag))

    (if (null comment)
        ;; If no comment supplied, go and get one..
        ;;
        (progn
          (clearcase-comment-start-entry (format "new-activity-%d" (random))
                                         "Enter comment for new activity."
                                         'clearcase-ucm-mkact-current-dir
                                         (list name headline)))
      ;; ...else do the operation.
      ;;
      (message "Making activity...")
      (clearcase-with-tempfile
       comment-file
       (write-region comment nil comment-file nil 'noprint)
       (let ((qualified-stream (format "%s@%s" stream pvob)))
         (if (stringp name)
             (if clearcase-set-to-new-activity
                 (clearcase-ct-blocking-call "mkact"
                                             "-cfile" (clearcase-path-native comment-file)
                                             "-headline" headline
                                             "-in" qualified-stream
                                             "-force"
                                             name)
               (clearcase-ct-blocking-call "mkact"
                                           "-nset"
                                           "-cfile" (clearcase-path-native comment-file)
                                           "-headline" headline
                                           "-in" qualified-stream
                                           "-force"
                                           name))
           (progn
             ;; If no name was provided we do the creation in two steps:
             ;;   mkact -force
             ;;   chact -headline
             ;; to make sure we get preferred internally generated activity
             ;; name of the form "activityNNN.MMM" rather than some horrible
             ;; concoction based on the headline.
             ;;
             (let ((name (clearcase-make-internally-named-activity qualified-stream comment-file)))
               (clearcase-ct-blocking-call "chact"
                                           "-headline" headline
                                           name))))))

      ;; Flush the activities for this view so they'll get refreshed when needed.
      ;;
      (clearcase-vprop-flush-activities viewtag)

      (message "Making activity...done"))))

;;}}}

;;{{{ Set activity

(defun clearcase-ucm-filter-out-rebases (activities)
  (if (not clearcase-hide-rebase-activities)
      activities
    (clearcase-utl-list-filter
     (function
      (lambda (activity)
        (let ((id (car activity)))
          (not (string-match clearcase-rebase-id-regexp id)))))
     activities)))

(defun clearcase-ucm-set-activity-current-dir ()
  (interactive)
  (let* ((viewtag (clearcase-fprop-viewtag default-directory)))
    (if (not (clearcase-vprop-ucm viewtag))
        (error "View %s is not a UCM view" viewtag))
    ;; Filter out the rebases here if the user doesn't want to see them.
    ;;
    (let ((activities (clearcase-ucm-filter-out-rebases (clearcase-vprop-activities viewtag))))
      (if (null activities)
          (error "View %s has no activities" viewtag))
      (clearcase-ucm-make-selection-window (format "*clearcase-activity-select-%s*" viewtag)
                                           (mapconcat
                                            (function
                                             (lambda (activity)
                                               (let ((id (car activity))
                                                     (title (cdr activity)))
                                                 (format "%s\t%s" id title))))
                                            activities
                                            "\n")
                                           'clearcase-ucm-activity-selection-interpreter
                                           'clearcase-ucm-set-activity
                                           (list viewtag)))))

(defun clearcase-ucm-activity-selection-interpreter ()
  "Extract the activity name from the buffer at point"
  (if (looking-at "^\\(.*\\)\t")
      (let ((activity-name (buffer-substring (match-beginning 1)
                                             (match-end 1))))
        activity-name)
    (error "No activity on this line")))

(defun clearcase-ucm-set-activity-none-current-dir ()
  (interactive)
  (let* ((viewtag (clearcase-fprop-viewtag default-directory)))
    (if (not (clearcase-vprop-ucm viewtag))
        (error "View %s is not a UCM view" viewtag))
    (clearcase-ucm-set-activity viewtag nil)))

(defun clearcase-ucm-set-activity (viewtag activity-name)
  (if activity-name
      ;; Set an activity
      ;;
      (progn
        (message "Setting activity...")
        (let ((qualified-activity-name (if (string-match "@" activity-name)
                                           activity-name
                                         (concat activity-name "@" (clearcase-vprop-pvob viewtag)))))
          (clearcase-ct-blocking-call "setactivity" "-nc" "-view"
                                      viewtag
                                      (if qualified-activity-name
                                          qualified-activity-name
                                        "-none")))
        ;; Update cache
        ;;
        (clearcase-vprop-set-current-activity viewtag activity-name)
        (message "Setting activity...done"))

    ;; Set NO activity
    ;;
    (message "Unsetting activity...")
    (clearcase-ct-blocking-call "setactivity"
                                "-nc"
                                "-view" viewtag
                                "-none")
    ;; Update cache
    ;;
    (clearcase-vprop-set-current-activity viewtag nil)
    (message "Unsetting activity...done")))

;;}}}

;;{{{ Show current activity

(defun clearcase-ucm-describe-current-activity ()
  (interactive)
  (let* ((viewtag (clearcase-fprop-viewtag default-directory)))
    (if (not viewtag)
        (error "Not in a view"))
    (if (not (clearcase-vprop-ucm viewtag))
        (error "View %s is not a UCM view" viewtag))
    (let ((pvob (clearcase-vprop-pvob viewtag))
          (current-activity (clearcase-vprop-current-activity viewtag)))
      (if (not current-activity)
          (message "No activity set")
        (let ((text (clearcase-ct-blocking-call "desc"
                                                (concat "activity:"
                                                        current-activity
                                                        "@"
                                                        pvob))))
          (if (not (zerop (length text)))
              (clearcase-utl-populate-and-view-buffer
               "*clearcase*"
               (list text)
               (function (lambda (s)
                           (insert s))))))))))
;;}}}

;;}}}

;;{{{ Next-action

(defun clearcase-next-action-current-buffer ()
  "Do the next logical operation on the current file.
Operations include mkelem, checkout, checkin, uncheckout"
  (interactive)
  (clearcase-next-action buffer-file-name))

(defun clearcase-next-action-dired-files ()
  "Do the next logical operation on the marked files.
Operations include mkelem, checkout, checkin, uncheckout.
If all the files are not in an equivalent state, an error is raised."

  (interactive)
  (clearcase-next-action-seq (dired-get-marked-files)))

(defun clearcase-next-action (file)
  (let ((action (clearcase-compute-next-action file)))
    (cond

     ((eq action 'mkelem)
      (clearcase-commented-mkelem file))

     ((eq action 'checkout)
      (clearcase-commented-checkout file))

     ((eq action 'uncheckout)
      (if (yes-or-no-p "Checked-out file appears unchanged. Cancel checkout ? ")
          (clearcase-uncheckout file)))

     ((eq action 'illegal-checkin)
      (error "This file is checked out by someone else: %s" (clearcase-fprop-user file)))

     ((eq action 'checkin)
      (clearcase-commented-checkin file))

     (t
      (error "Can't compute suitable next ClearCase action for file %s" file)))))

(defun clearcase-next-action-seq (files)
  "Do the next logical operation on the sequence of FILES."

  ;; Check they're all in the same state.
  ;;
  (let ((actions (mapcar (function clearcase-compute-next-action) files)))
    (if (not (clearcase-utl-elts-are-eq actions))
        (error "Marked files are not all in the same state"))
    (let ((action (car actions)))
      (cond

       ((eq action 'mkelem)
        (clearcase-commented-mkelem-seq files))

       ((eq action 'checkout)
        (clearcase-commented-checkout-seq files))

       ((eq action 'uncheckout)
        (if (yes-or-no-p "Checked-out files appears unchanged. Cancel checkouts ? ")
            (clearcase-uncheckout-seq files)))

       ((eq action 'illegal-checkin)
        (error "These files are checked out by someone else; will no checkin"))

       ((eq action 'checkin)
        (clearcase-commented-checkin-seq files))

       (t
        (error "Can't compute suitable next ClearCase action for marked files"))))))

(defun clearcase-compute-next-action (file)
  "Compute the next logical action on FILE."

  (cond
   ;; nyi: other cases to consider later:
   ;;
   ;;   - file is unreserved
   ;;   - file is not mastered

   ;; Case 1: it is not yet an element
   ;;         ==> mkelem
   ;;
   ((clearcase-file-ok-to-mkelem file)
    'mkelem)

   ;; Case 2: file is not checked out
   ;;         ==> checkout
   ;;
   ((clearcase-file-ok-to-checkout file)
    'checkout)

   ;; Case 3: file is checked-out but not modified in buffer or disk
   ;;         ==> offer to uncheckout
   ;;
   ((and (clearcase-file-ok-to-uncheckout file)
         (not (file-directory-p file))
         (not (buffer-modified-p))
         (not (clearcase-file-appears-modified-since-checkout-p file)))
    'uncheckout)

   ;; Case 4: file is checked-out but by somebody else using this view.
   ;;         ==> refuse to checkin
   ;;
   ;; This is not reliable on some Windows installations where a user is known
   ;; as "esler" on Unix and the ClearCase server, and "ESLER" on the Windows
   ;; client.
   ;;
   ((and (not clearcase-on-mswindows)
         (clearcase-fprop-checked-out file)
         (not (string= (user-login-name)
                       (clearcase-fprop-user file))))
    'illegal-checkin)

   ;; Case 5: user has checked-out the file
   ;;         ==> check it in
   ;;
   ((clearcase-file-ok-to-checkin file)
    'checkin)

   (t
    nil)))

;;}}}

;;{{{ Mkelem

(defun clearcase-mkelem-current-buffer ()
  "Make the current file into a ClearCase element."
  (interactive)

  ;; Watch out for new buffers of size 0: the corresponding file
  ;; does not exist yet, even though buffer-modified-p is nil.
  ;;
  (if (and (not (buffer-modified-p))
           (zerop (buffer-size))
           (not (file-exists-p buffer-file-name)))
      (set-buffer-modified-p t))

  (clearcase-commented-mkelem buffer-file-name))

(defun clearcase-mkelem-dired-files ()
  "Make the selected files into ClearCase elements."
  (interactive)
  (clearcase-commented-mkelem-seq (dired-get-marked-files)))

;;}}}

;;{{{ Checkin

(defun clearcase-checkin-current-buffer ()
  "Checkin the file in the current buffer."
  (interactive)

  ;; Watch out for new buffers of size 0: the corresponding file
  ;; does not exist yet, even though buffer-modified-p is nil.
  ;;
  (if (and (not (buffer-modified-p))
           (zerop (buffer-size))
           (not (file-exists-p buffer-file-name)))
      (set-buffer-modified-p t))

  (clearcase-commented-checkin buffer-file-name))

(defun clearcase-checkin-dired-files ()
  "Checkin the selected files."
  (interactive)
  (clearcase-commented-checkin-seq (dired-get-marked-files)))

(defun clearcase-dired-checkin-current-dir ()
  (interactive)
  (clearcase-commented-checkin (dired-current-directory)))

;;}}}

;;{{{ Edit checkout comment

(defun clearcase-edit-checkout-comment-current-buffer ()
  "Edit the clearcase comment for the checked-out file in the current buffer."
  (interactive)
  (clearcase-edit-checkout-comment buffer-file-name))

(defun clearcase-edit-checkout-comment-dired-file ()
  "Checkin the selected file."
  (interactive)
  (clearcase-edit-checkout-comment (dired-get-filename)))

(defun clearcase-edit-checkout-comment (file &optional comment)
  "Edit comment for FILE by popping up a buffer to accept one.  If COMMENT
is specified, save it."
  (if (null comment)
      ;; If no comment supplied, go and get one...
      ;;
      (clearcase-comment-start-entry (file-name-nondirectory file)
				     "Edit the file's check-out comment."
				     'clearcase-edit-checkout-comment
				     (list buffer-file-name)
				     (find-file-noselect file)
				     (clearcase-fprop-comment file))
    ;; We have a comment, save it
    (clearcase-comment-save-comment-for-buffer comment clearcase-parent-buffer)))

;;}}}

;;{{{ Checkout

(defun clearcase-checkout-current-buffer ()
  "Checkout the file in the current buffer."
  (interactive)
  (clearcase-commented-checkout buffer-file-name))

(defun clearcase-checkout-dired-files ()
  "Checkout the selected files."
  (interactive)
  (clearcase-commented-checkout-seq (dired-get-marked-files)))

(defun clearcase-dired-checkout-current-dir ()
  (interactive)
  (clearcase-commented-checkout (dired-current-directory)))

;;}}}

;;{{{ Uncheckout

(defun clearcase-uncheckout-current-buffer ()
  "Uncheckout the file in the current buffer."
  (interactive)
  (clearcase-uncheckout buffer-file-name))

(defun clearcase-uncheckout-dired-files ()
  "Uncheckout the selected files."
  (interactive)
  (clearcase-uncheckout-seq (dired-get-marked-files)))

(defun clearcase-dired-uncheckout-current-dir ()
  (interactive)
  (clearcase-uncheckout (dired-current-directory)))

;;}}}

;;{{{ Mkbrtype

(defun clearcase-mkbrtype (typename)
  (interactive "sBranch type name: ")
  (clearcase-commented-mkbrtype typename))

;;}}}

;;{{{ Describe

(defun clearcase-describe-current-buffer ()
  "Give a ClearCase description of the file in the current buffer."
  (interactive)
  (clearcase-describe buffer-file-name))

(defun clearcase-describe-dired-file ()
  "Describe the selected files."
  (interactive)
  (clearcase-describe (dired-get-filename)))

;;}}}

;;{{{ What-rule

(defun clearcase-what-rule-current-buffer ()
  (interactive)
  (clearcase-what-rule buffer-file-name))

(defun clearcase-what-rule-dired-file ()
  (interactive)
  (clearcase-what-rule (dired-get-filename)))

;;}}}

;;{{{ List history

(defun clearcase-list-history-current-buffer ()
  "List the change history of the current buffer in a window."
  (interactive)
  (clearcase-list-history buffer-file-name))

(defun clearcase-list-history-dired-file ()
  "List the change history of the current file."
  (interactive)
  (clearcase-list-history (dired-get-filename)))

;;}}}

;;{{{ Ediff

(defun clearcase-ediff-pred-current-buffer ()
  "Use Ediff to compare a version in the current buffer against its predecessor."
  (interactive)
  (clearcase-ediff-file-with-version buffer-file-name
                                     (clearcase-fprop-predecessor-version buffer-file-name)))

(defun clearcase-ediff-pred-dired-file ()
  "Use Ediff to compare the selected version against its predecessor."
  (interactive)
  (let ((truename (clearcase-fprop-truename (dired-get-filename))))
    (clearcase-ediff-file-with-version truename
                                       (clearcase-fprop-predecessor-version truename))))

(defun clearcase-ediff-branch-base-current-buffer()
  "Use Ediff to compare a version in the current buffer
against the base of its branch."
  (interactive)
  (clearcase-ediff-file-with-version buffer-file-name
                                     (clearcase-vxpath-version-of-branch-base buffer-file-name)))

(defun clearcase-ediff-branch-base-dired-file()
  "Use Ediff to compare the selected version against the base of its branch."
  (interactive)
  (let ((truename (clearcase-fprop-truename (dired-get-filename))))
    (clearcase-ediff-file-with-version truename
                                       (clearcase-vxpath-version-of-branch-base truename))))

(defun clearcase-ediff-named-version-current-buffer (version)
  ;; nyi: if we're in history-mode, probably should just use
  ;; (read-file-name)
  ;;
  (interactive (list (clearcase-read-version-name "Version for comparison: "
                                                  buffer-file-name)))
  (clearcase-ediff-file-with-version buffer-file-name version))

(defun clearcase-ediff-named-version-dired-file (version)
  ;; nyi: if we're in history-mode, probably should just use
  ;; (read-file-name)
  ;;
  (interactive (list (clearcase-read-version-name "Version for comparison: "
                                                  (dired-get-filename))))
  (clearcase-ediff-file-with-version  (clearcase-fprop-truename (dired-get-filename))
                                      version))

(defun clearcase-ediff-file-with-version (truename other-version)
  (let ((other-vxpath (clearcase-vxpath-cons-vxpath (clearcase-vxpath-element-part truename)
                                                    other-version)))
    (if (clearcase-file-is-in-mvfs-p truename)
        (ediff-files other-vxpath truename)
      (ediff-buffers (clearcase-vxpath-get-version-in-buffer other-vxpath)
                     (find-file-noselect truename t)))))

;;}}}

;;{{{ GUI diff

(defun clearcase-gui-diff-pred-current-buffer ()
  "Use GUI to compare a version in the current buffer against its predecessor."
  (interactive)
  (clearcase-gui-diff-file-with-version buffer-file-name
                                        (clearcase-fprop-predecessor-version buffer-file-name)))

(defun clearcase-gui-diff-pred-dired-file ()
  "Use GUI to compare the selected version against its predecessor."
  (interactive)
  (let ((truename (clearcase-fprop-truename (dired-get-filename))))
    (clearcase-gui-diff-file-with-version truename
                                          (clearcase-fprop-predecessor-version truename))))

(defun clearcase-gui-diff-branch-base-current-buffer()
  "Use GUI to compare a version in the current buffer
against the base of its branch."
  (interactive)
  (clearcase-gui-diff-file-with-version buffer-file-name
                                        (clearcase-vxpath-version-of-branch-base buffer-file-name)))

(defun clearcase-gui-diff-branch-base-dired-file()
  "Use GUI to compare the selected version against the base of its branch."
  (interactive)
  (let ((truename (clearcase-fprop-truename (dired-get-filename))))
    (clearcase-gui-diff-file-with-version truename
                                          (clearcase-vxpath-version-of-branch-base truename))))

(defun clearcase-gui-diff-named-version-current-buffer (version)
  ;; nyi: if we're in history-mode, probably should just use
  ;; (read-file-name)
  ;;
  (interactive (list (clearcase-read-version-name "Version for comparison: "
                                                  buffer-file-name)))
  (clearcase-gui-diff-file-with-version buffer-file-name version))

(defun clearcase-gui-diff-named-version-dired-file (version)
  ;; nyi: if we're in history-mode, probably should just use
  ;; (read-file-name)
  ;;
  (interactive (list (clearcase-read-version-name "Version for comparison: "
                                                  (dired-get-filename))))
  (clearcase-gui-diff-file-with-version  (clearcase-fprop-truename (dired-get-filename))
                                         version))

(defun clearcase-gui-diff-file-with-version (truename other-version)
  (let* ((other-vxpath (clearcase-vxpath-cons-vxpath (clearcase-vxpath-element-part truename)
                                                     other-version))
         (other-file (if (clearcase-file-is-in-mvfs-p truename)
                         other-vxpath
                       (clearcase-vxpath-get-version-in-temp-file other-vxpath)))
         (gui-name (if clearcase-on-mswindows
                       "cleardiffmrg"
                     "xcleardiff")))
    (start-process "Diff"
                   nil
                   gui-name
                   (clearcase-path-native other-file)
                   (clearcase-path-native truename))))

;;}}}

;;{{{ Diff

(defun clearcase-diff-pred-current-buffer ()
  "Use Diff to compare a version in the current buffer against its predecessor."
  (interactive)
  (clearcase-diff-file-with-version buffer-file-name
                                    (clearcase-fprop-predecessor-version buffer-file-name)))

(defun clearcase-diff-pred-dired-file ()
  "Use Diff to compare the selected version against its predecessor."
  (interactive)
  (let ((truename (clearcase-fprop-truename (dired-get-filename))))
    (clearcase-diff-file-with-version truename
                                      (clearcase-fprop-predecessor-version truename))))

(defun clearcase-diff-branch-base-current-buffer()
  "Use Diff to compare a version in the current buffer
against the base of its branch."
  (interactive)
  (clearcase-diff-file-with-version buffer-file-name
                                    (clearcase-vxpath-version-of-branch-base buffer-file-name)))

(defun clearcase-diff-branch-base-dired-file()
  "Use Diff to compare the selected version against the base of its branch."
  (interactive)
  (let ((truename (clearcase-fprop-truename (dired-get-filename))))
    (clearcase-diff-file-with-version truename
                                      (clearcase-vxpath-version-of-branch-base truename))))

(defun clearcase-diff-named-version-current-buffer (version)
  ;; nyi: if we're in history-mode, probably should just use
  ;; (read-file-name)
  ;;
  (interactive (list (clearcase-read-version-name "Version for comparison: "
                                                  buffer-file-name)))
  (clearcase-diff-file-with-version buffer-file-name version))

(defun clearcase-diff-named-version-dired-file (version)
  ;; nyi: if we're in history-mode, probably should just use
  ;; (read-file-name)
  ;;
  (interactive (list (clearcase-read-version-name "Version for comparison: "
                                                  (dired-get-filename))))
  (clearcase-diff-file-with-version (clearcase-fprop-truename (dired-get-filename))
                                    version))

(defun clearcase-diff-file-with-version (truename other-version)
  (let ((other-vxpath (clearcase-vxpath-cons-vxpath (clearcase-vxpath-element-part truename)
                                                    other-version)))
    (if (clearcase-file-is-in-mvfs-p truename)
        (clearcase-diff-files other-vxpath truename)
      (clearcase-diff-files (clearcase-vxpath-get-version-in-temp-file other-vxpath)
                            truename))))

;;}}}

;;{{{ Browse vtree

(defun clearcase-version-other-window (version)
  (interactive
   (list
    (clearcase-read-version-name (format "Version of %s to visit: "
      (file-name-nondirectory buffer-file-name))
                                 buffer-file-name)))
  (find-file-other-window (clearcase-vxpath-cons-vxpath
                           (clearcase-vxpath-element-part buffer-file-name)
                           version)))

(defun clearcase-browse-vtree-current-buffer ()
  (interactive)
  (clearcase-browse-vtree buffer-file-name))

(defun clearcase-browse-vtree-dired-file ()
  (interactive)
  (clearcase-browse-vtree (dired-get-filename)))

;;}}}

;;{{{ GUI vtree

(defun clearcase-gui-vtree-browser-current-buffer ()
  (interactive)
  (clearcase-gui-vtree-browser buffer-file-name))

(defun clearcase-gui-vtree-browser-dired-file ()
  (interactive)
  (clearcase-gui-vtree-browser (dired-get-filename)))

(defun clearcase-gui-vtree-browser (file)
  (let ((gui-name (if clearcase-on-mswindows
                      "clearvtree"
                    "xlsvtree")))
    (start-process-shell-command "Vtree_browser"
                                 nil
                                 gui-name
                                 (clearcase-path-native file))))

;;}}}

;;{{{ Other GUIs

(defun clearcase-gui-clearexplorer ()
  (interactive)
  (start-process-shell-command "ClearExplorer"
                               nil
                               "clearexplorer"
                               "."))

(defun clearcase-gui-rebase ()
  (interactive)
  (start-process-shell-command "Rebase"
                               nil
                               "clearmrgman"
                               (if clearcase-on-mswindows
                                   "/rebase"
                                 "-rebase")))

(defun clearcase-gui-deliver ()
  (interactive)
  (start-process-shell-command "Deliver"
                               nil
                               "clearmrgman"
                               (if clearcase-on-mswindows
                                   "/deliver"
                                 "-deliver")))

(defun clearcase-gui-merge-manager ()
  (interactive)
  (start-process-shell-command "Merge_manager"
                               nil
                               "clearmrgman"))

(defun clearcase-gui-project-explorer ()
  (interactive)
  (start-process-shell-command "Project_explorer"
                               nil
                               "clearprojexp"))

(defun clearcase-gui-snapshot-view-updater ()
  (interactive)
  (start-process-shell-command "View_updater"
                               nil
                               "clearviewupdate"))

;;}}}

;;{{{ Update snapshot

;; In a file buffer:
;;  - update current-file
;;  - update directory
;; In dired:
;;  - update dir
;;  - update marked files
;;  - update file

;; We allow several simultaneous updates, but only one per view.

(defun clearcase-update-view ()
  (interactive)
  (clearcase-update (clearcase-fprop-viewtag default-directory)))

(defun clearcase-update-default-directory ()
  (interactive)
  (clearcase-update (clearcase-fprop-viewtag default-directory)
                    default-directory))

(defun clearcase-update-current-buffer ()
  (interactive)
  (clearcase-update (clearcase-fprop-viewtag default-directory)
                    buffer-file-name))

(defun clearcase-update-dired-files ()
  (interactive)
  (apply (function clearcase-update)
         (cons (clearcase-fprop-viewtag default-directory)
               (dired-get-marked-files))))


;;}}}

;;}}}

;;{{{ Functions

;;{{{ Basic ClearCase operations

;;{{{ Update snapshot view

;;{{{ Asynchronous post-processing of update

(defvar clearcase-post-update-timer nil)
(defvar clearcase-post-update-work-queue nil)

(defun clearcase-post-update-schedule-work (buffer)
  (clearcase-trace "entering clearcase-post-update-schedule-work")
  ;; Add to the work queue.
  ;;
  (setq clearcase-post-update-work-queue (cons buffer
                                               clearcase-post-update-work-queue))
  ;; Create the timer if necessary.
  ;;
  (if (null clearcase-post-update-timer)
      (if clearcase-xemacs-p
          ;; Xemacs
          ;;
          (setq clearcase-post-update-timer
                (run-with-idle-timer 2 t 'clearcase-post-update-timer-function))
        ;; FSF Emacs
        ;;
        (progn
          (setq clearcase-post-update-timer (timer-create))
          (timer-set-function clearcase-post-update-timer 'clearcase-post-update-timer-function)
          (timer-set-idle-time clearcase-post-update-timer 2)
          (timer-activate-when-idle clearcase-post-update-timer)))
    (clearcase-trace "clearcase-post-update-schedule-work: post-update timer found to be non-null")))


(defun clearcase-post-update-timer-function ()
  (clearcase-trace "Entering clearcase-post-update-timer-function")
  ;; For (each update-process buffer in the work queue)
  ;;   if (its process has successfully terminated)
  ;;      do the post-processing for this update
  ;;      remove it from the work queue
  ;;
  (clearcase-trace (format "Queue before: %s" clearcase-post-update-work-queue))
  (setq clearcase-post-update-work-queue

        (clearcase-utl-list-filter
         (function clearcase-post-update-check-process-buffer)
         clearcase-post-update-work-queue))

  (clearcase-trace (format "Queue after: %s" clearcase-post-update-work-queue))
  ;; If the work queue is now empty cancel the timer.
  ;;
  (if (null clearcase-post-update-work-queue)
      (progn
        (cancel-timer clearcase-post-update-timer)
        (setq clearcase-post-update-timer nil))))

(defun clearcase-post-update-check-process-buffer (buffer)
  (clearcase-trace "Entering clearcase-post-update-check-process-buffer")

  ;; return t for those buffers that should remain in the work queue

  ;; if it has terminated successfully
  ;;   go sync buffers on the files that were updated

  ;; We want to field errors here and when they occurm return nil to avoid a
  ;; loop
  ;;
  ;;(condition-case nil

  ;; protected form
  (let ((proc (get-buffer-process buffer)))
    (if proc
        ;; Process still exists so keep this on the work queue.
        ;;
        (progn
          (clearcase-trace "Update process still exists")
          t)

      ;; Process no longer there, cleaned up by comint code.
      ;;

      ;; Sync any buffers that need it.
      ;;
      (clearcase-trace "Update process finished")
      (clearcase-sync-after-scopes-updated (with-current-buffer buffer
                                             ;; Evaluate buffer-local variable.
                                             ;;
                                             clearcase-update-buffer-scopes))

      ;; Remove  from work queue
      ;;
      nil))

  ;; Error occurred, make sure we return nil to remove the buffer from the
  ;; work queue, or a loop could develop.
  ;;
  ;;(error nil)
  )

(defun clearcase-sync-after-scopes-updated (scopes)
  (clearcase-trace "Entering clearcase-sync-after-scopes-updated")

  ;; nyi: reduce scopes to minimal set of disjoint scopes

  ;; Use dynamic binding here since we don't have lexical binding.
  ;;
  (let ((clearcase-dynbound-updated-scopes scopes))

    ;; For all buffers...
    ;;
    (mapcar
     (function
      (lambda (buffer)
        (let ((visited-file (buffer-file-name buffer)))
          (if visited-file
              (if (clearcase-path-file-in-any-scopes visited-file
                                                     clearcase-dynbound-updated-scopes)
                  ;; This buffer visits a file within an updated scope.
                  ;; Sync it from disk if it needs it.
                  ;;
                  (clearcase-sync-from-disk-if-needed visited-file))

            ;; Buffer is not visiting a file.  If it is a dired-mode buffer
            ;; under one of the scopes, revert it.
            ;;
            (with-current-buffer buffer
              (if (eq 'dired-mode major-mode)
                  (if (clearcase-path-file-in-any-scopes default-directory
                                                         clearcase-dynbound-updated-scopes)
                      (dired-revert nil t))))))))
     (buffer-list))))

;;}}}

;; Silence compiler complaints about free variable.
;;
(defvar clearcase-update-buffer-viewtag nil)

(defun clearcase-update (viewtag &rest files)
  "Run a cleartool+update process in VIEWTAG
if there isn't one already running in that view.
Other arguments FILES indicate files to update"

  ;; Check that there is no update process running in that view.
  ;;
  (if (apply (function clearcase-utl-or-func)
             (mapcar (function (lambda (proc)
                                 (if (not (eq 'exit (process-status proc)))
                                     (let ((buf (process-buffer proc)))
                                       (and buf
                                            (assq 'clearcase-update-buffer-viewtag
                                                  (buffer-local-variables buf))
                                            (save-excursion
                                              (set-buffer buf)
                                              (equal viewtag
                                                     clearcase-update-buffer-viewtag)))))))
                     (process-list)))
      (error "There is already an update running in view %s" viewtag))

  ;; All clear so:
  ;;  - create a process in a buffer
  ;;  - rename the buffer to be of the form *clearcase-update*<N>
  ;;  - mark it as one of ours by setting clearcase-update-buffer-viewtag
  ;;
  (pop-to-buffer (apply (function make-comint)
                        (append (list "*clearcase-update-temp-name*"
                                      clearcase-cleartool-path
                                      nil
                                      "update")
                                files))
                 t) ;; other window
  (rename-buffer "*clearcase-update*" t)

  ;; Store in this buffer what view was being updated and what files.
  ;;
  (set (make-local-variable 'clearcase-update-buffer-viewtag) viewtag)
  (set (make-local-variable 'clearcase-update-buffer-scopes) files)

  ;; nyi: schedule post-update buffer syncing
  (clearcase-post-update-schedule-work (current-buffer)))

;;}}}

;;{{{ Hijack

(defun clearcase-file-ok-to-hijack (file)

  "Test if FILE is suitable for hijack."

  (and

   ;; If it is writeable already, no need to offer a hijack operation, even
   ;; though, according to ClearCase, it may not yet be hijacked.
   ;;
   ;;(not (file-writable-p file))

   (not (clearcase-fprop-hijacked file))
   (clearcase-file-is-in-view-p file)
   (not (clearcase-file-is-in-mvfs-p file))
   (eq 'version (clearcase-fprop-mtype file))
   (not (clearcase-fprop-checked-out file))))

(defun clearcase-hijack-seq (files)
  (unwind-protect
      (progn
        (message "Hijacking...")
        (mapcar
         (function
          (lambda (file)
            (if (not (file-directory-p file))
                (clearcase-hijack file))))
         files))
    ;; Unwind
    ;;
    (message "Hijacking...done")))

(defun clearcase-hijack (file)

  ;; cases
  ;;  - buffer/files modtimes are equal
  ;;  - file more recent
  ;;    ==> revert
  ;;  - buffer more recent
  ;;    ==> make file writeable; save buffer ?
  ;;
  ;; Post-conditions:
  ;;   - file is hijacked wrt. CC
  ;;   - buffer is in sync with disk contents, modtime and writeability
  ;;     except if the user refused to save
  ;;
  (if (not (file-writable-p file))
      ;; Make it writeable.
      ;;
      (clearcase-utl-make-writeable file))

  ;; Attempt to modify the modtime of the file on disk, otherwise ClearCase
  ;; won't actually deem it hijacked. This will silently fail if there is no
  ;; "touch" command command available.
  ;;
  (clearcase-utl-touch-file file)

  ;; Sync up any buffers.
  ;;
  (clearcase-sync-from-disk file t))

;;}}}

;;{{{ Unhijack

(defun clearcase-file-ok-to-unhijack (file)
  "Test if FILE is suitable for unhijack."
  (clearcase-fprop-hijacked file))

(defun clearcase-unhijack (file)
  (clearcase-unhijack-seq (list file)))

(defun cleartool-unhijack-parse-for-kept-files (ret snapshot-view-root)
  ;; Look for occurrences of:
  ;; Loading "source\emacs\.emacs.el" (296690 bytes).
  ;; (renaming original hijacked object to ".emacs.el.keep.10").
  ;;
  (let ((start 0)
        (kept-files nil))
    (while (string-match
            "^Loading \"\\([^\"]+\\)\"[^\n]+\n(renaming original hijacked object to \"\\([^\"]+\\)\")\\.\n"
            ret
            start)
      (let* ((elt-path (substring ret (match-beginning 1) (match-end 1)))
             (abs-elt-path (concat (if snapshot-view-root
                                       snapshot-view-root
                                     "/")
                                   elt-path))
             (abs-elt-dir (file-name-directory abs-elt-path ))
             (kept-file-rel (concat abs-elt-dir
                                    (substring ret (match-beginning 2) (match-end 2))))

             ;; This is necessary on Windows to get an absolute path, i.e. one
             ;; with a drive letter. Note: probably only correct if
             ;; unhijacking files in a single snapshot view, mounted on a
             ;; drive-letter.
             ;;
             (kept-file (expand-file-name kept-file-rel)))
        (setq kept-files (cons kept-file kept-files)))
      (setq start (match-end 0)))
    kept-files))

(defun clearcase-utl-files-in-same-view-p (files)
  (if (< (length files) 2)
      t
    (let ((v0 (clearcase-fprop-viewtag (nth 0 files)))
          (v1 (clearcase-fprop-viewtag (nth 1 files))))
      (if (or (not (stringp v0))
              (not (stringp v1))
              (not (string= v0 v1)))
          nil
        (clearcase-utl-files-in-same-view-p (cdr files))))))

(defun clearcase-unhijack-seq (files)

  ;; Check: there are no directories involved.
  ;;
  (mapcar
   (function
    (lambda (file)
      (if (file-directory-p file)
          (error "Cannot unhijack a directory"))))
   files)

  ;; Check: all files are in the same snapshot view.
  ;;
  ;; (Why ?  The output from ct+update only has view-root-relative paths
  ;; and we need to obtain absolute paths of renamed-aside hijacks if we are to
  ;; dired-relist them.)
  ;;
  ;; Alternative: partition the set, with each partition containing elements in
  ;; the same view.
  ;;
  (if (not (clearcase-utl-files-in-same-view-p files))
      (error "Can't unhijack files in different views in the same operation"))

  ;; Run the scoped workspace update synchronously.
  ;;
  (unwind-protect
      (progn
        (message "Unhijacking...")
        (let* ((ret (apply (function clearcase-ct-blocking-call)
                           (append (list "update"
                                         (if clearcase-keep-unhijacks
                                             "-rename"
                                           "-overwrite")
                                         "-log" clearcase-sink-file-name)
                                   files)))
               (snapshot-view-root (clearcase-file-snapshot-root (car files)))

               ;; Scan for renamed-aside files.
               ;;
               (kept-files (if clearcase-keep-unhijacks
                               (cleartool-unhijack-parse-for-kept-files ret
                                                                        snapshot-view-root)
                             nil)))

          ;; Do post-update synchronisation.
          ;;
          (mapcar
           (function clearcase-sync-after-file-updated-from-vob)
           files)

          ;; Update any dired buffers as to the existence of the kept files.
          ;;
          (if clearcase-keep-unhijacks
              (mapcar (function
                       (lambda (file)
                         (dired-relist-file file)))
                      kept-files))))
    ;; unwind
    ;;
    (message "Unhijacking...done")))

;;}}}

;;{{{ Mkelem

(defun clearcase-file-ok-to-mkelem (file)
  "Test if FILE is okay to mkelem."
  (let ((mtype (clearcase-fprop-mtype file)))
    (and (not (file-directory-p file))
         (and (or (equal 'view-private-object mtype)
                  (equal 'derived-object mtype))
              (not (clearcase-fprop-hijacked file))
              (not (clearcase-file-covers-element-p file))))))

(defun clearcase-assert-file-ok-to-mkelem (file)
  "Raise an exception if FILE is not suitable for mkelem."
  (if (not (clearcase-file-ok-to-mkelem file))
      (error "%s cannot be made into an element" file)))

(defun clearcase-commented-mkelem (file &optional okay-to-checkout-dir-first comment)
  "Create a new element from FILE. If OKAY-TO-CHECKOUT-DIR-FIRST is non-nil,
the containing directory will be checked out if necessary.
If COMMENT is non-nil, it will be used, otherwise the user will be prompted
to enter one."

  ;; Pre-condition
  ;;
  (clearcase-assert-file-ok-to-mkelem file)

  (let ((containing-dir (file-name-directory file)))

    ;; Pre-condition
    ;;
    (if (not (eq 'directory-version (clearcase-fprop-mtype containing-dir)))
        (error "Parent directory of %s is not a ClearCase versioned directory."
               file))

    ;; Determine if we'll need to checkout the parent directory first.
    ;;
    (let ((dir-checkout-needed (not (clearcase-fprop-checked-out containing-dir))))
      (if dir-checkout-needed
          (progn
            ;; Parent dir will need to be checked out. Get permission if
            ;; appropriate.
            ;;
            (if (null okay-to-checkout-dir-first)
                (setq okay-to-checkout-dir-first
                      (or (null clearcase-verify-pre-mkelem-dir-checkout)
                          (y-or-n-p (format "Checkout directory %s " containing-dir)))))
            (if (null okay-to-checkout-dir-first)
                (error "Can't make an element unless directory is checked-out."))))

      (if (null comment)
          ;; If no comment supplied, go and get one...
          ;;
          (clearcase-comment-start-entry (file-name-nondirectory file)
                                         "Enter initial comment for the new element."
                                         'clearcase-commented-mkelem
                                         (list file okay-to-checkout-dir-first)
                                         (find-file-noselect file)
                                         clearcase-initial-mkelem-comment)

        ;; ...otherwise perform the operation.
        ;;

        ;;    We may need to checkout the directory.
        ;;
        (if dir-checkout-needed
            (clearcase-commented-checkout containing-dir comment))

        (clearcase-fprop-unstore-properties file)

        (message "Making element %s..." file)

        (save-excursion
          ;; Sync the buffer to disk.
          ;;
          (let ((buffer-on-file (find-buffer-visiting file)))
            (if buffer-on-file
                (progn
                  (set-buffer buffer-on-file)
                  (clearcase-sync-to-disk))))

          (clearcase-ct-do-cleartool-command "mkelem"
                                             file
                                             comment
                                             (if clearcase-checkin-on-mkelem
                                                 (list "-ci")))
          (message "Making element %s...done" file)

          ;; Resync.
          ;;
          (clearcase-sync-from-disk file t))))))

(defun clearcase-commented-mkelem-seq (files &optional comment)
  "Mkelem a sequence of FILES. If COMMENT is supplied it will be
used, otherwise the user will be prompted to enter one."

  (mapcar
   (function clearcase-assert-file-ok-to-mkelem)
   files)

  (if (null comment)
      ;; No comment supplied, go and get one...
      ;;
      (clearcase-comment-start-entry "mkelem"
                                     "Enter comment for elements' creation"
                                     'clearcase-commented-mkelem-seq
                                     (list files))
    ;; ...otherwise operate.
    ;;
    (mapcar
     (function
      (lambda (file)
        (clearcase-commented-mkelem file nil comment)))
     files)))

;;}}}

;;{{{ Checkin

(defun clearcase-file-ok-to-checkin (file)
  "Test if FILE is suitable for checkin."
  (let ((me (user-login-name)))
    (equal me (clearcase-fprop-owner-of-checkout file))))

(defun clearcase-assert-file-ok-to-checkin (file)
  "Raise an exception if FILE is not suitable for checkin."
  (if (not (clearcase-file-ok-to-checkin file))
      (error "You cannot checkin %s" file)))

(defun clearcase-commented-checkin (file &optional comment)
  "Check-in FILE with COMMENT. If the comment is omitted,
a buffer is popped up to accept one."

  (clearcase-assert-file-ok-to-checkin file)

  (if (null comment)
      ;; If no comment supplied, go and get one..
      ;;
      (progn
        (clearcase-comment-start-entry (file-name-nondirectory file)
                                       "Enter a checkin comment."
                                       'clearcase-commented-checkin
                                       (list file)
                                       (find-file-noselect file)
                                       (clearcase-fprop-comment file))

        ;; Also display a diff, if that is the custom:
        ;;
        (if (and (not (file-directory-p file))
                 clearcase-diff-on-checkin)
            (save-excursion
              (let ((tmp-buffer (current-buffer)))
                (message "Running diff...")
                (clearcase-diff-file-with-version file
                                                  (clearcase-fprop-predecessor-version file))
                (message "Running diff...done")
                (set-buffer "*clearcase*")
                (if (get-buffer "*clearcase-diff*")
                    (kill-buffer "*clearcase-diff*"))
                (rename-buffer "*clearcase-diff*")
                (pop-to-buffer tmp-buffer)))))

    ;; ...otherwise perform the operation.
    ;;
    (message "Checking in %s..." file)
    (save-excursion
      ;; Sync the buffer to disk, and get local value of clearcase-checkin-arguments
      ;;
      (let ((buffer-on-file (find-buffer-visiting file)))
        (if buffer-on-file
            (progn
              (set-buffer buffer-on-file)
              (clearcase-sync-to-disk))))
      (clearcase-ct-do-cleartool-command "ci"
                                         file
                                         comment
                                         clearcase-checkin-arguments))
    (message "Checking in %s...done" file)

    ;; Resync.
    ;;
    (clearcase-sync-from-disk file t)))

(defun clearcase-commented-checkin-seq (files &optional comment)
  "Checkin a sequence of FILES. If COMMENT is supplied it will be
used, otherwise the user will be prompted to enter one."

  ;; Check they're all in the right state to be checked-in.
  ;;
  (mapcar
   (function clearcase-assert-file-ok-to-checkin)
   files)

  (if (null comment)
      ;; No comment supplied, go and get one...
      ;;
      (clearcase-comment-start-entry "checkin"
                                     "Enter checkin comment."
                                     'clearcase-commented-checkin-seq
                                     (list files))
    ;; ...otherwise operate.
    ;;
    (mapcar
     (function
      (lambda (file)
        (clearcase-commented-checkin file comment)))
     files)))

;;}}}

;;{{{ Checkout

(defun clearcase-file-ok-to-checkout (file)
  "Test if FILE is suitable for checkout."
  (let ((mtype (clearcase-fprop-mtype file)))
    (and (or (eq 'version mtype)
             (eq 'directory-version mtype)
             (clearcase-fprop-hijacked file))
         (not (clearcase-fprop-checked-out file)))))

(defun clearcase-assert-file-ok-to-checkout (file)
  "Raise an exception if FILE is not suitable for checkout."
  (if (not (clearcase-file-ok-to-checkout file))
      (error "You cannot checkout %s" file)))

;; nyi: Offer to setact if appropriate

(defun clearcase-commented-checkout (file &optional comment)
  "Check-out FILE with COMMENT. If the comment is omitted,
a buffer is popped up to accept one."

  (clearcase-assert-file-ok-to-checkout file)

  (if (and (null comment)
           (not clearcase-suppress-checkout-comments))
      ;; If no comment supplied, go and get one...
      ;;
      (clearcase-comment-start-entry (file-name-nondirectory file)
                                     "Enter a checkout comment."
                                     'clearcase-commented-checkout
                                     (list file)
                                     (find-file-noselect file))

    ;; ...otherwise perform the operation.
    ;;
    (message "Checking out %s..." file)
    ;; Change buffers to get local value of clearcase-checkin-arguments.
    ;;
    (save-excursion
      (set-buffer (or (find-buffer-visiting file)
                      (current-buffer)))
      (clearcase-ct-do-cleartool-command "co"
                                         file
                                         comment
                                         clearcase-checkout-arguments))
    (message "Checking out %s...done" file)

    ;; Resync.
    ;;
    (clearcase-sync-from-disk file t)))


(defun clearcase-commented-checkout-seq (files &optional comment)
  "Checkout a sequence of FILES. If COMMENT is supplied it will be
used, otherwise the user will be prompted to enter one."

  (mapcar
   (function clearcase-assert-file-ok-to-checkout)
   files)

  (if (and (null comment)
           (not clearcase-suppress-checkout-comments))
      ;; No comment supplied, go and get one...
      ;;
      (clearcase-comment-start-entry "checkout"
                                     "Enter a checkout comment."
                                     'clearcase-commented-checkout-seq
                                     (list files))
    ;; ...otherwise operate.
    ;;
    (mapcar
     (function
      (lambda (file)
        (clearcase-commented-checkout file comment)))
     files)))

;;}}}

;;{{{ Uncheckout

(defun clearcase-file-ok-to-uncheckout (file)
  "Test if FILE is suitable for uncheckout."
  (equal (user-login-name)
         (clearcase-fprop-owner-of-checkout file)))

(defun clearcase-assert-file-ok-to-uncheckout (file)
  "Raise an exception if FILE is not suitable for uncheckout."
  (if (not (clearcase-file-ok-to-uncheckout file))
      (error "You cannot uncheckout %s" file)))

(defun cleartool-unco-parse-for-kept-file (ret)
  ;;Private version of "foo" saved in "foo.keep.1"
  (if (string-match "^Private version of .* saved in \"\\([^\"]+\\)\"\\.$" ret)
      (substring ret (match-beginning 1) (match-end 1))
    nil))

(defun clearcase-uncheckout (file)
  "Uncheckout FILE."

  (clearcase-assert-file-ok-to-uncheckout file)

  ;; If it has changed since checkout, insist the user confirm.
  ;;
  (if (and (not (file-directory-p file))
           (clearcase-file-appears-modified-since-checkout-p file)
           (not clearcase-suppress-confirm)
           (not (yes-or-no-p (format "Really discard changes to %s ?" file))))
      (message "Uncheckout of %s cancelled" file)

    ;; Go ahead and unco.
    ;;
    (message "Cancelling checkout of %s..." file)
    ;; nyi:
    ;;  - Prompt for -keep or -rm
    ;;  - offer to remove /0 branches
    ;;
    (let* ((ret (clearcase-ct-blocking-call "unco"
                                            (if clearcase-keep-uncheckouts
                                                "-keep"
                                              "-rm")
                                            file))
           ;; Discover the name of the saved.
           ;;
           (kept-file (if clearcase-keep-uncheckouts
                          (cleartool-unco-parse-for-kept-file ret)
                        nil)))

      (if kept-file
          (message "Checkout of %s cancelled (saved in %s)"
                   (file-name-nondirectory kept-file)
                   file)
        (message "Cancelling checkout of %s...done" file))

      ;; Sync any buffers over the file itself.
      ;;
      (clearcase-sync-from-disk file t)

      ;; Update any dired buffers as to the existence of the kept file.
      ;;
      (if kept-file
          (dired-relist-file kept-file)))))

(defun clearcase-uncheckout-seq (files)
  "Uncheckout a sequence of FILES."

  (mapcar
   (function clearcase-assert-file-ok-to-uncheckout)
   files)

  (mapcar
   (function clearcase-uncheckout)
   files))

;;}}}

;;{{{ Describe

(defun clearcase-describe (file)
  "Give a ClearCase description of FILE."

  (clearcase-utl-populate-and-view-buffer
   "*clearcase*"
   (list file)
   (function
    (lambda (file)
      (clearcase-ct-do-cleartool-command "describe" file 'unused)))))

(defun clearcase-describe-seq (files)
  "Give a ClearCase description of the sequence of FILES."
  (error "Not yet implemented"))

;;}}}

;;{{{ Mkbrtype

(defun clearcase-commented-mkbrtype (typename &optional comment)
  (if (null comment)
      (clearcase-comment-start-entry (format "mkbrtype:%s" typename)
                                     "Enter a comment for the new branch type."
                                     'clearcase-commented-mkbrtype
                                     (list typename))
    (clearcase-with-tempfile
     comment-file
     (write-region comment nil comment-file nil 'noprint)
     (let ((qualified-typename typename))
       (if (not (string-match "@" typename))
           (setq qualified-typename
                 (format "%s@%s" typename default-directory)))

       (clearcase-ct-cleartool-cmd "mkbrtype"
                                   "-cfile"
                                   (clearcase-path-native comment-file)
                                   qualified-typename)))))

;;}}}

;;{{{ Browse vtree (using Dired Mode)

(defun clearcase-file-ok-to-browse (file)
  (and file
       (or (equal 'version (clearcase-fprop-mtype file))
           (equal 'directory-version (clearcase-fprop-mtype file)))
       (clearcase-file-is-in-mvfs-p file)))

(defun clearcase-browse-vtree (file)
  (if (not (clearcase-fprop-file-is-version-p file))
      (error "%s is not a Clearcase element" file))

  (if (not (clearcase-file-is-in-mvfs-p file))
      (error "File is not in MVFS"))

  (let* ((version-path (clearcase-vxpath-cons-vxpath
                        file
                        (or (clearcase-vxpath-version-part file)
                            (clearcase-fprop-version file))))
         ;; nyi: Can't seem to get latest first here.
         ;;
         (dired-listing-switches (concat dired-listing-switches
                                         "rt"))

         (branch-path (clearcase-vxpath-branch version-path))

         ;; Position cursor to the version we came from.
         ;; If it was checked-out, go to predecessor.
         ;;
         (version-number (clearcase-vxpath-version
                          (if (clearcase-fprop-checked-out file)
                              (clearcase-fprop-predecessor-version file)
                            version-path))))

    (if (file-exists-p version-path)
        (progn
          ;; Invoke dired on the directory of the version branch.
          ;;
          (dired branch-path)

          (clearcase-dired-sort-by-date)

          (if (re-search-forward (concat "[ \t]+"
                                         "\\("
                                         (regexp-quote version-number)
                                         "\\)"
                                         "$")
                                 nil
                                 t)
              (goto-char (match-beginning 1))))
      (dired (concat file clearcase-vxpath-glue))

      ;; nyi: We want ANY directory in the history tree to appear with
      ;;      newest first. Probably requires a hook to dired mode.
      ;;
      (clearcase-dired-sort-by-date))))

;;}}}

;;{{{ List history

(defun clearcase-list-history (file)
  "List the change history of FILE.

FILE can be a file or a directory. If it is a directory, only the information
on the directory element itself is listed, not on its contents."

  (let ((mtype (clearcase-fprop-mtype file)))
    (if (or (eq mtype 'version)
            (eq mtype 'directory-version))
        (progn
          (message "Listing element history...")

          (clearcase-utl-populate-and-view-buffer
           "*clearcase*"
           (list file)
           (function
            (lambda (file)
              (clearcase-ct-do-cleartool-command "lshistory"
                                                 file
                                                 'unused
                                                 (if (eq mtype 'directory-version)
                                                     (list "-d")))
              (setq default-directory (file-name-directory file))
              (while (looking-at "=3D*\n")
                (delete-char (- (match-end 0) (match-beginning 0)))
                (forward-line -1))
              (goto-char (point-min))
              (if (looking-at "[\b\t\n\v\f\r ]+")
                  (delete-char (- (match-end 0) (match-beginning 0)))))))
          (message "Listing element history...done"))

      (error "%s is not a ClearCase element" file))))

;;}}}

;;{{{ Diff/cmp

(defun clearcase-files-are-identical (f1 f2)
  "Test if FILE1 and FILE2 have identical contents."

  (clearcase-when-debugging
   (if (not (file-exists-p f1))
       (error "%s  non-existent" f1))
   (if (not (file-exists-p f2))
       (error "%s  non-existent" f2)))

  (zerop (call-process "cleardiff" nil nil nil "-status_only" f1 f2)))

(defun clearcase-diff-files (file1 file2)
  "Run cleardiff on FILE1 and FILE2 and display the differences."
  (if clearcase-use-normal-diff
      (clearcase-do-command 2
                            clearcase-normal-diff-program
                            file2
                            (append clearcase-normal-diff-arguments
                                    (list file1)))
    (clearcase-do-command 2
                          "cleardiff"
                          file2
                          (list "-diff_format" file1)))
  (let ((diff-size  (save-excursion
                      (set-buffer "*clearcase*")
                      (buffer-size))))
    (if (zerop diff-size)
        (message "No differences")
      (clearcase-port-view-buffer-other-window "*clearcase*")
      (goto-char 0)
      (shrink-window-if-larger-than-buffer))))

;;}}}

;;{{{ What rule

(defun clearcase-what-rule (file)
  (let ((result (clearcase-ct-cleartool-cmd "ls"
                                            "-d"
                                            (clearcase-path-native file))))
    (if (string-match "Rule: \\(.*\\)\n" result)
        (message (substring result
                            ;; Be a little more verbose
                            (match-beginning 0) (match-end 1)))
      (error result))))

;;}}}

;;}}}

;;{{{ File property cache

;; ClearCase properties of files are stored in a vector in a hashtable with the
;; absolute-filename (with no trailing slashes) as the lookup key.
;;
;; Properties are:
;;
;; [0] truename            : string
;; [1] mtype               : { nil, view-private-object, version,
;;                             directory-version, file-element,
;;                             dir-element, derived-object
;;                           }
;; [2] checked-out         : boolean
;; [3] reserved            : boolean
;; [4] version             : string
;; [5] predecessor-version : string
;; [6] oid                 : string
;; [7] user                : string
;; [8] date                : string (yyyymmdd.hhmmss)
;; [9] time-last-described : (N, N, N) time when the properties were last read
;;                           from ClearCase
;; [10] viewtag            : string
;; [11] comment            : string
;; [12] slink-text         : string (empty string if not symlink)
;; [13] hijacked           : boolean

;; nyi: other possible properties to record:
;;      mtime when last described (lets us know when the cached properties
;;      might be stale)

;;{{{ Debug code

(defun clearcase-fprop-unparse-properties (properties)
  "Return a string suitable for printing PROPERTIES."
  (concat
   (format "truename:            %s\n" (aref properties 0))
   (format "mtype:               %s\n" (aref properties 1))
   (format "checked-out:         %s\n" (aref properties 2))
   (format "reserved:            %s\n" (aref properties 3))
   (format "version:             %s\n" (aref properties 4))
   (format "predecessor-version: %s\n" (aref properties 5))
   (format "oid:                 %s\n" (aref properties 6))
   (format "user:                %s\n" (aref properties 7))
   (format "date:                %s\n" (aref properties 8))
   (format "time-last-described: %s\n" (current-time-string (aref properties 9)))
   (format "viewtag:             %s\n" (aref properties 10))
   (format "comment:             %s\n" (aref properties 11))
   (format "slink-text:          %s\n" (aref properties 12))
   (format "hijacked:            %s\n" (aref properties 13))))

(defun clearcase-fprop-display-properties (file)
  "Display the recorded ClearCase properties of FILE."
  (interactive "F")
  (let* ((abs-file (expand-file-name file))
         (properties (clearcase-fprop-lookup-properties abs-file)))
    (if properties
        (let ((unparsed-properties (clearcase-fprop-unparse-properties properties)))
          (clearcase-utl-populate-and-view-buffer
           "*clearcase*"
           nil
           (function (lambda ()
                       (insert unparsed-properties)))))
      (error "Properties for %s not stored" file))))

(defun clearcase-fprop-dump-to-current-buffer ()
  "Dump to the current buffer the table recording ClearCase properties of files."
  (interactive)
  (insert (format "File describe count: %s\n" clearcase-fprop-describe-count))
  (mapatoms
   (function
    (lambda (symbol)
      (let ((properties (symbol-value symbol)))
        (insert "\n"
                (format "key:                 %s\n" (symbol-name symbol))
                "\n"
                (clearcase-fprop-unparse-properties properties)))))
   clearcase-fprop-hashtable)
  (insert "\n"))

(defun clearcase-fprop-dump ()
  (interactive)
  (clearcase-utl-populate-and-view-buffer
   "*clearcase*"
   nil
   (function (lambda ()
               (clearcase-fprop-dump-to-current-buffer)))))

;;}}}

(defvar clearcase-fprop-hashtable (make-vector 31 0)
  "Obarray for per-file ClearCase properties.")

(defun clearcase-fprop-canonicalise-path (filename)
  ;; We want DIR/y and DIR\y to map to the same cache entry on ms-windows.
  ;; We want DIR and DIR/ (and on windows DIR\) to map to the same cache entry.
  ;;
  ;; However, on ms-windows avoid canonicalising X:/ to X: because, for some
  ;; reason, cleartool+desc fails on X:, but works on X:/
  ;;
  (setq filename (clearcase-path-canonicalise-slashes filename))
  (if (and clearcase-on-mswindows
           (string-match (concat "^" "[A-Za-z]:" clearcase-pname-sep-regexp "$")
                         filename))
      filename
    (clearcase-utl-strip-trailing-slashes filename)))

(defun clearcase-fprop-clear-all-properties ()
  "Delete all entries in the clearcase-fprop-hashtable."
  (setq clearcase-fprop-hashtable (make-vector 31 0)))

(defun clearcase-fprop-store-properties (file properties)
  "For FILE, store its ClearCase PROPERTIES in the clearcase-fprop-hashtable."
  (assert (file-name-absolute-p file))
  (set (intern (clearcase-fprop-canonicalise-path file)
               clearcase-fprop-hashtable) properties))

(defun clearcase-fprop-unstore-properties (file)
  "For FILE, delete its entry in the clearcase-fprop-hashtable."
  (assert (file-name-absolute-p file))
  (unintern (clearcase-fprop-canonicalise-path file) clearcase-fprop-hashtable))

(defun clearcase-fprop-lookup-properties (file)
  "For FILE, lookup and return its ClearCase properties from the
clearcase-fprop-hashtable."
  (assert (file-name-absolute-p file))
  (symbol-value (intern-soft (clearcase-fprop-canonicalise-path file)
                             clearcase-fprop-hashtable)))

(defun clearcase-fprop-get-properties (file)
  "For FILE, make sure its ClearCase properties are in the hashtable
and then return them."
  (or (clearcase-fprop-lookup-properties file)
      (let ((properties
	     (condition-case signal-info
		 (clearcase-fprop-read-properties file)
	       (error
                (progn
                  (clearcase-trace (format "(clearcase-fprop-read-properties %s) signalled error: %s"
                                           file
                                           (cdr signal-info)))
                  (make-vector 31 nil))))))
        (clearcase-fprop-store-properties file properties)
        properties)))

(defun clearcase-fprop-truename (file)
  "For FILE, return its \"truename\" ClearCase property."
  (aref (clearcase-fprop-get-properties file) 0))

(defun clearcase-fprop-mtype (file)
  "For FILE, return its \"mtype\" ClearCase property."
  (aref (clearcase-fprop-get-properties file) 1))

(defun clearcase-fprop-checked-out (file)
  "For FILE, return its \"checked-out\" ClearCase property."
  (aref (clearcase-fprop-get-properties file) 2))

(defun clearcase-fprop-reserved (file)
  "For FILE, return its \"reserved\" ClearCase property."
  (aref (clearcase-fprop-get-properties file) 3))

(defun clearcase-fprop-version (file)
  "For FILE, return its \"version\" ClearCase property."
  (aref (clearcase-fprop-get-properties file) 4))

(defun clearcase-fprop-predecessor-version (file)
  "For FILE, return its \"predecessor-version\" ClearCase property."
  (aref (clearcase-fprop-get-properties file) 5))

(defun clearcase-fprop-oid (file)
  "For FILE, return its \"oid\" ClearCase property."
  (aref (clearcase-fprop-get-properties file) 6))

(defun clearcase-fprop-user (file)
  "For FILE, return its \"user\" ClearCase property."
  (aref (clearcase-fprop-get-properties file) 7))

(defun clearcase-fprop-date (file)
  "For FILE, return its \"date\" ClearCase property."
  (aref (clearcase-fprop-get-properties file) 8))

(defun clearcase-fprop-time-last-described (file)
  "For FILE, return its \"time-last-described\" ClearCase property."
  (aref (clearcase-fprop-get-properties file) 9))

(defun clearcase-fprop-viewtag (file)
  "For FILE, return its \"viewtag\" ClearCase property."
  (aref (clearcase-fprop-get-properties file) 10))

(defun clearcase-fprop-comment (file)
  "For FILE, return its \"comment\" ClearCase property."
  (aref (clearcase-fprop-get-properties file) 11))

(defun clearcase-fprop-vob-slink-text (file)
  "For FILE, return its \"slink-text\" ClearCase property."
  (aref (clearcase-fprop-get-properties file) 12))

(defun clearcase-fprop-hijacked (file)
  "For FILE, return its \"hijacked\" ClearCase property."
  (aref (clearcase-fprop-get-properties file) 13))

(defun clearcase-fprop-set-comment (file comment)
  "For FILE, set its \"comment\" ClearCase property to COMMENT."
  (aset (clearcase-fprop-get-properties file) 11 comment))

(defun clearcase-fprop-owner-of-checkout (file)
  "For FILE, return whether the current user has it checked-out."
  (if (clearcase-fprop-checked-out file)
      (clearcase-fprop-user file)
    nil))

(defun clearcase-fprop-file-is-vob-slink-p (object-name)
  (not (zerop (length (clearcase-fprop-vob-slink-text object-name)))))

(defun clearcase-fprop-file-is-version-p (object-name)
  (if object-name
      (let ((mtype (clearcase-fprop-mtype object-name)))
        (or (eq 'version mtype)
            (eq 'directory-version mtype)))))

;; Read the object's ClearCase properties using cleartool and the Lisp reader.
;;
;; nyi: for some reason the \n before the %c necessary here so avoid confusing the
;;      cleartool/tq interface.  Completely mysterious. Arrived at by
;;      trial and error.
;;
(defvar clearcase-fprop-fmt-string

  ;; Yuck.  Different forms of quotation are needed here apparently to deal with
  ;; all the various ways of spawning sub-process on the the various platforms
  ;; (XEmacs vs. GnuEmacs, Win32 vs. Unix, Cygwin-built vs. native-built).
  ;;
  (if clearcase-on-mswindows
      (if clearcase-xemacs-p
          ;; XEmacs/Windows
          ;;
	  (if clearcase-on-cygwin
	      ;; Cygwin build
	      ;;
	      "[nil \\\"%m\\\" \\\"%f\\\" \\\"%Rf\\\" \\\"%Sn\\\" \\\"%PSn\\\" \\\"%On\\\" \\\"%u\\\" \\\"%Nd\\\" nil nil nil \\\"%[slink_text]p\\\"  nil ]\\n%c"
	    ;; Native build
	    ;;
            "[nil \\\"%m\\\" \\\"%f\\\" \\\"%Rf\\\" \\\"%Sn\\\" \\\"%PSn\\\" \\\"%On\\\" \\\"%u\\\" \\\"%Nd\\\" nil nil nil \\\"%[slink_text]p\\\" nil]\n%c")

        ;; GnuEmacs/Windows
        ;;
        "[nil \"%m\" \"%f\" \"%Rf\" \"%Sn\" \"%PSn\" \"%On\" \"%u\" \"%Nd\" nil nil nil \"%[slink_text]p\" nil]\\n%c")

    ;; Unix
    ;;
    "'[nil \"%m\" \"%f\" \"%Rf\" \"%Sn\" \"%PSn\" \"%On\" \"%u\" \"%Nd\" nil nil nil \"%[slink_text]p\" nil]\\n%c'")

  "Format for cleartool+describe command when reading the
ClearCase properties of a file")

(defvar clearcase-fprop-describe-count 0
  "Count the number of times clearcase-fprop-read-properties is called")

(defun clearcase-fprop-read-properties (file)
  "Invoke the cleartool+describe command to obtain the ClearCase
properties of FILE."
  (assert (file-name-absolute-p file))
  (let* ((truename (clearcase-fprop-canonicalise-path (file-truename (expand-file-name file)))))

    ;; If the object doesn't exist, signal an error
    ;;
    (if (or (not (file-exists-p (clearcase-vxpath-element-part file)))
            (not (file-exists-p (clearcase-vxpath-element-part truename))))
        (error "File doesn't exist: %s" file)

      ;; Run cleartool+ describe and capture the output as a string:
      ;;
      (let ((desc-string (clearcase-ct-cleartool-cmd "desc"
                                                     "-fmt"
                                                     clearcase-fprop-fmt-string
                                                     (clearcase-path-native truename))))
        (setq clearcase-fprop-describe-count (1+ clearcase-fprop-describe-count))

        ;;(clearcase-trace (format "desc of %s <<<<" truename))
        ;;(clearcase-trace desc-string)
        ;;(clearcase-trace (format "desc of %s >>>>" truename))

        ;; Read all but the comment, using the Lisp reader, and then copy
        ;; what's left as the comment.  We don't try to use the Lisp reader to
        ;; fetch the comment to avoid problems with quotation.
        ;;
        ;; nyi: it would be nice if we could make cleartool use "/" as pname-sep,
        ;;      because read-from-string will barf on imbedded "\".  For now
        ;;      run clearcase-path-canonicalise-slashes over the cleartool
        ;;      output before invoking the Lisp reader.
        ;;
        (let* ((first-read (read-from-string (clearcase-path-canonicalise-slashes desc-string)))
               (result (car first-read))
               (bytes-read (cdr first-read))
               (comment (substring desc-string (1+ bytes-read)))) ;; skip \n

          ;; Plug in the slots I left empty:
          ;;
          (aset result 0 truename)
          (aset result 9 (current-time))

          (aset result 11 comment)

          ;; Convert mtype to an enumeration:
          ;;
          (let ((mtype-string (aref result 1)))
            (cond
             ((string= mtype-string "version")
              (aset result 1 'version))

             ((string= mtype-string "directory version")
              (aset result 1 'directory-version))

             ((string= mtype-string "view private object")
              (aset result 1 'view-private-object)

              ;; If we're in a snapshot see if it is hijacked by running
              ;; ct+desc FILE@@. No error indicates it's hijacked.
              ;;
              (if (clearcase-file-would-be-in-snapshot-p truename)
                  (aset result 13
                        (condition-case nil
                            (stringp
                             (clearcase-ct-cleartool-cmd
                              "desc"
                              "-short"
                              (concat (clearcase-path-native truename)
                                      clearcase-vxpath-glue)))
                          (error nil)))))

             ((string= mtype-string "file element")
              (aset result 1 'file-element))

             ((string= mtype-string "directory element")
              (aset result 1 'directory-element))

             ((string= mtype-string "derived object")
              (aset result 1 'derived-object))

             ;; For now treat checked-in DOs as versions.
             ;;
             ((string= mtype-string "derived object version")
              (aset result 1 'version))

             ;; On NT, coerce the mtype of symlinks into that
             ;; of their targets.
             ;;
             ;; nyi: I think this is approximately right.
             ;;
             ((and (string= mtype-string "symbolic link")
                   clearcase-on-mswindows)
              (if (file-directory-p truename)
                  (aset result 1 'directory-version)
                (aset result 1 'version)))

             ;; We get this on paths like foo.c@@/main
             ;;
             ((string= mtype-string "branch")
              (aset result 1 'branch))

             ((string= mtype-string "**null meta type**")
              (aset result 1 nil))

             (t
              (error "Unknown mtype returned by cleartool+describe: %s"
                     mtype-string))))

          ;; nyi: possible efficiency win: only evaluate the viewtag on demand.
          ;;
          (if (aref result 1)
              (aset result 10 (clearcase-file-viewtag truename)))

          ;; Convert checked-out field to boolean:
          ;;
          (aset result 2 (not (zerop (length (aref result 2)))))

          ;; Convert reserved field to boolean:
          ;;
          (aset result 3 (string= "reserved" (aref result 3)))

          ;; Return the array of properties.
          ;;
          result)))))

;;}}}

;;{{{ View property cache

;; ClearCase properties of views are stored in a vector in a hashtable
;; with the viewtag as the lookup key.
;;
;; Properties are:
;;
;; [0] ucm                 : boolean
;; [1] stream              : string
;; [2] pvob                : string
;; [3] activities          : list of strings
;; [4] current-activity    : string

;;{{{ Debug code

(defun clearcase-vprop-dump-to-current-buffer ()
  "Dump to the current buffer the table recording ClearCase properties of views."
  (insert (format "View describe count: %s\n" clearcase-vprop-describe-count))
  (mapatoms
   (function
    (lambda (symbol)
      (let ((properties (symbol-value symbol)))
        (insert "\n"
                (format "viewtag:             %s\n" (symbol-name symbol))
                "\n"
                (clearcase-vprop-unparse-properties properties)))))
   clearcase-vprop-hashtable)
  (insert "\n"))

(defun clearcase-vprop-dump ()
  (interactive)
  (clearcase-utl-populate-and-view-buffer
   "*clearcase*"
   nil
   (function (lambda ()
               (clearcase-vprop-dump-to-current-buffer)))))

(defun clearcase-vprop-unparse-properties (properties)
  "Return a string suitable for printing PROPERTIES."
  (concat
   (format "ucm:                 %s\n" (aref properties 0))
   (format "stream:              %s\n" (aref properties 1))
   (format "pvob:                %s\n" (aref properties 2))
   (format "activities:          %s\n" (aref properties 3))
   (format "current-activity:    %s\n" (aref properties 4))))

;;}}}

;;{{{ Asynchronously fetching view properties:

(defvar clearcase-vprop-timer nil)
(defvar clearcase-vprop-work-queue nil)

(defun clearcase-vprop-schedule-work (viewtag)
  ;; Add to the work queue.
  ;;
  (setq clearcase-vprop-work-queue (cons viewtag
                                             clearcase-vprop-work-queue))
  ;; Create the timer if necessary.
  ;;
  (if (null clearcase-vprop-timer)
      (if clearcase-xemacs-p
          ;; Xemacs
          ;;
          (setq clearcase-vprop-timer
                (run-with-idle-timer 5 t 'clearcase-vprop-timer-function))
        ;; FSF Emacs
        ;;
        (progn
          (setq clearcase-vprop-timer (timer-create))
          (timer-set-function clearcase-vprop-timer 'clearcase-vprop-timer-function)
          (timer-set-idle-time clearcase-vprop-timer 5)
          (timer-activate-when-idle clearcase-vprop-timer)))))

(defun clearcase-vprop-timer-function ()
  ;; Process the work queue and empty it.
  ;;
  (mapcar (function (lambda (viewtag)
                      (clearcase-vprop-get-properties viewtag)))
          clearcase-vprop-work-queue)
  (setq clearcase-vprop-work-queue nil)

  ;; Cancel the timer.
  ;;
  (cancel-timer clearcase-vprop-timer)
  (setq clearcase-vprop-timer nil))

;;}}}

(defvar clearcase-vprop-hashtable (make-vector 31 0)
  "Obarray for per-view ClearCase properties.")

(defun clearcase-vprop-clear-all-properties ()
  "Delete all entries in the clearcase-vprop-hashtable."
  (setq clearcase-vprop-hashtable (make-vector 31 0)))

(defun clearcase-vprop-store-properties (viewtag properties)
  "For VIEW, store its ClearCase PROPERTIES in the clearcase-vprop-hashtable."
  (set (intern viewtag clearcase-vprop-hashtable) properties))

(defun clearcase-vprop-unstore-properties (viewtag)
  "For VIEWTAG, delete its entry in the clearcase-vprop-hashtable."
  (unintern viewtag clearcase-vprop-hashtable))

(defun clearcase-vprop-lookup-properties (viewtag)
  "For VIEWTAG, lookup and return its ClearCase properties from the
clearcase-vprop-hashtable."
  (symbol-value (intern-soft viewtag clearcase-vprop-hashtable)))

(defun clearcase-vprop-get-properties (viewtag)
  "For VIEWTAG, make sure it's ClearCase properties are in the hashtable
and then return them."
  (or (clearcase-vprop-lookup-properties viewtag)
      (let ((properties (clearcase-vprop-read-properties viewtag)))
        (clearcase-vprop-store-properties viewtag properties)
        properties)))

(defun clearcase-vprop-ucm (viewtag)
  "For VIEWTAG, return its \"ucm\" ClearCase property."
  (aref (clearcase-vprop-get-properties viewtag) 0))

(defun clearcase-vprop-stream (viewtag)
  "For VIEWTAG, return its \"stream\" ClearCase property."
  (aref (clearcase-vprop-get-properties viewtag) 1))

(defun clearcase-vprop-pvob (viewtag)
  "For VIEWTAG, return its \"stream\" ClearCase property."
  (aref (clearcase-vprop-get-properties viewtag) 2))

(defun clearcase-vprop-activities (viewtag)
  "For VIEWTAG, return its \"activities\" ClearCase property."

  ;; If the activity set has been flushed, go and schedule a re-fetch.
  ;;
  (let ((properties (clearcase-vprop-get-properties viewtag)))
    (if (null (aref properties 3))
        (aset properties 3 (clearcase-vprop-read-activities-asynchronously viewtag))))

  ;; Now poll, waiting for the activities to be available.
  ;;
  (let ((loop-count 0))
    ;; If there is a background process still reading the activities,
    ;; wait for it to finish.
    ;;
    ;; nyi: probably want a timeout here.
    ;;
    ;; nyi: There seems to be a race on NT in accept-process-output so that
    ;;      we would wait forever.
    ;;
    (if (not clearcase-on-mswindows)
        ;; Unix synchronization with the end of the process
        ;; which is reading activities.
        ;;
        (while (bufferp (aref (clearcase-vprop-get-properties viewtag) 3))
          (save-excursion
            (set-buffer (aref (clearcase-vprop-get-properties viewtag) 3))
            (message "Reading activity list...")
            (setq loop-count (1+ loop-count))
            (accept-process-output clearcase-vprop-async-proc)))

      ;; NT synchronization with the end of the process which is reading
      ;; activities.
      ;;
      ;; Unfortunately on NT we can't rely on the process sentinel being called
      ;; so we have to explicitly test the process status.
      ;;
      (while (bufferp (aref (clearcase-vprop-get-properties viewtag) 3))
        (message "Reading activity list...")
        (save-excursion
          (set-buffer (aref (clearcase-vprop-get-properties viewtag) 3))
          (if (or (not (processp clearcase-vprop-async-proc))
                  (eq 'exit (process-status clearcase-vprop-async-proc)))

              ;; The process has finished or gone away and apparently
              ;; the sentinel didn't get called which would have called
              ;; clearcase-vprop-finish-reading-activities, so call it
              ;; explicitly here.
              ;;
              (clearcase-vprop-finish-reading-activities (current-buffer))

            ;; The process is apparently still running, so wait
            ;; so more.
            (setq loop-count (1+ loop-count))
            (sit-for 1)))))

    (if (not (zerop loop-count))
        (message "Reading activity list...done"))

    (aref (clearcase-vprop-get-properties viewtag) 3)))

(defun clearcase-vprop-current-activity (viewtag)
  "For VIEWTAG, return its \"current-activity\" ClearCase property."
  (aref (clearcase-vprop-get-properties viewtag) 4))

(defun clearcase-vprop-set-activities (viewtag activities)
  "For VIEWTAG, set its \"activities\" ClearCase property to ACTIVITIES."
  (let ((properties (clearcase-vprop-lookup-properties viewtag)))
    ;; We must only set the activities for an existing vprop entry.
    ;;
    (assert properties)
    (aset properties 3 activities)))

(defun clearcase-vprop-flush-activities (viewtag)
  "For VIEWTAG, set its \"activities\" ClearCase property to nil,
to cause a future re-fetch."
  (clearcase-vprop-set-activities viewtag nil))

(defun clearcase-vprop-set-current-activity (viewtag activity)
  "For VIEWTAG, set its \"current-activity\" ClearCase property to ACTIVITY."
  (aset (clearcase-vprop-get-properties viewtag) 4 activity))

;; Read the object's ClearCase properties using cleartool lsview and cleartool lsstream.

(defvar clearcase-vprop-describe-count 0
  "Count the number of times clearcase-vprop-read-properties is called")

(defvar clearcase-lsstream-fmt-string
  (if clearcase-on-mswindows
      (if clearcase-xemacs-p
          ;; XEmacs/Windows
          ;;
	  (if clearcase-on-cygwin
	      ;; Cygwin build
	      ;;
	      "[\\\"%n\\\"  \\\"%[master]p\\\" ]"
	    ;; Native build
	    ;;
	    "[\\\"%n\\\"  \\\"%[master]p\\\" ]")
        ;; GnuEmacs/Windows
        ;;
        "[\"%n\"  \"%[master]p\" ]")
    ;; Unix
    ;;
    "'[\"%n\"  \"%[master]p\" ]'"))

(defun clearcase-vprop-read-properties (viewtag)
  "Invoke cleartool commands to obtain the ClearCase
properties of VIEWTAG."

  ;; We used to use "ct+lsview -properties -full TAG", but this seemed to take
  ;; a long time in some circumstances. It appears to be because the
  ;; ADM_VIEW_GET_INFO RPC can take up to 60 seconds in certain circumstances
  ;; (typically on my laptop with self-contained ClearCase region).

  ;; Accordingly, since we don't really need to store snapshotness, the minimum
  ;; we really need to discover about a view is whether it is UCM-attached. For
  ;; this the much faster ct+lsstream suffices.
  ;;
  (let* ((result (make-vector 5 nil)))
    (if (not clearcase-v3)
        (let ((ucm nil)
              (stream nil)
              (pvob nil)
              (activity-names nil)
              (activity-titles nil)
              (activities nil)
              (current-activity nil)
              (ret ""))

          ;; This was necessary to make sure the "done" message was always
          ;; displayed.  Not quite sure why.
          ;;
          (unwind-protect
              (progn
                (message "Reading view properties...")
                (setq ret (clearcase-ct-blocking-call "lsstream" "-fmt"
                                                      clearcase-lsstream-fmt-string
                                                      "-view" viewtag))

                (setq clearcase-vprop-describe-count (1+ clearcase-vprop-describe-count))

                (if (setq ucm (not (zerop (length ret))))

                    ;; It's apparently a UCM view
                    ;;
                    (let* ((first-read (read-from-string (clearcase-utl-escape-backslashes ret)))
                           (array-read (car first-read))
                           (bytes-read (cdr first-read)))

                      ;; Get stream name
                      ;;
                      (setq stream (aref array-read 0))

                      ;; Get PVOB tag from something like "unix@/vobs/projects"
                      ;;
                      (let ((s (aref array-read 1)))
                        (if (string-match "@" s)
                            (setq pvob (substring s (match-end 0)))
                          (setq pvob s)))

                      ;; Get the activity list and store as a list of (NAME . TITLE) pairs
                      ;;
                      (setq activities (clearcase-vprop-read-activities-asynchronously viewtag))

                      ;; Get the current activity
                      ;;
                      (let ((name-string (clearcase-ct-blocking-call "lsact" "-cact" "-fmt" "%n"
                                                                     "-view" viewtag)))
                        (if (not (zerop (length name-string)))
                            (setq current-activity name-string)))

                      (aset result 0 ucm)
                      (aset result 1 stream)
                      (aset result 2 pvob)
                      (aset result 3 activities)
                      (aset result 4 current-activity))))

            (message "Reading view properties...done"))))

    result))

(defvar clearcase-vprop-async-viewtag nil)
(defvar clearcase-vprop-async-proc nil)
(defun clearcase-vprop-read-activities-asynchronously (viewtag)
  (let ((buf-name (format "*clearcase-activities-%s*" viewtag)))
    ;; Clean up old instance of the buffer we use to fetch activities:
    ;;
    (let ((buf (get-buffer buf-name)))
      (if buf
          (progn
            (save-excursion
              (set-buffer buf)
              (if (and (boundp 'clearcase-vprop-async-proc)
                       clearcase-vprop-async-proc)
                  (condition-case nil
                      (kill-process clearcase-vprop-async-proc)
                    (error nil))))
            (kill-buffer buf))))

    ;; Create a buffer and an associated new process to read activities in the
    ;; background. We return the buffer to be stored in the activities field of
    ;; the view-properties record. The function clearcase-vprop-activities will
    ;; recognise when the asynch fetching is still underway and wait for it to
    ;; finish.
    ;;
    ;; The process has a sentinel function which is supposed to get called when
    ;; the process finishes. This sometimes doesn't happen on Windows, so that
    ;; clearcase-vprop-activities has to do a bit more work.  (Perhaps a race
    ;; exists: the process completes before the sentinel can be set ?)
    ;;
    (let* ((buf (get-buffer-create buf-name))
           (proc (start-process (format "*clearcase-activities-process-%s*" viewtag)
                                buf
                                clearcase-cleartool-path
                                "lsact" "-view" viewtag)))
      (process-kill-without-query proc)
      (save-excursion
        (set-buffer buf)
        ;; Create a sentinel to parse and store the activities when the
        ;; process finishes. We record the viewtag as a buffer-local
        ;; variable so the sentinel knows where to store the activities.
        ;;
        (set (make-local-variable 'clearcase-vprop-async-viewtag) viewtag)
        (set (make-local-variable 'clearcase-vprop-async-proc) proc)
        (set-process-sentinel proc 'clearcase-vprop-read-activities-sentinel))
      ;; Return the buffer.
      ;;
      buf)))

(defun clearcase-vprop-read-activities-sentinel (process event-string)
  (clearcase-trace "Activity reading process sentinel called")
  (if (not (equal "finished\n" event-string))
      ;; Failure
      ;;
      (error "Reading activities failed: %s" event-string))
  (clearcase-vprop-finish-reading-activities (process-buffer process)))

(defun clearcase-vprop-finish-reading-activities (buffer)
  (let ((activity-list nil))
    (message "Parsing view activities...")
    (save-excursion
      (set-buffer buffer)
      (if (or (not (boundp 'clearcase-vprop-async-viewtag))
              (null clearcase-vprop-async-viewtag))
          (error "Internal error: clearcase-vprop-async-viewtag not set"))

      ;; Check that our buffer is the one currently expected to supply the
      ;; activities. (Avoid races.)
      ;;
      (let ((properties (clearcase-vprop-lookup-properties clearcase-vprop-async-viewtag)))
        (if (and properties
                 (eq buffer (aref properties 3)))
            (progn

              ;; Parse the buffer, slicing out the 2nd and 4th fields as name and title.
              ;;
              (goto-char (point-min))
              (while (re-search-forward "^[^ \t]+[ \t]+\\([^ \t]+\\)[ \t]+[^ \t]+[ \t]+\"+\\(.*\\)\"$" nil t)
                (let ((id (buffer-substring (match-beginning 1)
                                            (match-end 1)))
                      (title (buffer-substring (match-beginning 2)
                                               (match-end 2))))
                  (setq activity-list (cons (cons id title)
                                            activity-list))))

              ;; We've got activity-list in the reverse order that
              ;; cleartool+lsactivity generated them.  I think this is reverse
              ;; chronological order, so keep this order since it is more
              ;; convenient when setting to an activity.
              ;;
              ;;(setq activity-list (nreverse activity-list))

              (clearcase-vprop-set-activities clearcase-vprop-async-viewtag activity-list))

          (kill-buffer buffer))))
    (message "Parsing view activities...done")))

;;{{{ old synchronous activity reader

;; (defun clearcase-vprop-read-activities-synchronously (viewtag)
;;   "Return a list of (activity-name . title) pairs for VIEWTAG"
;;   ;; nyi: ought to use a variant of clearcase-ct-blocking-call that returns a buffer
;;   ;;      rather than a string

;;   ;; Performance: takes around 30 seconds to read 1000 activities.
;;   ;; Too slow to invoke willy-nilly on integration streams for example,
;;   ;; which typically can have 1000+ activities.

;;   (let ((ret (clearcase-ct-blocking-call "lsact" "-view" viewtag)))
;;     (let ((buf (get-buffer-create "*clearcase-temp-activities*"))
;;           (activity-list nil))
;;       (save-excursion
;;         (set-buffer buf)
;;         (erase-buffer)
;;         (insert ret)
;;         (goto-char (point-min))
;;         ;; Slice out the 2nd and 4th fields as name and title
;;         ;;
;;         (while (re-search-forward "^[^ \t]+[ \t]+\\([^ \t]+\\)[ \t]+[^ \t]+[ \t]+\"+\\(.*\\)\"$" nil t)
;;           (setq activity-list (cons (cons (buffer-substring (match-beginning 1)
;;                                                             (match-end 1))
;;                                           (buffer-substring (match-beginning 2)
;;                                                             (match-end 2)))
;;                                     activity-list)))
;;         (kill-buffer buf))

;;       ;; We've got activity-list in the reverse order that
;;       ;; cleartool+lsactivity generated them.  I think this is reverse
;;       ;; chronological order, so keep this order since it is more
;;       ;; convenient when setting to an activity.
;;       ;;
;;       ;;(nreverse activity-list))))
;;       activity-list)))

;;}}}

;;}}}

;;{{{ Determining if a checkout was modified.

;; How to tell if a file changed since checkout ?
;;
;; In the worst case we actually run "ct diff -pred" but we attempt several
;; less expensive tests first.
;;
;;  1. If it's size differs from pred.
;;  2. The mtime and the ctime are no longer the same.
;;
;; nyi: Other cheaper tests we could use:
;;
;;  (a) After each Emacs-driven checkout go and immediately fetch the mtime of
;;      the file and store as fprop-checkout-mtime. Then use that to compare
;;      against current mtime. This at least would make this function work
;;      right on files checked out by the current Emacs process.
;;
;;  (b) In the MVFS, after each Emacs-driven checkout go and immediately fetch
;;      the OID and store as fprop-checkout-oid. Then use that to compare
;;      against the current oid (the MVFS assigns a new OID at each write).
;;      This might not always be a win since we'd still need to run cleartool
;;      to get the current OID.

(defun clearcase-file-appears-modified-since-checkout-p (file)
  "Return whether FILE appears to have been modified since checkout.
It doesn't examine the file contents."

  (if (not (clearcase-fprop-checked-out file))
      nil

    (let ((mvfs (clearcase-file-is-in-mvfs-p file)))

      ;; We consider various cases in order of increasing cost to compute.

      (cond
       ;; Case 1: (MVFS only) the size is different to its predecessor.
       ;;
       ((and mvfs
             (not
              (equal
               (clearcase-utl-file-size file)
               ;; nyi: For the snapshot case it'd be nice to get the size of the
               ;;      predecessor by using "ct+desc -pred -fmt" but there doesn't
               ;;      seem to be a format descriptor for file size. On the other hand
               ;;      ct+dump can obtain the size.
               ;;
               (clearcase-utl-file-size (clearcase-vxpath-cons-vxpath
                                         file
                                         (clearcase-fprop-predecessor-version
                                          file)))))
             ;; Return:
             ;;
             'size-changed))

       ;; Case 2: (MVFS only) the mtime and the ctime are no longer the same.
       ;;
       ;; nyi: At least on Windows there seems to be a small number of seconds
       ;;      difference here even when the file is not modified.
       ;;      So we really check to see of they are close.
       ;;
       ;; nyi: This doesn't work in a snapshot view.
       ;;
       ((and mvfs
             (not (clearcase-utl-filetimes-close (clearcase-utl-file-mtime file)
                                                 (clearcase-utl-file-ctime file)
                                                 5))
             ;; Return:
             ;;
             'ctime-mtime-not-close))

       (t
        ;; Case 3: last resort. Actually run a diff against predecessor.
        ;;
        (let ((ret (clearcase-ct-blocking-call "diff"
                                               "-options"
                                               "-quiet"
                                               "-pred"
                                               file)))
          (if (not (zerop (length ret)))
              ;; Return:
              ;;
              'diffs-nonempty

            ;; Return:
            ;;
            nil)))))))

;;}}}

;;{{{ Tests for view-residency

;;{{{ Tests for MVFS file residency

;; nyi: probably superseded by clearcase-file-would-be-in-view-p
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; nyi: this should get at least partially invalidated when
;;          VOBs are unmounted.

;; nyi: make this different for NT
;;
(defvar clearcase-always-mvfs-regexp (if (not clearcase-on-mswindows)
                                         "^/vobs/[^/]+/"

                                       ;; nyi: express this using drive variable
                                       ;;
                                       (concat "^"
                                               "[Mm]:"
                                               clearcase-pname-sep-regexp)))

;; This prevents the clearcase-file-vob-root function from pausing for long periods
;; stat-ing /net/host@@
;;
;; nyi: is there something equivalent on NT I need to avoid ?
;;

(defvar clearcase-never-mvfs-regexps (if clearcase-on-mswindows
                                         nil
                                       '(
                                         "^/net/[^/]+/"
                                         "^/tmp_mnt/net/[^/]+/"
                                         ))
  "Regexps matching those paths we can assume are never inside the MVFS.")

(defvar clearcase-known-vob-root-cache nil)

(defun clearcase-file-would-be-in-mvfs-p (filename)
  "Return whether FILE, after it is created, would reside in an MVFS filesystem."
  (let ((truename (file-truename filename)))
    (if (file-exists-p truename)
        (clearcase-file-is-in-mvfs-p truename)
      (let ((containing-dir (file-name-as-directory (file-name-directory truename))))
        (clearcase-file-is-in-mvfs-p containing-dir)))))

(defun clearcase-file-is-in-mvfs-p (filename)
  "Return whether existing FILE, resides in an MVFS filesystem."
  (let ((truename (file-truename filename)))

    (or
     ;; case 1: its prefix matches an "always VOB" prefix like /vobs/...
     ;;
     ;; nyi: problem here: we return true for "/vobs/nonexistent/"
     ;;
     (numberp (string-match clearcase-always-mvfs-regexp truename))

     ;; case 2: it has a prefix which is a known VOB-root
     ;;
     (clearcase-file-matches-vob-root truename clearcase-known-vob-root-cache)

     ;; case 3: it has an ancestor dir which is a newly met VOB-root
     ;;
     (clearcase-file-vob-root truename))))

(defun clearcase-wd-is-in-mvfs ()
  "Return whether the current directory resides in an MVFS filesystem."
  (clearcase-file-is-in-mvfs-p (file-truename ".")))

(defun clearcase-file-matches-vob-root (truename vob-root-list)
  "Return whether TRUENAME has a prefix in VOB-ROOT-LIST."
  (if (null vob-root-list)
      nil
    (or (numberp (string-match (regexp-quote (car vob-root-list))
                               truename))
        (clearcase-file-matches-vob-root truename (cdr vob-root-list)))))

(defun clearcase-file-vob-root (truename)
  "File the highest versioned directory in TRUENAME."

  ;; Use known non-MVFS patterns to rule some paths out.
  ;;
  (if (apply (function clearcase-utl-or-func)
             (mapcar (function (lambda (regexp)
                                 (string-match regexp truename)))
                     clearcase-never-mvfs-regexps))
      nil
    (let ((previous-dir nil)
          (dir  (file-name-as-directory (file-name-directory truename)))
          (highest-versioned-directory nil))

      (while (not (string-equal dir previous-dir))
        (if (clearcase-file-covers-element-p dir)
            (setq highest-versioned-directory dir))
        (setq previous-dir dir)
        (setq dir (file-name-directory (directory-file-name dir))))

      (if highest-versioned-directory
          (add-to-list 'clearcase-known-vob-root-cache highest-versioned-directory))

      highest-versioned-directory)))

;; Note: you should probably be using clearcase-fprop-mtype instead of this
;;       unless you really know what you're doing (nyi: check usages of this.)
;;
(defun clearcase-file-covers-element-p (path)
  "Determine quickly if PATH refers to a Clearcase element,
without caching the result."

  ;; nyi: Even faster: consult the fprop cache first ?

  (let ((element-dir (concat (clearcase-vxpath-element-part path) clearcase-vxpath-glue)))
    (and (file-exists-p path)
         (file-directory-p element-dir))))

;;}}}

;;{{{ Tests for snapshot view residency

;; nyi: probably superseded by clearcase-file-would-be-in-view-p
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar clearcase-known-snapshot-root-cache nil)

(defun clearcase-file-would-be-in-snapshot-p (filename)
  "Return whether FILE, after it is created, would reside in a snapshot view.
If so, return the viewtag."
  (let ((truename (file-truename filename)))
    (if (file-exists-p truename)
        (clearcase-file-is-in-snapshot-p truename)
      (let ((containing-dir (file-name-as-directory (file-name-directory truename))))
        (clearcase-file-is-in-snapshot-p containing-dir)))))

(defun clearcase-file-is-in-snapshot-p (truename)
  "Return whether existing FILE, resides in a snapshot view.
If so, return the viewtag."

  (or
   ;; case 1: it has a prefix which is a known snapshot-root
   ;;
   (clearcase-file-matches-snapshot-root truename clearcase-known-snapshot-root-cache)

   ;; case 2: it has an ancestor dir which is a newly met VOB-root
   ;;
   (clearcase-file-snapshot-root truename)))

(defun clearcase-wd-is-in-snapshot ()
  "Return whether the current directory resides in a snapshot view."
  (clearcase-file-is-in-snapshot-p (file-truename ".")))

(defun clearcase-file-matches-snapshot-root (truename snapshot-root-list)
  "Return whether TRUENAME has a prefix in SNAPSHOT-ROOT-LIST."
  (if (null snapshot-root-list)
      nil
    (or (numberp (string-match (regexp-quote (car snapshot-root-list))
                               truename))
        (clearcase-file-matches-snapshot-root truename (cdr snapshot-root-list)))))

;; This prevents the clearcase-file-snapshot-root function from pausing for long periods
;; stat-ing /net/host@@
;;
;; nyi: is there something equivalent on NT I need to avoid ?
;;

(defvar clearcase-never-snapshot-regexps (if clearcase-on-mswindows
                                             nil
                                           '(
                                             "^/net/[^/]+/"
                                             "^/tmp_mnt/net/[^/]+/"
                                             ))
  "Regexps matching those paths we can assume are never inside a snapshot view.")

(defun clearcase-file-snapshot-root (truename)
  "File the the snapshot view root containing TRUENAME."

  ;; Use known non-snapshot patterns to rule some paths out.
  ;;
  (if (apply (function clearcase-utl-or-func)
             (mapcar (function (lambda (regexp)
                                 (string-match regexp truename)))
                     clearcase-never-snapshot-regexps))
      nil
    (let ((previous-dir nil)
          (dir (file-name-as-directory (file-name-directory truename)))
          (viewtag nil)
          (viewroot nil))


      (while (and (not (string-equal dir previous-dir))
                  (null viewtag))

        ;; See if .view.dat exists and contains a valid view uuid
        ;;
        (let ((view-dat-name (concat dir (if clearcase-on-mswindows
					     "view.dat" ".view.dat"))))
          (if (file-readable-p view-dat-name)
              (let ((uuid (clearcase-viewdat-to-uuid view-dat-name)))
                (if uuid
                    (progn
                      (setq viewtag (clearcase-view-uuid-to-tag uuid))
                      (if viewtag
                          (setq viewroot dir)))))))

        (setq previous-dir dir)
        (setq dir (file-name-directory (directory-file-name dir))))

      (if viewroot
          (add-to-list 'clearcase-known-snapshot-root-cache viewroot))

      ;; nyi: update a viewtag==>viewroot map ?

      viewroot)))

(defun clearcase-viewdat-to-uuid (file)
  "Extract the view-uuid from a .view.dat file."
  ;; nyi, but return non-nil so clearcase-file-snapshot-root works
  t
  )

(defun clearcase-view-uuid-to-tag (uuid)
  "Look up the view-uuid in the register to discover its tag."
  ;; nyi, but return non-nil so clearcase-file-snapshot-root works
  t
  )

;;}}}

;; This is simple-minded but seems to work because cleartool+describe
;; groks snapshot views.
;;
;; nyi: Might be wise to cache view-roots to speed this up because the
;;      filename-handlers call this.
;;
;; nyi: Some possible shortcuts
;;      1. viewroot-relative path [syntax]
;;      2. under m:/ on NT        [syntax]
;;      3. setviewed on Unix      [find a containing VOB-root]
;;      4. subst-ed view on NT (calling net use seems very slow though)
;;                                [find a containing VOB-root]
;;      5. snapshot view
;;
(defun clearcase-file-would-be-in-view-p (filename)
  "Return whether FILE, after it is created, would reside in a ClearCase view."
  (let  ((truename (file-truename (expand-file-name filename))))

    ;; We use clearcase-path-file-really-exists-p here to make sure we are dealing
    ;; with a real file and not something faked by Emacs' file name handlers
    ;; like Ange-FTP.
    ;;
    (if (clearcase-path-file-really-exists-p truename)
        (clearcase-file-is-in-view-p truename)
      (let ((containing-dir (file-name-as-directory (file-name-directory truename))))
        (and (clearcase-path-file-really-exists-p containing-dir)
             (clearcase-file-is-in-view-p containing-dir))))))

(defun clearcase-file-is-in-view-p (filename)
  (let  ((truename (file-truename (expand-file-name filename))))
    ;; Shortcut if the file is a version-extended path.
    ;;
    (or (clearcase-file-snapshot-root truename)
        (clearcase-vxpath-p truename)
        (clearcase-fprop-mtype truename)

        ;; nyi: How to efficiently know if we're in a dynamic-view root
        ;;   1. Test each contained name for elementness.
        ;;      Too inefficient.
        ;;   2. If it is viewroot-relative.
        ;;      Okay but not sufficient.
        ;;      How about case v:/ when view is substed ?
        ;;   3. We're setviewed.
        ;;      Okay but not sufficient.
        ;;  Maintain a cache of viewroots ?
        )))

(defun clearcase-file-viewtag (filename)
  "Find the viewtag associated with existing FILENAME."

  (clearcase-when-debugging
   (assert (file-exists-p filename)))

  (let ((truename (file-truename (expand-file-name filename))))
    (cond

     ;; Case 1: viewroot-relative path
     ;;         ==> syntax
     ;;
     ((clearcase-vrpath-p truename)
      (clearcase-vrpath-viewtag truename))

     ;; Case 2: under m:/ on NT
     ;;         ==> syntax
     ;;
     ((and clearcase-on-mswindows
           (string-match (concat clearcase-viewroot-drive
                                 clearcase-pname-sep-regexp
                                 "\\("
                                 clearcase-non-pname-sep-regexp "*"
                                 "\\)"
                                 )
                         truename))
      (substring truename (match-beginning 1) (match-end 1)))

     ;; Case 3: setviewed on Unix
     ;;         ==> read EV, but need to check it's beneath a VOB-root
     ;;
     ((and clearcase-setview-viewtag
           (clearcase-file-would-be-in-mvfs-p truename))
      clearcase-setview-viewtag)

     ;; Case 4: subst-ed view on NT
     ;;         ==> use ct+pwv -wdview
     ;; Case 5: snapshot view
     ;;         ==> use ct+pwv -wdview
     (t
      (clearcase-file-wdview truename)))))

(defun clearcase-file-wdview (truename)
  "Return the working-directory view associated with TRUENAME,
or nil if none"
  (let ((default-directory (if (file-directory-p truename)
                               truename
                             (file-name-directory truename))))
    (clearcase-ct-cd default-directory)
    (let ((ret (clearcase-ct-blocking-call "pwv" "-wdview" "-short")))
      (if (not (string-match " NONE " ret))
          (clearcase-utl-1st-line-of-string ret)))))

;;}}}

;;{{{ The cleartool sub-process

;; We use pipes rather than pty's for two reasons:
;;
;;   1. NT only has pipes
;;   2. On Solaris there appeared to be a problem in the pty handling part
;;      of Emacs, which resulted in Emacs/tq seeing too many cleartool prompt
;;      strings. This would occasionally occur and prevent the tq-managed
;;      interactions with the cleartool sub-process from working correctly.
;;
;; Now we use pipes. Cleartool detects the "non-tty" nature of the output
;; device and doesn't send a prompt. We manufacture an end-of-transaction
;; marker by sending a "pwd -h" after each cleartool sub-command and then use
;; the expected output of "Usage: pwd\n" as our end-of-txn pattern for tq.
;;
;; Even using pipes, the semi-permanent outboard-process using tq doesn't work
;; well on NT. There appear to be bugs in accept-process-output such that:
;;   0. there apparently were hairy race conditions, which a sprinkling
;;      of (accept-process-output nil 1) seemed to avoid somewhat.
;;   1. it never seems to timeout if you name a process as arg1.
;;   2. it always seems to wait for TIMEOUT, even if there is output ready.
;; The result seemed to be less responsive tha just calling a fresh cleartool
;; process for each invocation of clearcase-ct-blocking-call
;;
;; It still seems worthwhile to make it work on NT, as clearcase-ct-blocking-call
;; typically takes about 0.5 secs on NT versus 0.05 sec on Solaris,
;; an order of magnitude difference.
;;

(defconst clearcase-ct-eotxn-cmd "pwd -h\n")
(defconst clearcase-ct-eotxn-response "Usage: pwd\n")
(defconst clearcase-ct-eotxn-response-length (length clearcase-ct-eotxn-response))

(defconst clearcase-ct-subproc-timeout 30
  "Timeout on calls to subprocess")

(defvar clearcase-ct-tq nil
  "Transaction queue to talk to ClearTool in a subprocess")

(defvar clearcase-ct-return nil
  "Return value when we're involved in a blocking call")

(defvar clearcase-ct-view ""
  "Current view of cleartool subprocess, or the empty string if none")

(defvar clearcase-ct-wdir ""
  "Current working directory of cleartool subprocess,
or the empty string if none")

(defvar clearcase-ct-running nil)

(defun clearcase-ct-accept-process-output (proc timeout)
  (accept-process-output proc timeout))

(defun clearcase-ct-start-cleartool ()
  (interactive)
  (clearcase-trace "clearcase-ct-start-cleartool()")
  (let ((process-environment (append '("ATRIA_NO_BOLD=1"
                                       "ATRIA_FORCE_GUI=1")
                                     ;;; emacs is a GUI, right? :-)
                                     process-environment)))
    (clearcase-trace (format "Starting cleartool in %s" default-directory))
    (let* ( ;; Force the use of a pipe
           ;;
           (process-connection-type nil)
           (cleartool-process
            (start-process "cleartool" ;; Absolute path won't work here
                           " *cleartool*"
                           clearcase-cleartool-path)))
      (process-kill-without-query cleartool-process)
      (setq clearcase-ct-view "")
      (setq clearcase-ct-tq (tq-create cleartool-process))
      (tq-enqueue clearcase-ct-tq
                  clearcase-ct-eotxn-cmd ;; question
                  clearcase-ct-eotxn-response ;; regexp
                  'clearcase-ct-running ;; closure
                  'set) ;; function
      (while (not clearcase-ct-running)
        (message "waiting for cleartool to start...")
        (clearcase-ct-accept-process-output (tq-process clearcase-ct-tq)
                                            clearcase-ct-subproc-timeout))
      ;; Assign a sentinel to restart it if it dies.
      ;; nyi: This needs debugging.
      ;;(set-process-sentinel cleartool-process 'clearcase-ct-sentinel)

      (clearcase-trace "clearcase-ct-start-cleartool() done")
      (message "waiting for cleartool to start...done"))))

;; nyi: needs debugging.
;;
(defun clearcase-ct-sentinel (process event-string)
  (clearcase-trace (format "Cleartool process sentinel called: %s" event-string))
  (if (not (eq 'run (process-status process)))
      (progn
        ;; Restart the dead cleartool.
        ;;
        (clearcase-trace "Cleartool process restarted")
        (clearcase-ct-start-cleartool))))

(defun clearcase-ct-kill-cleartool ()
  "Kill off cleartool subprocess.  If another one is needed,
it will be restarted.  This may be useful if you're debugging clearcase."
  (interactive)
  (clearcase-ct-kill-tq))

(defun clearcase-ct-callback (arg val)
  (clearcase-trace (format "clearcase-ct-callback:<\n"))
  (clearcase-trace val)
  (clearcase-trace (format "clearcase-ct-callback:>\n"))
  ;; This can only get called when the last thing received from
  ;; the cleartool sub-process was clearcase-ct-eotxn-response,
  ;; so it is safe to just remove it here.
  ;;
  (setq clearcase-ct-return (substring val 0 (- clearcase-ct-eotxn-response-length))))

(defun clearcase-ct-do-cleartool-command (command file comment &optional extra-args)
  "Execute a cleartool command, notifying user and checking for
errors. Output from COMMAND goes to buffer *clearcase*.  The last argument of the
command is the name of FILE; this is appended to an optional list of
EXTRA-ARGS."

  (if file
      (setq file (expand-file-name file)))
  (if (listp command)
      (error "command must not be a list"))
  (if clearcase-command-messages
      (if file
          (message "Running %s on %s..." command file)
        (message "Running %s..." command)))
  (let ((camefrom (current-buffer))
        (squeezed nil)
        status)
    (set-buffer (get-buffer-create "*clearcase*"))
    (setq buffer-read-only nil)
    (erase-buffer)
    (set (make-local-variable 'clearcase-parent-buffer) camefrom)
    (set (make-local-variable 'clearcase-parent-buffer-name)
         (concat " from " (buffer-name camefrom)))

    ;; This is so that command arguments typed in the *clearcase* buffer will
    ;; have reasonable defaults.
    ;;
    (if file
        (setq default-directory (file-name-directory file)))

    (mapcar
     (function (lambda (s)
                 (and s
                      (not (zerop (length s)))
                      (setq squeezed
                            (append squeezed (list s))))))
     extra-args)

    (clearcase-with-tempfile
     comment-file
     (if (not (eq comment 'unused))
         (if comment
             (progn
               (write-region comment nil comment-file nil 'noprint)
               (setq squeezed (append squeezed (list "-cfile" (clearcase-path-native comment-file)))))
           (setq squeezed (append squeezed (list "-nc")))))
     (if file
         (setq squeezed (append squeezed (list (clearcase-path-native file)))))
     (let ((default-directory (file-name-directory
                               (or file default-directory))))
       (clearcase-ct-cd default-directory)
       (if clearcase-command-messages
           (message "Running %s..." command))
       (insert
        (apply 'clearcase-ct-cleartool-cmd (append (list command) squeezed)))
       (if clearcase-command-messages
           (message "Running %s...done" command))))

    (goto-char (point-min))
    (clearcase-view-mode 0 camefrom)
    (set-buffer-modified-p nil)         ; XEmacs - fsf uses `not-modified'
    (if (re-search-forward "^cleartool: Error:.*$" nil t)
        (progn
          (setq status (buffer-substring (match-beginning 0) (match-end 0)))
          (clearcase-port-view-buffer-other-window "*clearcase*")
          (shrink-window-if-larger-than-buffer)
          (error "Running %s...FAILED (%s)" command status))
      (if clearcase-command-messages
          (message "Running %s...OK" command)))
    (set-buffer camefrom)
    status))

(defun clearcase-ct-cd (dir)
  (if (or (not dir)
          (string= dir clearcase-ct-wdir))
      clearcase-ct-wdir
    (clearcase-ct-blocking-call "cd" (clearcase-path-native dir))
    (setq clearcase-ct-wdir dir)))

(defun clearcase-ct-cleartool-cmd (&rest cmd)
  (apply 'clearcase-ct-blocking-call cmd))

;; NT Emacs - needs a replacement for tq.
;;
(defun clearcase-ct-get-command-stdout (program &rest args)
  "Call PROGRAM.
Returns PROGRAM's stdout.
ARGS is the command line arguments to PROGRAM."
  (let ((buf (get-buffer-create "cleartoolexecution")))
    (prog1
        (save-excursion
          (set-buffer buf)
	  (apply 'call-process program nil buf nil args)
          (buffer-string))
      (kill-buffer buf))))

;; The TQ interaction still doesn't work on NT.
;;
(defvar clearcase-disable-tq clearcase-on-mswindows
  "Set to T if the Emacs/cleartool interactions via tq are not working right.")

(defun clearcase-ct-blocking-call (&rest cmd)
  (clearcase-trace (format "clearcase-ct-blocking-call(%s)" cmd))
  (save-excursion
    (setq clearcase-ct-return nil)

    (if clearcase-disable-tq
        ;; Don't use tq:
        ;;
        (setq clearcase-ct-return (apply 'clearcase-ct-get-command-stdout
                                         clearcase-cleartool-path cmd))

      ;; Use tq:
      ;;
      (setq clearcase-ct-return nil)
      (if (not clearcase-ct-tq)
          (clearcase-ct-start-cleartool))
      (unwind-protect
          (let ((command ""))
	    (mapcar
	     (function
              (lambda (token)
                ;; If the token has imbedded spaces and is not already quoted,
                ;; add double quotes.
                ;;
                (setq command (concat command
                                      " "
                                      (clearcase-utl-quote-if-nec token)))))
	     cmd)
            (tq-enqueue clearcase-ct-tq
                        (concat command "\n"
                                clearcase-ct-eotxn-cmd) ;; question
                        clearcase-ct-eotxn-response ;; regexp
                        nil ;; closure
                        'clearcase-ct-callback) ;; function
            (while (not clearcase-ct-return)
              (clearcase-ct-accept-process-output (tq-process clearcase-ct-tq)
                                                  clearcase-ct-subproc-timeout)))
        ;; Error signalled:
        ;;
        (while (tq-queue clearcase-ct-tq)
          (tq-queue-pop clearcase-ct-tq)))))
  (if (string-match "cleartool: Error:" clearcase-ct-return)
      (error "cleartool process error %s: "
             (substring clearcase-ct-return (match-end 0))))
  (clearcase-trace (format "command-result(%s)" clearcase-ct-return))
  clearcase-ct-return)

(defun clearcase-ct-kill-tq ()
  (setq clearcase-ct-running nil)
  (setq clearcase-ct-tq nil)
  (process-send-eof (tq-process clearcase-ct-tq))
  (kill-process (tq-process clearcase-ct-tq)))

(defun clearcase-ct-kill-buffer-hook ()

  ;; NT Emacs - doesn't use tq.
  ;;
  (if (not clearcase-on-mswindows)
      (let ((kill-buffer-hook nil))
        (if (and (boundp 'clearcase-ct-tq)
                 clearcase-ct-tq
                 (eq (current-buffer) (tq-buffer clearcase-ct-tq)))
            (error "Don't kill TQ buffer %s, use `clearcase-ct-kill-tq'" (current-buffer))))))

(add-hook 'kill-buffer-hook 'clearcase-ct-kill-buffer-hook)

;;}}}

;;{{{ Invoking a command

;; nyi Would be redundant if we didn't need it to invoke normal-diff-program

(defun clearcase-do-command (okstatus command file &optional extra-args)
  "Execute a version-control command, notifying user and checking for errors.
The command is successful if its exit status does not exceed OKSTATUS.
Output from COMMAND goes to buffer *clearcase*.  The last argument of the command is
an optional list of EXTRA-ARGS."
  (setq file (expand-file-name file))
  (if clearcase-command-messages
      (message "Running %s on %s..." command file))
  (let ((camefrom (current-buffer))
        (pwd )
        (squeezed nil)
        status)
    (set-buffer (get-buffer-create "*clearcase*"))
    (setq buffer-read-only nil)
    (erase-buffer)
    (set (make-local-variable 'clearcase-parent-buffer) camefrom)
    (set (make-local-variable 'clearcase-parent-buffer-name)
         (concat " from " (buffer-name camefrom)))
    ;; This is so that command arguments typed in the *clearcase* buffer will
    ;; have reasonable defaults.
    ;;
    (setq default-directory (file-name-directory file)
          file (file-name-nondirectory file))

    (mapcar
     (function (lambda (s)
                 (and s
                      (not (zerop (length s)))
                      (setq squeezed
                            (append squeezed (list s))))))
     extra-args)
    (setq squeezed (append squeezed (list file)))
    (setq status (apply 'call-process command nil t nil squeezed))
    (goto-char (point-min))
    (clearcase-view-mode 0 camefrom)
    (set-buffer-modified-p nil)         ; XEmacs - fsf uses `not-modified'
    (if (or (not (integerp status)) (< okstatus status))
        (progn
          (clearcase-port-view-buffer-other-window "*clearcase*")
          (shrink-window-if-larger-than-buffer)
          (error "Running %s...FAILED (%s)" command
                 (if (integerp status)
                     (format "status %d" status)
                   status)))
      (if clearcase-command-messages
          (message "Running %s...OK" command)))
    (set-buffer camefrom)
    status))

;;}}}

;;{{{ Viewtag management

;;{{{ Started views

(defun clearcase-viewtag-try-to-start-view (viewtag)
  "If VIEW is not apparently already visible under viewroot, start it."
  (if (not (member viewtag (clearcase-viewtag-started-viewtags)))
      (clearcase-viewtag-start-view viewtag)))

(defun clearcase-viewtag-started-viewtags-alist ()
  "Return an alist of views that are currently visible under the viewroot."
  (mapcar
   (function
    (lambda (tag)
      (list (concat tag "/"))))
   (clearcase-viewtag-started-viewtags)))

(defun clearcase-viewtag-started-viewtags ()
  "Return the list of viewtags already visible under the viewroot."
  (let ((raw-list  (if clearcase-on-mswindows
                       (directory-files clearcase-viewroot-drive)
                     (directory-files clearcase-viewroot))))
    (clearcase-utl-list-filter
     (function (lambda (string)
                 ;; Exclude the ones that start with ".",
                 ;; and the ones that end with "@@".
                 ;;
                 (and (not (equal ?. (aref string 0)))
                      (not (string-match "@@$" string)))))
     raw-list)))

;; nyi: Makes sense on NT ?
;;      Probably also want to run subst ?
;;      Need a better high-level interface to start-view
;;
(defun clearcase-viewtag-start-view (viewtag)
  "If VIEWTAG is in our cache of valid view names, start it."
  (if (clearcase-viewtag-exists viewtag)
      (progn
        (message "Starting view server for %s..." viewtag)
        (clearcase-ct-blocking-call "startview" viewtag)
        (message "Starting view server for %s...done" viewtag))))

;;}}}

;;{{{ All views

;;{{{ Internals

(defvar clearcase-viewtag-cache nil
  "Oblist of all known viewtags.")

(defvar clearcase-viewtag-dir-cache nil
  "Oblist of all known viewtag dirs.")

(defvar clearcase-viewtag-cache-timeout 1800
  "*Default timeout of all-viewtag cache, in seconds.")

(defun clearcase-viewtag-schedule-cache-invalidation ()
  "Schedule the next invalidation of clearcase-viewtag-cache."
  (run-at-time (format "%s sec" clearcase-viewtag-cache-timeout)
               nil
               (function (lambda (&rest ignore)
                           (setq clearcase-viewtag-cache nil)))
               nil))
;; Some primes:
;;
;;     1,
;;     2,
;;     3,
;;     7,
;;     17,
;;     31,
;;     61,
;;     127,
;;     257,
;;     509,
;;     1021,
;;     2053,

(defun clearcase-viewtag-read-all-viewtags ()
  "Invoke ct+lsview to get all viewtags, and return an obarry containing them."
  (message "Fetching view names...")
  (let* ((default-directory "/")
         (result (make-vector 1021 0))
         (raw-views-string (clearcase-ct-blocking-call "lsview" "-short"))
         (view-list (clearcase-utl-split-string-at-char raw-views-string ?\n)))
    (message "Fetching view names...done")
    (mapcar (function (lambda (string)
                        (set (intern string result) t)))
            view-list)
    result))

(defun clearcase-viewtag-populate-caches ()
  (setq clearcase-viewtag-cache (clearcase-viewtag-read-all-viewtags))
  (let ((dir-cache (make-vector 1021 0)))
    (mapatoms
     (function (lambda (sym)
                 (set (intern (concat (symbol-name sym) "/") dir-cache) t)))
     clearcase-viewtag-cache)
    (setq clearcase-viewtag-dir-cache dir-cache))
  (clearcase-viewtag-schedule-cache-invalidation))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;}}}

;; Exported interfaces

;; This is for completion of viewtags.
;;
(defun clearcase-viewtag-all-viewtags-obarray ()
  "Return an obarray of all valid viewtags as of the last time we looke  d."
  (if (null clearcase-viewtag-cache)
      (clearcase-viewtag-populate-caches))
  clearcase-viewtag-cache)

;; This is for completion of viewtag dirs, like /view/my_view_name/
;; The trailing slash is required for compatibility with other instances
;; of filename completion in Emacs.
;;
(defun clearcase-viewtag-all-viewtag-dirs-obarray ()
  "Return an obarray of all valid viewtag directory names as of the last time we looked."
  (if (null clearcase-viewtag-dir-cache)
      (clearcase-viewtag-populate-caches))
  clearcase-viewtag-dir-cache)

(defun clearcase-viewtag-exists (viewtag)
  (symbol-value (intern-soft viewtag (clearcase-viewtag-all-viewtags-obarray))))

;;}}}

;;}}}

;;{{{ Pathnames

;;{{{ Pathnames: version-extended

(defun clearcase-vxpath-p (path)
  (or (string-match (concat clearcase-vxpath-glue "/") path)
      (string-match (concat clearcase-vxpath-glue "\\\\") path)))

(defun clearcase-vxpath-element-part (vxpath)
  "Return the element part of version-extended PATH."
  (if (string-match clearcase-vxpath-glue vxpath)
      (substring vxpath 0 (match-beginning 0))
    vxpath))

(defun clearcase-vxpath-version-part (vxpath)
  "Return the version part of version-extended PATH."
  (if (string-match clearcase-vxpath-glue vxpath)
      (substring vxpath (match-end 0))
    nil))

(defun clearcase-vxpath-branch (vxpath)
  "Return the branch part of a version-extended path or of a version"
  (if (clearcase-vxpath-p vxpath)
      (clearcase-vxpath-cons-vxpath
       (clearcase-vxpath-element-part vxpath)
       (file-name-directory (clearcase-vxpath-version-part vxpath)))
    (file-name-directory vxpath)))

(defun clearcase-vxpath-version (vxpath)
  "Return the numeric version part of a version-extended path or of a version"
  (if (clearcase-vxpath-p vxpath)
      (file-name-nondirectory (clearcase-vxpath-version-part vxpath))
    (file-name-nondirectory vxpath)))

(defun clearcase-vxpath-cons-vxpath (file version &optional viewtag)
  "Make a ClearCase version-extended pathname for ELEMENT's version VERSION.
If ELEMENT is actually a version-extended pathname, substitute VERSION for
the version included in ELEMENT.  If VERSION is nil, remove the version-extended
pathname.

If optional VIEWTAG is specified, make a view-relative pathname, possibly
replacing the existing view prefix."
  (let* ((element (clearcase-vxpath-element-part file))
         (glue-fmt (if (and (> (length version) 0)
                            (= (aref version 0) ?/))
                       (concat "%s" clearcase-vxpath-glue "%s")
                     (concat "%s" clearcase-vxpath-glue "/%s")))
         (relpath (clearcase-vrpath-tail element)))
    (if viewtag
        (setq element (concat clearcase-viewroot "/" viewtag (or relpath element))))
    (if version
        (format glue-fmt element version)
      element)))

;; NYI: This should cache the predecessor version as a property
;; of the file.
;;
(defun clearcase-vxpath-of-predecessor (file)
  "Compute the version-extended pathname of the predecessor version of FILE."
  (if (not (equal 'version (clearcase-fprop-mtype file)))
      (error "Not a clearcase version: %s" file))
  (let ((abs-file (expand-file-name file)))
    (let ((ver (clearcase-utl-1st-line-of-string
                (clearcase-ct-cleartool-cmd "describe"
                                            "-pred"
                                            "-short"
                                            (clearcase-path-native abs-file)))))
      (clearcase-path-canonicalise-slashes (concat
                                            (clearcase-vxpath-element-part file)
                                            clearcase-vxpath-glue
                                            ver)))))

(defun clearcase-vxpath-version-extend (file)
  "Compute the version-extended pathname of FILE."
  (if (not (equal 'version (clearcase-fprop-mtype file)))
      (error "Not a clearcase version: %s" file))
  (let ((abs-file (expand-file-name file)))
    (clearcase-path-canonicalise-slashes
     (clearcase-utl-1st-line-of-string
      (clearcase-ct-cleartool-cmd "describe"
                                  "-fmt"
				  (concat "%En"
					  clearcase-vxpath-glue
					  "%Vn")
                                  (clearcase-path-native abs-file))))))

(defun clearcase-vxpath-of-branch-base (file)
  "Compute the version-extended pathname of the version at the branch base of FILE."
  (let* ((file-version-path
          (if  (clearcase-fprop-checked-out file)
              ;; If the file is checked-out, start with its predecessor version...
              ;;
              (clearcase-vxpath-version-extend (clearcase-vxpath-of-predecessor file))
            ;; ...otherwise start with the file's version.
            ;;
            (clearcase-vxpath-version-extend file)))
         (file-version-number (string-to-int (clearcase-vxpath-version file-version-path)))
         (branch (clearcase-vxpath-branch file-version-path)))
    (let* ((base-number 0)
           (base-version-path (format "%s%d" branch base-number)))
      (while (and (not (clearcase-file-is-in-snapshot-p base-version-path))
		  (not (file-exists-p base-version-path))
                  (< base-number file-version-number))
        (setq base-number (1+ base-number))
        (setq base-version-path (format "%s%d" branch base-number)))
      base-version-path)))

(defun clearcase-vxpath-version-of-branch-base (file)
  (clearcase-vxpath-version-part (clearcase-vxpath-of-branch-base file)))

(defun clearcase-vxpath-get-version-in-buffer (vxpath)
  "Return a buffer containing the version named by VXPATH.
Intended for use in snapshot views."
  (let* ((temp-file (clearcase-vxpath-get-version-in-temp-file vxpath))
         (buffer (find-file-noselect temp-file t)))

    ;; XEmacs throws an error if you delete a read-only file
    ;;
    (if clearcase-xemacs-p
        (if (not (file-writable-p temp-file))
            (set-file-modes temp-file (string-to-number "666" 8))))

    (delete-file temp-file)
    buffer))

(defun clearcase-vxpath-get-version-in-temp-file (vxpath)
  "Return the name of a temporary file containing the version named by VXPATH.
Intended for use in snapshot views."

  (let ((temp-file (clearcase-utl-tempfile-name vxpath)))
    (progn
      (clearcase-ct-blocking-call "get"
                                  "-to"
                                  (clearcase-path-native temp-file)
                                  (clearcase-path-native vxpath))
      temp-file)))

;;}}}

;;{{{ Pathnames: viewroot-relative

;; nyi: make all this work with viewroot-drive-relative files too

(defun clearcase-vrpath-p (path)
  "Return whether PATH is viewroot-relative."
  (string-match clearcase-vrpath-regexp path))

(defun clearcase-vrpath-head (vrpath)
  "Given viewroot-relative PATH, return the prefix including the view-tag."
  (if (string-match clearcase-vrpath-regexp vrpath)
      (substring vrpath (match-end 0))))

(defun clearcase-vrpath-tail (vrpath)
  "Given viewroot-relative PATH, return the suffix after the view-tag."
  (if (string-match clearcase-vrpath-regexp vrpath)
      (substring vrpath (match-end 0))))

(defun clearcase-vrpath-viewtag (vrpath)
  "Given viewroot-relative PATH, return the view-tag."
  (if (string-match clearcase-vrpath-regexp vrpath)
      (substring vrpath (match-beginning 1) (match-end 1))))

;; Remove useless viewtags from a pathname.
;; e.g. if we're setviewed to view "VIEWTAG"
;;    (clearcase-path-remove-useless-viewtags "/view/VIEWTAG/PATH")
;;     ==> "PATH"
;;    (clearcase-path-remove-useless-viewtags "/view/z/view/y/PATH")
;;     ==> /view/y/"PATH"
;;
(defvar clearcase-multiple-viewroot-regexp
  (concat "^"
          clearcase-viewroot
          clearcase-pname-sep-regexp
          clearcase-non-pname-sep-regexp "+"
          "\\("
          clearcase-viewroot
          clearcase-pname-sep-regexp
          "\\)"
          ))

(defun clearcase-path-remove-useless-viewtags (pathname)
  ;; Try to avoid file-name-handler recursion here:
  ;;
  (let ((setview-root clearcase-setview-root))
    (if setview-root
        ;; Append "/":
        ;;
        (setq setview-root (concat setview-root "/")))

    (cond

     ((string-match clearcase-multiple-viewroot-regexp pathname)
      (clearcase-path-remove-useless-viewtags (substring pathname (match-beginning 1))))

     ((and setview-root
           (string= setview-root "/"))
      pathname)

     ;; If pathname has setview-root as a proper prefix,
     ;; strip it off and recurse:
     ;;
     ((and setview-root
           (< (length setview-root) (length pathname))
           (string= setview-root (substring pathname 0 (length setview-root))))
      (clearcase-path-remove-useless-viewtags (substring pathname (- (length setview-root) 1))))

     (t
      pathname))))

;;}}}

;; Don't pass the "INPLACE" parameter to subst-char-in-string here since the
;; parameter is not necessarily a local variable (in some cases it is
;; buffer-file-name and replacing / with \ in it wreaks havoc).
;;
(defun clearcase-path-canonicalise-slashes (path)
  (if (not clearcase-on-mswindows)
      path
    (subst-char-in-string ?\\ ?/ path)))

(defun clearcase-path-canonical (path)
  (if (not clearcase-on-mswindows)
      path
    (if clearcase-on-cygwin
	(substring (shell-command-to-string (concat "cygpath -u '" path "'")) 0 -1)
      (subst-char-in-string ?\\ ?/ path))))

(defun clearcase-path-native (path)
  (if (not clearcase-on-mswindows)
      path
    (if clearcase-on-cygwin
	(substring (shell-command-to-string (concat "cygpath -w " path)) 0 -1)
      (subst-char-in-string ?/ ?\\ path))))

(defun clearcase-path-file-really-exists-p (filename)
  "Test if a file really exists, when all file-name handlers are disabled."
  (let ((inhibit-file-name-operation 'file-exists-p)
        (inhibit-file-name-handlers (mapcar
                                     (lambda (pair)
                                       (cdr pair))
                                     file-name-handler-alist)))
    (file-exists-p filename)))

(defun clearcase-path-file-in-any-scopes (file scopes)
  (let ((result nil)
        (cursor scopes))
    (while (and (null result)
                cursor)
      (if (clearcase-path-file-in-scope file (car cursor))
          (setq result t))
      (setq cursor (cdr cursor)))
    result))


(defun clearcase-path-file-in-scope (file scope)
  (assert (file-name-absolute-p file))
  (assert (file-name-absolute-p scope))

  (or
   ;; Pathnames are equal
   ;;
   (string= file scope)

   ;; scope-qua-dir is an ancestor of file (proper string prefix)
   ;;
   (let ((scope-as-dir (concat scope "/")))
     (string= scope-as-dir
              (substring file 0 (length scope-as-dir))))))

;;}}}

;;{{{ Mode-line

(defun clearcase-mode-line-buffer-id (filename)
  "Compute an abbreviated version string for the mode-line.
It will be in one of three forms: /main/NNN, or .../branchname/NNN, or DO-NAME"

  (if (clearcase-fprop-checked-out filename)
      (if (clearcase-fprop-reserved filename)
          "RESERVED"
        "UNRESERVED")
    (let ((ver-string (clearcase-fprop-version filename)))
      (if (not (zerop (length ver-string)))
          (let ((i (length ver-string))
                (slash-count 0))
            ;; Search back from the end to the second-last slash
            ;;
            (while (and (> i 0)
                        (< slash-count  2))
              (if (equal ?/ (aref ver-string (1- i)))
                  (setq slash-count (1+ slash-count)))
              (setq i (1- i)))
            (if (> i 0)
                (concat "..." (substring ver-string i))
              (substring ver-string i)))))))

;;}}}

;;{{{ Minibuffer reading

;;{{{ clearcase-read-version-name

(defun clearcase-read-version-name (prompt file)
  "Display PROMPT and read a version string for FILE in the minibuffer,
with completion if possible."
  (let* ((insert-default-directory nil)
         (predecessor (clearcase-fprop-predecessor-version file))
         (default-filename (clearcase-vxpath-cons-vxpath file predecessor))

         ;; To get this to work it is necessary to make Emacs think
         ;; we're completing with respect to "ELEMENT@@/" rather
         ;; than "ELEMENT@@". Otherwise when we enter a version
         ;; like "/main/NN", it thinks we entered an absolute path.
         ;; So instead, we prompt the user to enter "main/..../NN"
         ;; and add back the leading slash before returning.
         ;;
         (completing-dir (concat file "@@/")))
    (if (and (clearcase-file-is-in-mvfs-p file) (not clearcase-on-mswindows))
        ;; Completion only works in MVFS:
        ;;
        (concat "/" (read-file-name prompt
                                    completing-dir
                                    (substring predecessor 1)
                                    ;;nil
                                    t
                                    (substring predecessor 1)))
      (concat "/" (read-string prompt
                               (substring predecessor 1)
                               nil)))))

;;}}}

;;{{{ clearcase-read-label-name

;; nyi: unused

(defun clearcase-read-label-name (prompt)
  "Read a label name."

  (let* ((string (clearcase-ct-cleartool-cmd "lstype"
                                             "-kind"
                                             "lbtype"
                                             "-short"))
         labels)
    (mapcar (function (lambda (arg)
                        (if (string-match "(locked)" arg)
                            nil
                          (setq labels (cons (list arg) labels)))))
            (clearcase-utl-split-string string "\n"))
    (completing-read prompt labels nil t)))

;;}}}

;;}}}

;;{{{ Directory-tree walking

(defun clearcase-dir-all-files (func &rest args)
  "Invoke FUNC f ARGS on each regular file f in default directory."
  (let ((dir default-directory))
    (message "Scanning directory %s..." dir)
    (mapcar (function (lambda (f)
                        (let ((dirf (expand-file-name f dir)))
                          (apply func dirf args))))
            (directory-files dir))
    (message "Scanning directory %s...done" dir)))

(defun clearcase-file-tree-walk-internal (file func args quiet)
  (if (not (file-directory-p file))
      (apply func file args)
    (or quiet
        (message "Traversing directory %s..." file))
    (let ((dir (file-name-as-directory file)))
      (mapcar
       (function
        (lambda (f) (or
                     (string-equal f ".")
                     (string-equal f "..")
                     (member f clearcase-directory-exclusion-list)
                     (let ((dirf (concat dir f)))
                       (or
                        (file-symlink-p dirf) ;; Avoid possible loops
                        (clearcase-file-tree-walk-internal dirf func args quiet))))))
       (directory-files dir)))))
;;
(defun clearcase-file-tree-walk (func &rest args)
  "Walk recursively through default directory.
Invoke FUNC f ARGS on each non-directory file f underneath it."
  (clearcase-file-tree-walk-internal default-directory func args nil)
  (message "Traversing directory %s...done" default-directory))

(defun clearcase-subdir-tree-walk (func &rest args)
  "Walk recursively through default directory.
Invoke FUNC f ARGS on each subdirectory underneath it."
  (clearcase-subdir-tree-walk-internal default-directory func args nil)
  (message "Traversing directory %s...done" default-directory))

(defun clearcase-subdir-tree-walk-internal (file func args quiet)
  (if (file-directory-p file)
      (let ((dir (file-name-as-directory file)))
        (apply func dir args)
        (or quiet
            (message "Traversing directory %s..." file))
        (mapcar
         (function
          (lambda (f) (or
                       (string-equal f ".")
                       (string-equal f "..")
                       (member f clearcase-directory-exclusion-list)
                       (let ((dirf (concat dir f)))
                         (or
                          (file-symlink-p dirf) ;; Avoid possible loops
                          (clearcase-subdir-tree-walk-internal dirf
                                                               func
                                                               args
                                                               quiet))))))
         (directory-files dir)))))

;;}}}

;;{{{ Buffer context

;; nyi: it would be nice if we could restore fold context too, for folded files.

;; Save a bit of the text around POSN in the current buffer, to help
;; us find the corresponding position again later.  This works even
;; if all markers are destroyed or corrupted.
;;
(defun clearcase-position-context (posn)
  (list posn
        (buffer-size)
        (buffer-substring posn
                          (min (point-max) (+ posn 100)))))

;; Return the position of CONTEXT in the current buffer, or nil if we
;; couldn't find it.
;;
(defun clearcase-find-position-by-context (context)
  (let ((context-string (nth 2 context)))
    (if (equal "" context-string)
        (point-max)
      (save-excursion
        (let ((diff (- (nth 1 context) (buffer-size))))
          (if (< diff 0) (setq diff (- diff)))
          (goto-char (nth 0 context))
          (if (or (search-forward context-string nil t)
                  ;; Can't use search-backward since the match may continue
                  ;; after point.
                  ;;
                  (progn (goto-char (- (point) diff (length context-string)))
                         ;; goto-char doesn't signal an error at
                         ;; beginning of buffer like backward-char would.
                         ;;
                         (search-forward context-string nil t)))
              ;; to beginning of OSTRING
              ;;
              (- (point) (length context-string))))))))

;;}}}

;;{{{ Synchronizing buffers with disk

(defun clearcase-sync-after-file-updated-from-vob (file)
  ;; Do what is needed after a file in a snapshot is updated or a checkout is
  ;; cancelled.

  ;; "ct+update" will not always make the file readonly, if, for
  ;; example, its contents didn't actually change.  But we'd like
  ;; update to result in a readonly file, so force it here.
  ;;
  (clearcase-utl-make-unwriteable file)

  (or
   ;; If this returns true, there was a buffer visiting the file and it it
   ;; flushed fprops...
   ;;
   (clearcase-sync-from-disk-if-needed file)

   ;; ...otherwise, just sync this other state:
   ;;
   (progn
     (clearcase-fprop-unstore-properties file)
     (dired-relist-file file))))

(defun clearcase-sync-from-disk (file &optional no-confirm)

  (clearcase-fprop-unstore-properties file)
  ;; If the given file is in any buffer, revert it.
  ;;
  (let ((buffer (find-buffer-visiting file)))
    (if buffer
        (save-excursion
          (set-buffer buffer)
          (clearcase-buffer-revert no-confirm)
          (clearcase-fprop-get-properties file)

          ;; Make sure the mode-line gets updated.
          ;;
          (setq clearcase-mode
                (concat " ClearCase:"
                        (clearcase-mode-line-buffer-id file)))
          (force-mode-line-update))))

  ;; Update any Dired Mode buffers that list this file.
  ;;
  (dired-relist-file file)

  ;; If the file was a directory, update any dired-buffer for
  ;; that directory.
  ;;
  (mapcar (function (lambda (buffer)
                      (save-excursion
                        (set-buffer buffer)
                        (revert-buffer))))
          (dired-buffers-for-dir file)))

(defun clearcase-sync-from-disk-if-needed (file)

  ;; If the buffer on FILE is out of sync with its file, synch it. Returns t if
  ;; clearcase-sync-from-disk is called.

  (let ((buffer (find-buffer-visiting file)))
    (if (and buffer
             ;; Buffer can be out of sync in two ways:
             ;;  (a) Buffer is modified (hasn't been written)
             ;;  (b) Buffer is recording a different modtime to what the file has.
             ;;      This is what happens when the file is updated by another
             ;;      process.
             ;;  (c) Buffer and file differ in their writeability.
             ;;
             (or (buffer-modified-p buffer)
                 (not (verify-visited-file-modtime buffer))
                 (eq (file-writable-p file)
                     (with-current-buffer buffer buffer-read-only))))
        (progn
          (clearcase-sync-from-disk file
                                    ;; Only confirm for modified buffers.
                                    ;;
                                    (not (buffer-modified-p buffer)))
          t)
      nil)))


(defun clearcase-sync-to-disk (&optional not-urgent)

  ;; Make sure the current buffer and its working file are in sync
  ;; NOT-URGENT means it is ok to continue if the user says not to save.
  ;;
  (if (buffer-modified-p)
      (if (or clearcase-suppress-confirm
              (y-or-n-p (format "Buffer %s modified; save it? "
                                (buffer-name))))
          (save-buffer)
        (if not-urgent
            nil
          (error "Aborted")))))


(defun clearcase-buffer-revert (&optional no-confirm)
  ;; Should never call for Dired buffers
  ;;
  (assert (not (eq major-mode 'dired-mode)))

  ;; Revert buffer, try to keep point and mark where user expects them in spite
  ;; of changes because of expanded version-control key words.  This is quite
  ;; important since otherwise typeahead won't work as expected.
  ;;
  (widen)
  (let ((point-context (clearcase-position-context (point)))

        ;; Use clearcase-utl-mark-marker to avoid confusion in transient-mark-mode.
        ;; XEmacs - mark-marker t, FSF Emacs - mark-marker.
        ;;
        (mark-context (if (eq (marker-buffer (clearcase-utl-mark-marker))
                              (current-buffer))
                          (clearcase-position-context (clearcase-utl-mark-marker))))
        (camefrom (current-buffer)))

    ;; nyi: Should we run font-lock ?
    ;; Want to avoid re-doing a buffer that is already correct, such as on
    ;; check-in/check-out.
    ;; For now do-nothing.

    ;; The actual revisit.
    ;; For some reason, revert-buffer doesn't recompute whether View Minor Mode
    ;; should be on, so turn it off and then turn it on if necessary.
    ;;
    ;; nyi: Perhaps we should re-find-file ?
    ;;
    (or clearcase-xemacs-p
        (if (fboundp 'view-mode)
            (view-mode 0)))
    (revert-buffer t no-confirm t)
    (or clearcase-xemacs-p
        (if (and (boundp 'view-read-only)
                 view-read-only
                 buffer-read-only)
            (view-mode 1)))

    ;; Restore point and mark.
    ;;
    (let ((new-point (clearcase-find-position-by-context point-context)))
      (if new-point
          (goto-char new-point))
      (if mark-context
          (let ((new-mark (clearcase-find-position-by-context mark-context)))
            (if new-mark
                (set-mark new-mark))))

      ;; Restore a semblance of folded state.
      ;;
      (if (and (boundp 'folded-file)
               folded-file)
          (progn
            (folding-open-buffer)
            (folding-whole-buffer)
            (if new-point
                (folding-goto-char new-point)))))))

;;}}}

;;{{{ Utilities

;;{{{ Displaying content in special buffers

(defun clearcase-utl-populate-and-view-buffer (buffer
                                               args
                                               content-generating-func)
  "Empty BUFFER, and populate it by applying to ARGS the CONTENT-GENERATING-FUNC,
and display in a separate window."

  (clearcase-utl-edit-and-view-buffer
   buffer
   (list args)
   (function
    (lambda (args)
      (erase-buffer)
      (apply content-generating-func args)))))

(defun clearcase-utl-edit-and-view-buffer (buffer
                                           args
                                           content-editing-func)
  "Empty BUFFER, and edit it by applying to ARGS the CONTENT-EDITING-FUNC,
and display in a separate window."

  (let ( ;; Create the buffer if necessary.
        ;;
        (buf (get-buffer-create buffer))

        ;; Record where we came from.
        ;;
        (camefrom (current-buffer)))

    (set-buffer buf)
    (clearcase-view-mode 0 camefrom)

    ;; Edit the buffer.
    ;;
    (apply content-editing-func args)

    ;; Display the buffer.
    ;;
    (clearcase-port-view-buffer-other-window buf)
    (goto-char 0)
    (set-buffer-modified-p nil)         ; XEmacs - fsf uses `not-modified'
    (shrink-window-if-larger-than-buffer)))

;;}}}

;;{{{ Temporary files

(defvar clearcase-tempfiles nil)
(defun clearcase-utl-tempfile-name (&optional vxpath)
  (let ((ext ""))
    (and vxpath
         (save-match-data
           (if (string-match "\\(\\.[^.]+\\)@@" vxpath)
               (setq ext (match-string 1 vxpath)))))
    (let ((filename (concat
                     (make-temp-name (clearcase-path-canonical
                                      ;; Use TEMP e.v. if set.
                                      ;;
                                      (concat (or (getenv "TEMP") "/tmp")
                                              "/clearcase-")))
                     ext)))
      ;; Store its name for later cleanup.
      ;;
      (setq clearcase-tempfiles (cons filename clearcase-tempfiles))
      filename)))

(defun clearcase-utl-clean-tempfiles ()
  (mapcar (function
           (lambda (tempfile)
             (if (file-exists-p tempfile)
                 (condition-case nil
                     (delete-file tempfile)
                   (error nil)))))
          clearcase-tempfiles)
  (setq clearcase-tempfiles nil))

;;}}}

(defun clearcase-utl-touch-file (file)
  "Attempt to update the modtime of FILE. Return t if it worked."
  (zerop
   ;; Silently fail if there is no "touch" command available.  Couldn't find a
   ;; convenient way to update a file's modtime in ELisp.
   ;;
   (condition-case nil
       (prog1
         (shell-command (concat "touch " file))
         (message ""))
     (error nil))))

(defun clearcase-utl-filetimes-close (filetime1 filetime2 tolerance)
  "Test if FILETIME1 and FILETIME2 are within TOLERANCE of each other."
  ;; nyi: To do this correctly we need to know MAXINT.
  ;; For now this is correct enough since we only use this as a guideline to
  ;; avoid generating a diff.
  ;;
  (if (equal (first filetime1) (first filetime2))
      (< (abs (- (second filetime1) (second filetime2))) tolerance)
    nil))

(defun clearcase-utl-emacs-date-to-clearcase-date (s)
  (concat
   (substring s 20) ;; yyyy
   (int-to-string (clearcase-utl-month-unparse (substring s 4 7))) ;; mm
   (substring s 8 10) ;; dd
   "."
   (substring s 11 13) ;; hh
   (substring s 14 16) ;; mm
   (substring s 17 19))) ;; ss

(defun clearcase-utl-month-unparse (s)
  (cond
   ((string= s "Jan") 1)
   ((string= s "Feb") 2)
   ((string= s "Mar") 3)
   ((string= s "Apr") 4)
   ((string= s "May") 5)
   ((string= s "Jun") 6)
   ((string= s "Jul") 7)
   ((string= s "Aug") 8)
   ((string= s "Sep") 9)
   ((string= s "Oct") 10)
   ((string= s "Nov") 11)
   ((string= s "Dec") 12)))

(defun clearcase-utl-strip-trailing-slashes (name)
  (let* ((len (length name)))
    (while (and (> len 1)
                (or (equal ?/ (aref name (1- len)))
                    (equal ?\\ (aref name (1- len)))))
      (setq len (1- len)))
    (substring name 0 len)))

(defun clearcase-utl-file-size (file)
  (nth 7 (file-attributes file)))
(defun clearcase-utl-file-atime (file)
  (nth 4 (file-attributes file)))
(defun clearcase-utl-file-mtime (file)
  (nth 5 (file-attributes file)))
(defun clearcase-utl-file-ctime (file)
  (nth 6 (file-attributes file)))

(defun clearcase-utl-kill-view-buffer ()
  (interactive)
  (let ((buf (current-buffer)))
    (delete-windows-on buf)
    (kill-buffer buf)))

(defun clearcase-utl-escape-double-quotes (s)
  "Escape any double quotes in string S"
  (mapconcat (function (lambda (char)
                         (if (equal ?\" char)
                             (string ?\\ char)
                           (string char))))
             s
             ""))

(defun clearcase-utl-escape-backslashes (s)
  "Double any backslashes in string S"
  (mapconcat (function (lambda (char)
                         (if (equal ?\\ char)
                             "\\\\"
                           (string char))))
             s
             ""))

(defun clearcase-utl-quote-if-nec (token)
  "If TOKEN contains whitespace and is not already quoted,
wrap it in double quotes."
  (if (and (string-match "[ \t]" token)
           (not (equal ?\" (aref token 0)))
           (not (equal ?\' (aref token 0))))
      (concat "\"" token "\"")
    token))

(defun clearcase-utl-or-func (&rest args)
  "A version of `or' that can be applied to a list."
  (let ((result nil)
        (cursor args))
    (while (and (null result)
                cursor)
      (if (car cursor)
          (setq result t))
      (setq cursor (cdr cursor)))
    result))

(defun clearcase-utl-any (predicate list)
  "Returns t if PREDICATE is satisfied by any element in LIST."
  (let ((result nil)
        (cursor list))
    (while (and (null result)
                cursor)
      (if (funcall predicate (car cursor))
          (setq result t))
      (setq cursor (cdr cursor)))
    result))

(defun clearcase-utl-every (predicate list)
  "Returns t if PREDICATE is satisfied by every element in LIST."
  (let ((result t)
        (cursor list))
    (while (and result
                cursor)
      (if (not (funcall predicate (car cursor)))
          (setq result nil))
      (setq cursor (cdr cursor)))
    result))

(defun clearcase-utl-list-filter (predicate list)
  "Map PREDICATE over each element of LIST, and return a list of the elements
that mapped to non-nil."
  (let ((result '())
        (cursor list))
    (while (not (null cursor))
      (let ((elt (car cursor)))
        (if (funcall predicate elt)
            (setq result (cons elt result)))
        (setq cursor (cdr cursor))))
    (nreverse result)))

(defun clearcase-utl-elts-are-eq (l)
  "Test if all elements of LIST are eq."
  (if (null l)
      t
    (let ((head (car l))
          (answer t))
      (mapcar (function (lambda (elt)
                          (if (not (eq elt head))
                              (setq answer nil))))
              (cdr l))
      answer)))

;; FSF Emacs - doesn't like parameters on mark-marker.
;;
(defun clearcase-utl-mark-marker ()
  (if clearcase-xemacs-p
      (mark-marker t)
    (mark-marker)))

(defun clearcase-utl-syslog (buf value)
  (save-excursion
    (let ((tmpbuf (get-buffer buf)))
      (if (bufferp tmpbuf)
          (progn
            (set-buffer buf)
            (goto-char (point-max))
            (insert (format "%s\n" value)))))))

;; Extract the first line of a string.
;;
(defun clearcase-utl-1st-line-of-string (s)
  (let ((newline ?\n)
        (len (length s))
        (i 0))
    (while (and (< i len)
                (not (eq newline
                         (aref s i))))
      (setq i (1+ i)))
    (substring s 0 i)))

(defun clearcase-utl-split-string (str pat &optional indir suffix)
  (let ((ret nil)
        (start 0)
        (last (length str)))
    (while (< start last)
      (if (string-match pat str start)
          (progn
            (let ((tmp (substring str start (match-beginning 0))))
              (if suffix (setq tmp (concat tmp suffix)))
              (setq ret (cons (if indir (cons tmp nil)
                                tmp)
                              ret)))
            (setq start (match-end 0)))
        (setq start last)
        (setq ret (cons (substring str start) ret))))
    (nreverse ret)))

(defun clearcase-utl-split-string-at-char (str char)
  (let ((ret nil)
        (i 0)
        (eos (length str)))
    (while (< i eos)
      ;; Collect next token
      ;;
      (let ((token-begin i))
        ;; Find the end
        ;;
        (while (and (< i eos)
                    (not (eq char (aref str i))))
          (setq i (1+ i)))

        (setq ret (cons (substring str token-begin i)
                        ret))
        (setq i (1+ i))))
    (nreverse ret)))


(defun clearcase-utl-add-env (env var)
  (catch 'return
    (let ((a env)
          (vname (substring var 0
                            (and (string-match "=" var)
                                 (match-end 0)))))
      (let ((vnl (length vname)))
        (while a
          (if (and (> (length (car a)) vnl)
                   (string= (substring (car a) 0 vnl)
                            vname))
              (throw 'return env))
          (setq a (cdr a)))
        (cons var env)))))


(defun clearcase-utl-augment-env-from-view-config-spec (old-env tag &optional add-ons)
  (let ((newenv nil)
        (cc-env (clearcase-misc-extract-evs-from-config-spe tag)))

    ;; 1. Add-on bindings at the front:
    ;;
    (while add-ons
      (setq newenv (clearcase-utl-add-env newenv (car add-ons)))
      (setq add-ons (cdr add-ons)))

    ;; 2. Then bindings defined in the config-spec:
    ;;
    (while cc-env
      (setq newenv (clearcase-utl-add-env newenv (car cc-env)))
      (setq cc-env (cdr cc-env)))

    ;; 3. Lastly bindings that were in the old environment.
    ;;
    (while old-env
      (setq newenv (clearcase-utl-add-env newenv (car old-env)))
      (setq old-env (cdr old-env)))
    newenv))

(defun clearcase-utl-make-writeable (file)
  ;; Equivalent to chmod u+w
  ;;
  (set-file-modes file
                  (logior #o0200 (file-modes file))))

(defun clearcase-utl-make-unwriteable (file)
  ;; Equivalent to chmod u-w
  ;;
  (set-file-modes file
                  (logand #o7577 (file-modes file))))

;;}}}

;;}}}

;;{{{ Menus

;; Predicate to determine if ClearCase menu items are relevant.
;; nyi" this should disappear
;;
(defun clearcase-buffer-contains-version-p ()
  "Return true if the current buffer contains a ClearCase file or directory."
  (let ((object-name (if (eq major-mode 'dired-mode)
                         default-directory
                       buffer-file-name)))
    (clearcase-fprop-file-is-version-p object-name)))

;;{{{ clearcase-mode menu

;;{{{ The contents

;; This version of the menu will hide rather than grey out inapplicable entries.
;;
(defvar clearcase-menu-contents-minimised
  (list "ClearCase"

        ["Checkin" clearcase-checkin-current-buffer
         :keys nil
         :visible (clearcase-file-ok-to-checkin buffer-file-name)]

        ["Edit checkout comment" clearcase-edit-checkout-comment-current-buffer
         :keys nil
         :visible (clearcase-file-ok-to-checkin buffer-file-name)]

        ["Checkout" clearcase-checkout-current-buffer
         :keys nil
         :visible (clearcase-file-ok-to-checkout buffer-file-name)]

        ["Hijack" clearcase-hijack-current-buffer
         :keys nil
         :visible (clearcase-file-ok-to-hijack buffer-file-name)]

        ["Unhijack" clearcase-unhijack-current-buffer
         :keys nil
         :visible (clearcase-file-ok-to-unhijack buffer-file-name)]

        ["Uncheckout" clearcase-uncheckout-current-buffer
         :visible (clearcase-file-ok-to-uncheckout buffer-file-name)]

        ["Find checkouts" clearcase-find-checkouts-in-current-view t]

        ["Make element" clearcase-mkelem-current-buffer
         :visible (clearcase-file-ok-to-mkelem buffer-file-name)]

        "---------------------------------"
        ["Describe version" clearcase-describe-current-buffer
         :visible (clearcase-buffer-contains-version-p)]

        ["Describe file" clearcase-describe-current-buffer
         :visible (not (clearcase-buffer-contains-version-p))]

        ["Annotate version" clearcase-annotate-current-buffer
         :visible (clearcase-buffer-contains-version-p)]

        ["Show config-spec rule" clearcase-what-rule-current-buffer
         :visible (clearcase-buffer-contains-version-p)]

        ;; nyi: enable this also when setviewed ?
        ;;
        ["Edit config-spec" clearcase-edcs-edit t]

        "---------------------------------"
        (list "Compare (Emacs)..."
              ["Compare with predecessor" clearcase-ediff-pred-current-buffer
               :keys nil
               :visible (clearcase-buffer-contains-version-p)]
              ["Compare with branch base" clearcase-ediff-branch-base-current-buffer
               :keys nil
               :visible (clearcase-buffer-contains-version-p)]
              ["Compare with named version" clearcase-ediff-named-version-current-buffer
               :keys nil
               :visible (clearcase-buffer-contains-version-p)])
        (list "Compare (GUI)..."
              ["Compare with predecessor" clearcase-gui-diff-pred-current-buffer
               :keys nil
               :visible (clearcase-buffer-contains-version-p)]
              ["Compare with branch base" clearcase-gui-diff-branch-base-current-buffer
               :keys nil
               :visible (clearcase-buffer-contains-version-p)]
              ["Compare with named version" clearcase-gui-diff-named-version-current-buffer
               :keys nil
               :visible (clearcase-buffer-contains-version-p)])
        (list "Compare (diff)..."
              ["Compare with predecessor" clearcase-diff-pred-current-buffer
               :keys nil
               :visible (clearcase-buffer-contains-version-p)]
              ["Compare with branch base" clearcase-diff-branch-base-current-buffer
               :keys nil
               :visible (clearcase-buffer-contains-version-p)]
              ["Compare with named version" clearcase-diff-named-version-current-buffer
               :keys nil
               :visible (clearcase-buffer-contains-version-p)])
        "---------------------------------"
        ["Browse versions (dired)" clearcase-browse-vtree-current-buffer
         :visible (clearcase-file-ok-to-browse buffer-file-name)]
        ["Vtree browser GUI" clearcase-gui-vtree-browser-current-buffer
         :keys nil
         :visible (clearcase-buffer-contains-version-p)]
        "---------------------------------"
        (list "Update snapshot..."
              ["Update view" clearcase-update-view
               :keys nil
               :visible (and (clearcase-file-is-in-view-p default-directory)
                             (not (clearcase-file-is-in-mvfs-p default-directory)))]
              ["Update directory" clearcase-update-default-directory
               :keys nil
               :visible (and (clearcase-file-is-in-view-p default-directory)
                             (not (clearcase-file-is-in-mvfs-p default-directory)))]
              ["Update this file" clearcase-update-current-buffer
               :keys nil
               :visible (and (clearcase-file-ok-to-checkout buffer-file-name)
                             (not (clearcase-file-is-in-mvfs-p buffer-file-name)))]
              )
        "---------------------------------"
        (list "Element history..."
              ["Element history (full)" clearcase-list-history-current-buffer
               :keys nil
               :visible (clearcase-buffer-contains-version-p)]
              ["Element history (branch)" clearcase-list-history-current-buffer
               :keys nil
               :visible (clearcase-buffer-contains-version-p)]
              ["Element history (me)" clearcase-list-history-current-buffer
               :keys nil
               :visible (clearcase-buffer-contains-version-p)])
        "---------------------------------"
        ["Show current activity" clearcase-ucm-describe-current-activity
         :keys nil
         :active (clearcase-vprop-ucm (clearcase-fprop-viewtag default-directory))]
        ["Make activity" clearcase-ucm-mkact-current-dir
         :keys nil
         :visible (clearcase-vprop-ucm (clearcase-fprop-viewtag default-directory))]
        ["Set activity..." clearcase-ucm-set-activity-current-dir
         :keys nil
         :visible (clearcase-vprop-ucm (clearcase-fprop-viewtag default-directory))]
        ["Set NO activity" clearcase-ucm-set-activity-none-current-dir
         :keys nil
         :visible (clearcase-vprop-ucm (clearcase-fprop-viewtag default-directory))]
        ["Rebase this stream" clearcase-gui-rebase
         :keys nil
         :visible (clearcase-vprop-ucm (clearcase-fprop-viewtag default-directory))]
        ["Deliver from this stream" clearcase-gui-deliver
         :keys nil
         :visible (clearcase-vprop-ucm (clearcase-fprop-viewtag default-directory))]
        "---------------------------------"
        (list "ClearCase GUI"
              ["ClearCase Explorer" clearcase-gui-clearexplorer
               :keys nil
               :visible clearcase-on-mswindows]
              ["Project Explorer" clearcase-gui-project-explorer
               :keys nil]
              ["Merge Manager" clearcase-gui-merge-manager
               :keys nil]
              ["Snapshot View Updater" clearcase-gui-snapshot-view-updater
               :keys nil])
        "---------------------------------"

        ;; nyi:
        ;; Enable this when current buffer is on VOB.
        ;;
        ["Make branch type" clearcase-mkbrtype
         :keys nil]

        "---------------------------------"
        ["Report Bug in ClearCase Mode" clearcase-submit-bug-report
         :keys nil]

        ["Dump internals" clearcase-dump
         :keys nil
         :visible (or (equal "rwhitby" (user-login-name))
                      (equal "esler" (user-login-name)))]

        ["Flush caches" clearcase-flush-caches
         :keys nil
         :visible (or (equal "rwhitby" (user-login-name))
                      (equal "esler" (user-login-name)))]

        "---------------------------------"
        ["Customize..." (customize-group 'clearcase)
         :keys nil]))

(defvar clearcase-menu-contents
  (list "ClearCase"

        ["Checkin" clearcase-checkin-current-buffer
         :keys nil
         :active (clearcase-file-ok-to-checkin buffer-file-name)]

        ["Edit checkout comment" clearcase-edit-checkout-comment-current-buffer
         :keys nil
         :active (clearcase-file-ok-to-checkin buffer-file-name)]

        ["Checkout" clearcase-checkout-current-buffer
         :keys nil
         :active (clearcase-file-ok-to-checkout buffer-file-name)]

        ["Hijack" clearcase-hijack-current-buffer
         :keys nil
         :active (clearcase-file-ok-to-hijack buffer-file-name)]

        ["Unhijack" clearcase-unhijack-current-buffer
         :keys nil
         :active (clearcase-file-ok-to-unhijack buffer-file-name)]

        ["Uncheckout" clearcase-uncheckout-current-buffer
         :active (clearcase-file-ok-to-uncheckout buffer-file-name)]

        ["Make element" clearcase-mkelem-current-buffer
         :active (clearcase-file-ok-to-mkelem buffer-file-name)]

        "---------------------------------"
        ["Describe version" clearcase-describe-current-buffer
         :active (clearcase-buffer-contains-version-p)]

        ["Describe file" clearcase-describe-current-buffer
         :active (not (clearcase-buffer-contains-version-p))]

        ["Annotate version" clearcase-annotate-current-buffer
         :keys nil
         :active (clearcase-buffer-contains-version-p)]

        ["Show config-spec rule" clearcase-what-rule-current-buffer
         :active (clearcase-buffer-contains-version-p)]

        ;; nyi: enable this also when setviewed ?
        ;;
        ["Edit config-spec" clearcase-edcs-edit t]

        "---------------------------------"
        (list "Compare (Emacs)..."
              ["Compare with predecessor" clearcase-ediff-pred-current-buffer
               :keys nil
               :active (clearcase-buffer-contains-version-p)]
              ["Compare with branch base" clearcase-ediff-branch-base-current-buffer
               :keys nil
               :active (clearcase-buffer-contains-version-p)]
              ["Compare with named version" clearcase-ediff-named-version-current-buffer
               :keys nil
               :active (clearcase-buffer-contains-version-p)])
        (list "Compare (GUI)..."
              ["Compare with predecessor" clearcase-gui-diff-pred-current-buffer
               :keys nil
               :active (clearcase-buffer-contains-version-p)]
              ["Compare with branch base" clearcase-gui-diff-branch-base-current-buffer
               :keys nil
               :active (clearcase-buffer-contains-version-p)]
              ["Compare with named version" clearcase-gui-diff-named-version-current-buffer
               :keys nil
               :active (clearcase-buffer-contains-version-p)])
        (list "Compare (diff)..."
              ["Compare with predecessor" clearcase-diff-pred-current-buffer
               :keys nil
               :active (clearcase-buffer-contains-version-p)]
              ["Compare with branch base" clearcase-diff-branch-base-current-buffer
               :keys nil
               :active (clearcase-buffer-contains-version-p)]
              ["Compare with named version" clearcase-diff-named-version-current-buffer
               :keys nil
               :active (clearcase-buffer-contains-version-p)])
        "---------------------------------"
        ["Browse versions (dired)" clearcase-browse-vtree-current-buffer
         :active (clearcase-file-ok-to-browse buffer-file-name)]
        ["Vtree browser GUI" clearcase-gui-vtree-browser-current-buffer
         :keys nil
         :active (clearcase-buffer-contains-version-p)]
        "---------------------------------"
        (list "Update snapshot..."
              ["Update view" clearcase-update-view
               :keys nil
               :active (and (clearcase-file-is-in-view-p default-directory)
                            (not (clearcase-file-is-in-mvfs-p default-directory)))]
              ["Update directory" clearcase-update-default-directory
               :keys nil
               :active (and (clearcase-file-is-in-view-p default-directory)
                            (not (clearcase-file-is-in-mvfs-p default-directory)))]
              ["Update this file" clearcase-update-current-buffer
               :keys nil
               :active (and (clearcase-file-ok-to-checkout buffer-file-name)
                            (not (clearcase-file-is-in-mvfs-p buffer-file-name)))]
              )
        "---------------------------------"
        (list "Element history..."
              ["Element history (full)" clearcase-list-history-current-buffer
               :keys nil
               :active (clearcase-buffer-contains-version-p)]
              ["Element history (branch)" clearcase-list-history-current-buffer
               :keys nil
               :active (clearcase-buffer-contains-version-p)]
              ["Element history (me)" clearcase-list-history-current-buffer
               :keys nil
               :active (clearcase-buffer-contains-version-p)])
        "---------------------------------"
        ["Show current activity" clearcase-ucm-describe-current-activity
         :keys nil
         :active (clearcase-vprop-ucm (clearcase-fprop-viewtag default-directory))]
        ["Make activity" clearcase-ucm-mkact-current-dir
         :keys nil
         :active (clearcase-vprop-ucm (clearcase-fprop-viewtag default-directory))]
        ["Set activity..." clearcase-ucm-set-activity-current-dir
         :keys nil
         :active (clearcase-vprop-ucm (clearcase-fprop-viewtag default-directory))]
        ["Set NO activity" clearcase-ucm-set-activity-none-current-dir
         :keys nil
         :active (clearcase-vprop-ucm (clearcase-fprop-viewtag default-directory))]
        ["Rebase this stream" clearcase-gui-rebase
         :keys nil
         :active (clearcase-vprop-ucm (clearcase-fprop-viewtag default-directory))]
        ["Deliver from this stream" clearcase-gui-deliver
         :keys nil
         :active (clearcase-vprop-ucm (clearcase-fprop-viewtag default-directory))]
        "---------------------------------"
        (list "ClearCase GUI"
              ["ClearCase Explorer" clearcase-gui-clearexplorer
               :keys nil
               :active clearcase-on-mswindows]
              ["Project Explorer" clearcase-gui-project-explorer
               :keys nil]
              ["Merge Manager" clearcase-gui-merge-manager
               :keys nil]
              ["Snapshot View Updater" clearcase-gui-snapshot-view-updater
               :keys nil])
        "---------------------------------"

        ;; nyi:
        ;; Enable this when current buffer is on VOB.
        ;;
        ["Make branch type" clearcase-mkbrtype
         :keys nil]

        "---------------------------------"
        ["Report Bug in ClearCase Mode" clearcase-submit-bug-report
         :keys nil]

        ["Dump internals" clearcase-dump
         :keys nil
         :active (or (equal "rwhitby" (user-login-name))
                     (equal "esler" (user-login-name)))]

        ["Flush caches" clearcase-flush-caches
         :keys nil
         :active (or (equal "rwhitby" (user-login-name))
                     (equal "esler" (user-login-name)))]

        "---------------------------------"
        ["Customize..." (customize-group 'clearcase)
         :keys nil]))

(if (and clearcase-minimise-menus
         (not clearcase-xemacs-p))
    (setq clearcase-menu-contents clearcase-menu-contents-minimised))

;;}}}1

(if (>= emacs-major-version '20)
    (progn
      ;; Define the menu
      ;;
      (easy-menu-define
        clearcase-menu
        (list clearcase-mode-map)
        "ClearCase menu"
        clearcase-menu-contents)

      (or clearcase-xemacs-p
          (add-to-list 'menu-bar-final-items 'ClearCase))))

;;}}}

;;{{{ clearcase-dired-mode menu

;;{{{ Related functions

;; nyi: this probably gets run for each menu element.
;;      For better efficiency, look into using a one-pass ":filter"
;;      to construct this menu dynamically.

(defun clearcase-dired-mark-count ()
  (let ((old-point (point))
        (count 0))
    (goto-char (point-min))
    (while (re-search-forward
            (concat "^" (regexp-quote (char-to-string
                                       dired-marker-char))) nil t)
      (setq count (1+ count)))
    (goto-char old-point)
    count))

(defun clearcase-dired-current-ok-to-checkin ()
  (let ((file (dired-get-filename nil t)))
    (and file
         (clearcase-file-ok-to-checkin file))))

(defun clearcase-dired-current-ok-to-checkout ()
  (let ((file (dired-get-filename nil t)))
    (and file
         (clearcase-file-ok-to-checkout file))))

(defun clearcase-dired-current-ok-to-uncheckout ()
  (let ((file (dired-get-filename nil t)))
    (and file
         (clearcase-file-ok-to-uncheckout file))))

(defun clearcase-dired-current-ok-to-hijack ()
  (let ((file (dired-get-filename nil t)))
    (and file
         (clearcase-file-ok-to-hijack file))))

(defun clearcase-dired-current-ok-to-unhijack ()
  (let ((file (dired-get-filename nil t)))
    (and file
         (clearcase-file-ok-to-unhijack file))))

(defun clearcase-dired-current-ok-to-mkelem ()
  (let ((file (dired-get-filename nil t)))
    (and file
         (clearcase-file-ok-to-mkelem file))))

(defun clearcase-dired-current-ok-to-browse ()
  (let ((file (dired-get-filename nil t)))
    (clearcase-file-ok-to-browse file)))

(defvar clearcase-dired-max-marked-files-to-check 5
  "The maximum number of marked files in a Dired buffer when constructing
the ClearCase menu.")

;; nyi: speed these up by stopping check when a non-qualifying file is found
;; Better:
;;   - hook the menu constuction  and figure out what ops apply
;;   - hook mark/unmark/move cursor

(defun clearcase-dired-marked-ok-to-checkin ()
  (let ((files (dired-get-marked-files)))
    (or (> (length files) clearcase-dired-max-marked-files-to-check)
        (clearcase-utl-every (function clearcase-file-ok-to-checkin)
                             files))))

(defun clearcase-dired-marked-ok-to-checkout ()
  (let ((files (dired-get-marked-files)))
    (or (> (length files) clearcase-dired-max-marked-files-to-check)
        (clearcase-utl-every (function clearcase-file-ok-to-checkout)
                             files))))

(defun clearcase-dired-marked-ok-to-uncheckout ()
  (let ((files (dired-get-marked-files)))
    (or (> (length files) clearcase-dired-max-marked-files-to-check)
        (clearcase-utl-every (function clearcase-file-ok-to-uncheckout)
                             files))))

(defun clearcase-dired-marked-ok-to-hijack ()
  (let ((files (dired-get-marked-files)))
    (or (> (length files) clearcase-dired-max-marked-files-to-check)
        (clearcase-utl-every (function clearcase-file-ok-to-hijack)
                             files))))

(defun clearcase-dired-marked-ok-to-unhijack ()
  (let ((files (dired-get-marked-files)))
    (or (> (length files) clearcase-dired-max-marked-files-to-check)
        (clearcase-utl-every (function clearcase-file-ok-to-unhijack)
                             files))))

(defun clearcase-dired-marked-ok-to-mkelem ()
  (let ((files (dired-get-marked-files)))
    (or (> (length files) clearcase-dired-max-marked-files-to-check)
        (clearcase-utl-every (function clearcase-file-ok-to-mkelem)
                             files))))

(defun clearcase-dired-current-dir-ok-to-checkin ()
  (let ((dir (dired-current-directory)))
    (clearcase-file-ok-to-checkin dir)))

(defun clearcase-dired-current-dir-ok-to-checkout ()
  (let ((dir (dired-current-directory)))
    (clearcase-file-ok-to-checkout dir)))

(defun clearcase-dired-current-dir-ok-to-uncheckout ()
  (let ((dir (dired-current-directory)))
    (clearcase-file-ok-to-uncheckout dir)))

;;}}}

;;{{{ Contents

;; This version of the menu will hide rather than grey out inapplicable entries.
;;
(defvar clearcase-dired-menu-contents-minimised
  (list "ClearCase"

        ;; Current file
        ;;
        ["Checkin file" clearcase-checkin-dired-files
         :keys nil
         :visible (and (< (clearcase-dired-mark-count) 2)
                       (clearcase-dired-current-ok-to-checkin))]

        ["Edit checkout comment" clearcase-edit-checkout-comment-dired-file
         :keys nil
         :visible (and (< (clearcase-dired-mark-count) 2)
                       (clearcase-dired-current-ok-to-checkin))]

        ["Checkout file" clearcase-checkout-dired-files
         :keys nil
         :visible (and (< (clearcase-dired-mark-count) 2)
                       (clearcase-dired-current-ok-to-checkout))]

        ["Uncheckout file" clearcase-uncheckout-dired-files
         :keys nil
         :visible (and (< (clearcase-dired-mark-count) 2)
                       (clearcase-dired-current-ok-to-uncheckout))]

        ["Hijack file" clearcase-hijack-dired-files
         :keys nil
         :visible (and (< (clearcase-dired-mark-count) 2)
                       (clearcase-dired-current-ok-to-hijack))]

        ["Unhijack file" clearcase-unhijack-dired-files
         :keys nil
         :visible (and (< (clearcase-dired-mark-count) 2)
                       (clearcase-dired-current-ok-to-unhijack))]

        ["Find checkouts" clearcase-find-checkouts-in-current-view t]

        ["Make file an element" clearcase-mkelem-dired-files
         :visible (and (< (clearcase-dired-mark-count) 2)
                       (clearcase-dired-current-ok-to-mkelem))]

        ;; Marked files
        ;;
        ["Checkin marked files" clearcase-checkin-dired-files
         :keys nil
         :visible (and (>= (clearcase-dired-mark-count) 2)
                       (clearcase-dired-marked-ok-to-checkin))]

        ["Checkout marked files" clearcase-checkout-dired-files
         :keys nil
         :visible (and (>= (clearcase-dired-mark-count) 2)
                       (clearcase-dired-marked-ok-to-checkout))]

        ["Uncheckout marked files" clearcase-uncheckout-dired-files
         :keys nil
         :visible (and (>= (clearcase-dired-mark-count) 2)
                       (clearcase-dired-marked-ok-to-uncheckout))]

        ["Hijack marked files" clearcase-hijack-dired-files
         :keys nil
         :visible (and (>= (clearcase-dired-mark-count) 2)
                       (clearcase-dired-marked-ok-to-hijack))]

        ["Unhijack marked files" clearcase-unhijack-dired-files
         :keys nil
         :visible (and (>= (clearcase-dired-mark-count) 2)
                       (clearcase-dired-marked-ok-to-unhijack))]

        ["Make marked files elements" clearcase-mkelem-dired-files
         :keys nil
         :visible (and (>= (clearcase-dired-mark-count) 2)
                       (clearcase-dired-marked-ok-to-mkelem))]


        ;; Current directory
        ;;
        ["Checkin current-dir" clearcase-dired-checkin-current-dir
         :keys nil
         :visible (clearcase-dired-current-dir-ok-to-checkin)]

        ["Checkout current dir" clearcase-dired-checkout-current-dir
         :keys nil
         :visible (clearcase-dired-current-dir-ok-to-checkout)]

        ["Uncheckout current dir" clearcase-dired-uncheckout-current-dir
         :keys nil
         :visible (clearcase-dired-current-dir-ok-to-uncheckout)]

        "---------------------------------"
        ["Describe file" clearcase-describe-dired-file
         :visible t]

        ["Annotate file" clearcase-annotate-dired-file
         :visible t]

        ["Show config-spec rule" clearcase-what-rule-dired-file
         :visible t]


        ["Edit config-spec" clearcase-edcs-edit t]

        "---------------------------------"
        (list "Compare (Emacs)..."
              ["Compare with predecessor" clearcase-ediff-pred-dired-file
               :keys nil
               :visible t]
              ["Compare with branch base" clearcase-ediff-branch-base-dired-file
               :keys nil
               :visible t]
              ["Compare with named version" clearcase-ediff-named-version-dired-file
               :keys nil
               :visible t])
        (list "Compare (GUI)..."
              ["Compare with predecessor" clearcase-gui-diff-pred-dired-file
               :keys nil
               :visible t]
              ["Compare with branch base" clearcase-gui-diff-branch-base-dired-file
               :keys nil
               :visible t]
              ["Compare with named version" clearcase-gui-diff-named-version-dired-file
               :keys nil
               :visible t])
        (list "Compare (diff)..."
              ["Compare with predecessor" clearcase-diff-pred-dired-file
               :keys nil
               :visible t]
              ["Compare with branch base" clearcase-diff-branch-base-dired-file
               :keys nil
               :visible t]
              ["Compare with named version" clearcase-diff-named-version-dired-file
               :keys nil
               :visible t])
        "---------------------------------"
        ["Browse versions (dired)" clearcase-browse-vtree-dired-file
         :visible (clearcase-dired-current-ok-to-browse)]
        ["Vtree browser GUI" clearcase-gui-vtree-browser-dired-file
         :keys nil
         :visible t]
        "---------------------------------"
        (list "Update snapshot..."
              ["Update view" clearcase-update-view
               :keys nil
               :visible (and (clearcase-file-is-in-view-p default-directory)
                             (not (clearcase-file-is-in-mvfs-p default-directory)))]
              ["Update directory" clearcase-update-default-directory
               :keys nil
               :visible (and (clearcase-file-is-in-view-p default-directory)
                             (not (clearcase-file-is-in-mvfs-p default-directory)))]
              ["Update file" clearcase-update-dired-files
               :keys nil
               :visible (and (< (clearcase-dired-mark-count) 2)
                             (clearcase-dired-current-ok-to-checkout)
                             (not (clearcase-file-is-in-mvfs-p default-directory)))]
              ["Update marked files" clearcase-update-dired-files
               :keys nil
               :visible (and (>= (clearcase-dired-mark-count) 2)
                             (not (clearcase-file-is-in-mvfs-p default-directory)))]
              )
        "---------------------------------"
        (list "Element history..."
              ["Element history (full)" clearcase-list-history-dired-file
               :keys nil
               :visible t]
              ["Element history (branch)" clearcase-list-history-dired-file
               :keys nil
               :visible t]
              ["Element history (me)" clearcase-list-history-dired-file
               :keys nil
               :visible t])
        "---------------------------------"
        ["Show current activity" clearcase-ucm-describe-current-activity
         :keys nil
         :active (clearcase-vprop-ucm (clearcase-fprop-viewtag default-directory))]
        ["Make activity" clearcase-ucm-mkact-current-dir
         :keys nil
         :visible (clearcase-vprop-ucm (clearcase-fprop-viewtag default-directory))]
        ["Set activity..." clearcase-ucm-set-activity-current-dir
         :keys nil
         :visible (clearcase-vprop-ucm (clearcase-fprop-viewtag default-directory))]
        ["Set NO activity" clearcase-ucm-set-activity-none-current-dir
         :keys nil
         :visible (clearcase-vprop-ucm (clearcase-fprop-viewtag default-directory))]
        ["Rebase this stream" clearcase-gui-rebase
         :keys nil
         :visible (clearcase-vprop-ucm (clearcase-fprop-viewtag default-directory))]
        ["Deliver from this stream" clearcase-gui-deliver
         :keys nil
         :visible (clearcase-vprop-ucm (clearcase-fprop-viewtag default-directory))]
        "---------------------------------"
        (list "ClearCase GUI"
              ["ClearCase Explorer" clearcase-gui-clearexplorer
               :keys nil
               :visible clearcase-on-mswindows]
              ["Project Explorer" clearcase-gui-project-explorer
               :keys nil]
              ["Merge Manager" clearcase-gui-merge-manager
               :keys nil]
              ["Snapshot View Updater" clearcase-gui-snapshot-view-updater
               :keys nil])
        "---------------------------------"

        ["Make branch type" clearcase-mkbrtype
         :keys nil]

        "---------------------------------"
        ["Report Bug in ClearCase Mode" clearcase-submit-bug-report
         :keys nil]

        ["Dump internals" clearcase-dump
         :keys nil
         :visible (or (equal "rwhitby" (user-login-name))
                      (equal "esler" (user-login-name)))]

        ["Flush caches" clearcase-flush-caches
         :keys nil
         :visible (or (equal "rwhitby" (user-login-name))
                      (equal "esler" (user-login-name)))]

        "---------------------------------"
        ["Customize..." (customize-group 'clearcase)
         :keys nil]))

(defvar clearcase-dired-menu-contents
  (list "ClearCase"

        ;; Current file
        ;;
        ["Checkin file" clearcase-checkin-dired-files
         :keys nil
         :active (and (< (clearcase-dired-mark-count) 2)
                      (clearcase-dired-current-ok-to-checkin))]

        ["Edit checkout comment" clearcase-edit-checkout-comment-dired-file
         :keys nil
         :active (and (< (clearcase-dired-mark-count) 2)
                      (clearcase-dired-current-ok-to-checkin))]
        
        ["Checkout file" clearcase-checkout-dired-files
         :keys nil
         :active (and (< (clearcase-dired-mark-count) 2)
                      (clearcase-dired-current-ok-to-checkout))]

        ["Uncheckout file" clearcase-uncheckout-dired-files
         :keys nil
         :active (and (< (clearcase-dired-mark-count) 2)
                      (clearcase-dired-current-ok-to-uncheckout))]

        ["Hijack file" clearcase-hijack-dired-files
         :keys nil
         :active (and (< (clearcase-dired-mark-count) 2)
                      (clearcase-dired-current-ok-to-hijack))]

        ["Unhijack file" clearcase-unhijack-dired-files
         :keys nil
         :active (and (< (clearcase-dired-mark-count) 2)
                      (clearcase-dired-current-ok-to-unhijack))]

        ["Make file an element" clearcase-mkelem-dired-files
         :active (and (< (clearcase-dired-mark-count) 2)
                      (clearcase-dired-current-ok-to-mkelem))]

        ;; Marked files
        ;;
        ["Checkin marked files" clearcase-checkin-dired-files
         :keys nil
         :active (and (>= (clearcase-dired-mark-count) 2)
                      (clearcase-dired-marked-ok-to-checkin))]

        ["Checkout marked files" clearcase-checkout-dired-files
         :keys nil
         :active (and (>= (clearcase-dired-mark-count) 2)
                      (clearcase-dired-marked-ok-to-checkout))]

        ["Uncheckout marked files" clearcase-uncheckout-dired-files
         :keys nil
         :active (and (>= (clearcase-dired-mark-count) 2)
                      (clearcase-dired-marked-ok-to-uncheckout))]

        ["Hijack marked files" clearcase-hijack-dired-files
         :keys nil
         :active (and (>= (clearcase-dired-mark-count) 2)
                      (clearcase-dired-marked-ok-to-hijack))]

        ["Unhijack marked files" clearcase-unhijack-dired-files
         :keys nil
         :active (and (>= (clearcase-dired-mark-count) 2)
                      (clearcase-dired-marked-ok-to-unhijack))]

        ["Make marked files elements" clearcase-mkelem-dired-files
         :keys nil
         :active (and (>= (clearcase-dired-mark-count) 2)
                      (clearcase-dired-marked-ok-to-mkelem))]


        ;; Current directory
        ;;
        ["Checkin current-dir" clearcase-dired-checkin-current-dir
         :keys nil
         :active (clearcase-dired-current-dir-ok-to-checkin)]

        ["Checkout current dir" clearcase-dired-checkout-current-dir
         :keys nil
         :active (clearcase-dired-current-dir-ok-to-checkout)]

        ["Uncheckout current dir" clearcase-dired-uncheckout-current-dir
         :keys nil
         :active (clearcase-dired-current-dir-ok-to-uncheckout)]

        "---------------------------------"
        ["Describe file" clearcase-describe-dired-file
         :active t]

        ["Annotate file" clearcase-annotate-dired-file
         :active t]

        ["Show config-spec rule" clearcase-what-rule-dired-file
         :active t]


        ["Edit config-spec" clearcase-edcs-edit t]

        "---------------------------------"
        (list "Compare (Emacs)..."
              ["Compare with predecessor" clearcase-ediff-pred-dired-file
               :keys nil
               :active t]
              ["Compare with branch base" clearcase-ediff-branch-base-dired-file
               :keys nil
               :active t]
              ["Compare with named version" clearcase-ediff-named-version-dired-file
               :keys nil
               :active t])
        (list "Compare (GUI)..."
              ["Compare with predecessor" clearcase-gui-diff-pred-dired-file
               :keys nil
               :active t]
              ["Compare with branch base" clearcase-gui-diff-branch-base-dired-file
               :keys nil
               :active t]
              ["Compare with named version" clearcase-gui-diff-named-version-dired-file
               :keys nil
               :active t])
        (list "Compare (diff)..."
              ["Compare with predecessor" clearcase-diff-pred-dired-file
               :keys nil
               :active t]
              ["Compare with branch base" clearcase-diff-branch-base-dired-file
               :keys nil
               :active t]
              ["Compare with named version" clearcase-diff-named-version-dired-file
               :keys nil
               :active t])
        "---------------------------------"
        ["Browse versions (dired)" clearcase-browse-vtree-dired-file
         :active (clearcase-dired-current-ok-to-browse)]
        ["Vtree browser GUI" clearcase-gui-vtree-browser-dired-file
         :keys nil
         :active t]
        "---------------------------------"
        (list "Update snapshot..."
              ["Update view" clearcase-update-view
               :keys nil
               :active (and (clearcase-file-is-in-view-p default-directory)
                            (not (clearcase-file-is-in-mvfs-p default-directory)))]
              ["Update directory" clearcase-update-default-directory
               :keys nil
               :active (and (clearcase-file-is-in-view-p default-directory)
                            (not (clearcase-file-is-in-mvfs-p default-directory)))]
              ["Update file" clearcase-update-dired-files
               :keys nil
               :active (and (< (clearcase-dired-mark-count) 2)
                            (clearcase-dired-current-ok-to-checkout)
                            (not (clearcase-file-is-in-mvfs-p default-directory)))]
              ["Update marked files" clearcase-update-dired-files
               :keys nil
               :active (and (>= (clearcase-dired-mark-count) 2)
                            (not (clearcase-file-is-in-mvfs-p default-directory)))]
              )
        "---------------------------------"
        (list "Element history..."
              ["Element history (full)" clearcase-list-history-dired-file
               :keys nil
               :active t]
              ["Element history (branch)" clearcase-list-history-dired-file
               :keys nil
               :active t]
              ["Element history (me)" clearcase-list-history-dired-file
               :keys nil
               :active t])
        "---------------------------------"
        ["Show current activity" clearcase-ucm-describe-current-activity
         :keys nil
         :active (clearcase-vprop-ucm (clearcase-fprop-viewtag default-directory))]
        ["Make activity" clearcase-ucm-mkact-current-dir
         :keys nil
         :active (clearcase-vprop-ucm (clearcase-fprop-viewtag default-directory))]
        ["Set activity..." clearcase-ucm-set-activity-current-dir
         :keys nil
         :active (clearcase-vprop-ucm (clearcase-fprop-viewtag default-directory))]
        ["Set NO activity" clearcase-ucm-set-activity-none-current-dir
         :keys nil
         :active (clearcase-vprop-ucm (clearcase-fprop-viewtag default-directory))]
        ["Rebase this stream" clearcase-gui-rebase
         :keys nil
         :active (clearcase-vprop-ucm (clearcase-fprop-viewtag default-directory))]
        ["Deliver from this stream" clearcase-gui-deliver
         :keys nil
         :active (clearcase-vprop-ucm (clearcase-fprop-viewtag default-directory))]
        "---------------------------------"
        (list "ClearCase GUI"
              ["ClearCase Explorer" clearcase-gui-clearexplorer
               :keys nil
               :active clearcase-on-mswindows]
              ["Project Explorer" clearcase-gui-project-explorer
               :keys nil]
              ["Merge Manager" clearcase-gui-merge-manager
               :keys nil]
              ["Snapshot View Updater" clearcase-gui-snapshot-view-updater
               :keys nil])
        "---------------------------------"

        ["Make branch type" clearcase-mkbrtype
         :keys nil]

        "---------------------------------"
        ["Report Bug in ClearCase Mode" clearcase-submit-bug-report
         :keys nil]

        ["Dump internals" clearcase-dump
         :keys nil
         :active (or (equal "rwhitby" (user-login-name))
                     (equal "esler" (user-login-name)))]

        ["Flush caches" clearcase-flush-caches
         :keys nil
         :active (or (equal "rwhitby" (user-login-name))
                     (equal "esler" (user-login-name)))]

        "---------------------------------"
        ["Customize..." (customize-group 'clearcase)
         :keys nil]))

(if (and clearcase-minimise-menus
         (not clearcase-xemacs-p))
    (setq clearcase-dired-menu-contents clearcase-dired-menu-contents-minimised))

;;}}}

(if (>= emacs-major-version '20)
    (progn
      (easy-menu-define
        clearcase-dired-menu
        (list clearcase-dired-mode-map)
        "ClearCase Dired menu"
        clearcase-dired-menu-contents)

      (or clearcase-xemacs-p
          (add-to-list 'menu-bar-final-items 'ClearCase))))

;;}}}

;;}}}

;;{{{ Widgets

;;{{{ Single-selection buffer widget

;; Keep the compiler quiet by declaring these
;; buffer-local variables here thus.
;;
(defvar clearcase-selection-window-config nil)
(defvar clearcase-selection-interpreter nil)
(defvar clearcase-selection-continuation nil)
(defvar clearcase-selection-operands nil)

(defun clearcase-ucm-make-selection-window (buffer-name
                                            buffer-contents
                                            selection-interpreter
                                            continuation
                                            cont-arglist)
  (let ((buf (get-buffer-create buffer-name)))
    (save-excursion

      ;; Reset the buffer
      ;;
      (set-buffer buf)
      (setq buffer-read-only nil)
      (erase-buffer)
      (setq truncate-lines t)

      ;; Paint the buffer
      ;;
      (goto-char (point-min))
      (insert buffer-contents)

      ;; Insert mouse-highlighting
      ;;
      (save-excursion
        (goto-char (point-min))
        (while (< (point) (point-max))
          (condition-case nil
              (progn
                (beginning-of-line)
                (put-text-property (point)
                                   (save-excursion
                                     (end-of-line)
                                     (point))
                                   'mouse-face 'highlight))
            (error nil))
          (forward-line 1)))

      ;; Set a keymap
      ;;
      (setq buffer-read-only t)
      (use-local-map clearcase-selection-keymap)

      ;; Set up the interpreter and continuation
      ;;
      (set (make-local-variable 'clearcase-selection-window-config)
           (current-window-configuration))
      (set (make-local-variable 'clearcase-selection-interpreter)
           selection-interpreter)
      (set (make-local-variable 'clearcase-selection-continuation)
           continuation)
      (set (make-local-variable 'clearcase-selection-operands)
           cont-arglist))

    ;; Display the buffer
    ;;
    (pop-to-buffer buf)
    (goto-char 0)
    (shrink-window-if-larger-than-buffer)
    (message "Use RETURN to select an item")))

(defun clearcase-selection-continue ()
  (interactive)
  (beginning-of-line)
  (sit-for 0)
  ;; Call the interpreter to extract the item of interest
  ;; from the buffer.
  ;;
  (let ((item (funcall clearcase-selection-interpreter)))
    ;; Call the continuation.
    ;;
    (apply clearcase-selection-continuation
           (append clearcase-selection-operands (list item))))

  ;; Restore window config
  ;;
  (let ((sel-buffer (current-buffer)))
    (if clearcase-selection-window-config
        (set-window-configuration clearcase-selection-window-config))
    (delete-windows-on sel-buffer)
    (kill-buffer sel-buffer)))

(defun clearcase-selection-mouse-continue (click)
  (interactive "@e")
  (mouse-set-point click)
  (clearcase-selection-continue))

(defvar clearcase-selection-keymap
  (let ((map (make-sparse-keymap)))
    (define-key map [return] 'clearcase-selection-continue)
    (define-key map [mouse-2] 'clearcase-selection-mouse-continue)
    (define-key map "q" 'clearcase-utl-kill-view-buffer)
    ;; nyi: refresh list
    ;; (define-key map "g" 'clearcase-selection-get)
    map))

;;}}}

;;}}}

;;{{{ Integration with Emacs

;;{{{ Functions: examining the ClearCase installation

;; Discover ClearCase version-string
;;
(defun clearcase-get-version-string ()
  ;; Some care seems to be necessary to avoid problems caused by odd settings
  ;; of the "SHELL" environment variable.  I found that simply
  ;; (shell-command-to-string "cleartool -version") on Windows-2000 with
  ;; SHELL==cmd.exe just returned a copy of the Windows command prompt. The
  ;; result was that clearcase-integrate would not complete.
  ;;
  ;; The follow seems to work.
  ;;
  (if clearcase-on-mswindows
      (shell-command-to-string "cmd /c cleartool -version")
    (shell-command-to-string "sh -c \"cleartool -version\"")))

;; Find where cleartool is installed.
;;
(defun clearcase-find-cleartool ()
  "Search directories listed in the PATH environment variable
looking for a cleartool executable. If found return the full pathname."
  (let ((dir-list (parse-colon-path (getenv "PATH")))
        (cleartool-name (if clearcase-on-mswindows
                            "cleartool.exe"
                          "cleartool"))
        (cleartool-path nil))
    (catch 'found
      (mapcar
       (function (lambda (dir)
                   (let ((f (expand-file-name (concat dir cleartool-name))))
                     (if (file-executable-p f)
                         (progn
                           (setq cleartool-path f)
                           (throw 'found t))))))
       dir-list)
      nil)
    cleartool-path))

(defun clearcase-non-lt-registry-server-online-p ()
  "Heuristic to determine if the local host is network-connected to
its ClearCase servers. Used for a non-LT system."

  (let ((result nil)
        (buf (get-buffer-create " *clearcase-lsregion*")))
    (save-excursion
      (set-buffer buf)
      (erase-buffer)
      (let ((process (start-process "lsregion"
                                    buf
                                    "cleartool"
                                    "lsregion"
                                    "-long"))
            (timeout-occurred nil))

        ;; Now wait a little while, if necessary, for some output.
        ;;
        (while (and (null result)
                    (not timeout-occurred)
                    (< (buffer-size) (length "Tag: ")))
          (if (null (accept-process-output process 10))
              (setq timeout-occurred t))
          (goto-char (point-min))
          (if (looking-at "Tag: ")
              (setq result t)))
        (condition-case nil
            (kill-process process)
          (error nil))))
    ;; If servers are apparently not online, keep the
    ;; buffer around so we can see what lsregion reported.
    ;;
    (sit-for 0.01); Fix by AJM to prevent kill-buffer claiming process still running
    (if result
        (kill-buffer buf))
    result))

;; We could have an LT system, which lacks ct+lsregion, but has ct+lssite.
;;
(defun clearcase-lt-registry-server-online-p ()
  "Heuristic to determine if the local host is network-connected to
its ClearCase servers. Used for LT system."

  (let ((result nil)
        (buf (get-buffer-create " *clearcase-lssite*")))
    (save-excursion
      (set-buffer buf)
      (erase-buffer)
      (let ((process (start-process "lssite"
                                    buf
                                    "cleartool"
                                    "lssite"
                                    "-inquire"))
            (timeout-occurred nil))

        ;; Now wait a little while, if necessary, for some output.
        ;;
        (while (and (null result)
                    (not timeout-occurred)
                    (< (buffer-size) (length "  view_cache_size")))
          (if (null (accept-process-output process 10))
              (setq timeout-occurred t))
          (goto-char (point-min))
          (if (re-search-forward "view_cache_size" nil t)
              (setq result t)))
        (condition-case nil
            (kill-process process)
          (error nil))))

    ;; If servers are apparently not online, keep the
    ;; buffer around so we can see what lssite reported.
    ;;
    (sit-for 0.01); Fix by AJM to prevent kill-buffer claiming process still running
    (if result
        (kill-buffer buf))
    result))

;; Find out if the ClearCase registry server is accessible.
;; We could be on a disconnected laptop.
;;
(defun clearcase-registry-server-online-p ()
  "Heuristic to determine if the local host is network-connected to
its ClearCase server(s)."

  (if clearcase-lt
      (clearcase-lt-registry-server-online-p)
    (clearcase-non-lt-registry-server-online-p)))

;;}}}
;;{{{ Functions: hooks

;;{{{ A find-file hook to turn on clearcase-mode

(defun clearcase-hook-find-file-hook ()
  (let ((filename (buffer-file-name)))
    (if filename
        (progn
          (clearcase-fprop-unstore-properties filename)
          (if (clearcase-file-would-be-in-view-p filename)
              (progn
                ;; 1. Activate minor mode
                ;;
                (clearcase-mode 1)

                ;; 2. Pre-fetch file properties
                ;;
                (if (file-exists-p filename)
                    (progn
                      (clearcase-fprop-get-properties filename)

                      ;; 3. Put branch/ver in mode-line
                      ;;
                      (setq clearcase-mode
                            (concat " ClearCase:"
                                    (clearcase-mode-line-buffer-id filename)))
                      (force-mode-line-update)

                      ;; 4. Schedule the asynchronous fetching of the view's properties
                      ;;    next time Emacs is idle enough.
                      ;;
                      (clearcase-vprop-schedule-work (clearcase-fprop-viewtag filename))

                      ;; 5. Set backup policy
                      ;;
                      (unless clearcase-make-backup-files
                        (make-local-variable 'backup-inhibited)
                        (setq backup-inhibited t))))

                (clearcase-set-auto-mode)))))))

(defun clearcase-set-auto-mode ()
  "Check again for the mode of the current buffer when using ClearCase version extended paths."

  (let* ((version (clearcase-vxpath-version-part (buffer-file-name)))
         (buffer-file-name (clearcase-vxpath-element-part (buffer-file-name))))

    ;; Need to recheck the major mode only if a version was appended.
    ;;
    (if version
        (set-auto-mode))))

;;}}}

;;{{{ A find-file hook for version-extended pathnames

(defun clearcase-hook-vxpath-find-file-hook ()
  (if (clearcase-vxpath-p default-directory)
      (let ((element (clearcase-vxpath-element-part default-directory))
            (version (clearcase-vxpath-version-part default-directory)))

        ;; 1. Set the buffer name to <filename>@@/<branch path>/<version>.
        ;;
        (let ((new-buffer-name
               (concat (file-name-nondirectory element)
                       clearcase-vxpath-glue
                       version
                       (buffer-name))))

          (or (string= new-buffer-name (buffer-name))

              ;; Uniquify the name, if necessary.
              ;;
              (let ((n 2)
                    (uniquifier-string ""))
                (while (get-buffer (concat new-buffer-name uniquifier-string))
                  (setq uniquifier-string (format "<%d>" n))
                  (setq n (1+ n)))
                (rename-buffer
                 (concat new-buffer-name uniquifier-string)))))

        ;; 2. Set the default directory to the dir containing <filename>.
        ;;
        (let ((new-dir (file-name-directory element)))
          (setq default-directory new-dir))

        ;; 3. Disable auto-saving.
        ;;
        ;; If we're visiting <filename>@@/<branch path>/199
        ;; we don't want Emacs trying to find a place to create a "#199#.
        ;;
        (auto-save-mode 0))))

;;}}}

;;{{{ A dired-mode-hook to turn on clearcase-dired-mode

(defun clearcase-hook-dired-mode-hook ()
  ;; Force a re-computation of whether the directory is within ClearCase.
  ;;
  (clearcase-fprop-unstore-properties default-directory)

  ;; Wrap this in an exception handler. Otherwise, diredding into
  ;; a deregistered or otherwise defective snapshot-view fails.
  ;;
  (condition-case nil
      ;; If this directory is below a ClearCase element,
      ;;   1. turn on ClearCase Dired Minor Mode.
      ;;   2. display branch/ver in mode-line
      ;;
      (if (clearcase-file-would-be-in-view-p default-directory)
          (progn
            (if clearcase-auto-dired-mode
                (progn
                  (clearcase-dired-mode 1)
                  (clearcase-fprop-get-properties default-directory)
                  (clearcase-vprop-schedule-work (clearcase-fprop-viewtag default-directory))))
            (setq clearcase-dired-mode
                  (concat " ClearCase:"
                          (clearcase-mode-line-buffer-id default-directory)))
            (force-mode-line-update)))
    (error (message "Error fetching ClearCase properties of %s" default-directory))))

;;}}}

;;{{{ A dired-after-readin-hook to add ClearCase information to the display

(defun clearcase-hook-dired-after-readin-hook ()

  ;; If in clearcase-dired-mode, reformat the buffer.
  ;;
  (if clearcase-dired-mode
      (progn
        (clearcase-dired-reformat-buffer)
          (if clearcase-dired-show-view
              (clearcase-dired-insert-viewtag))))
  t)

;;}}}

;;{{{ A write-file-hook to auto-insert a version-string.

;; To use this, put a line containing this in the first 8 lines of your file:
;;    ClearCase-version: </main/laptop/155>
;; and make sure that clearcase-version-stamp-active gets set to true at least
;; locally in the file.

(defvar clearcase-version-stamp-line-limit 1000)
(defvar clearcase-version-stamp-begin-regexp "ClearCase-version:[ \t]<")
(defvar clearcase-version-stamp-end-regexp ">")
(defvar clearcase-version-stamp-active nil)

(defun clearcase-increment-version (version-string)
  (let* ((branch (clearcase-vxpath-branch version-string))
         (number (clearcase-vxpath-version version-string))
         (new-number (1+ (string-to-number number))))
    (format "%s%d" branch new-number)))

(defun clearcase-version-stamp ()
  (interactive)
  (if (and clearcase-mode
           clearcase-version-stamp-active
           (file-exists-p buffer-file-name)
           (equal 'version (clearcase-fprop-mtype buffer-file-name)))
      (let ((latest-version (clearcase-fprop-predecessor-version buffer-file-name)))

        ;; Note: If the buffer happens to be folded, we may not find the place
        ;; to insert the version-stamp. Folding mode really needs to supply a
        ;; 'save-folded-excursion function to solve this one.  We won't attempt
        ;; a cheaper hack here.

        (save-excursion
          (save-restriction
            (widen)
            (goto-char (point-min))
            (forward-line clearcase-version-stamp-line-limit)
            (let ((limit (point))
                  (v-start nil)
                  (v-end nil))
              (goto-char (point-min))
              (while (and (< (point) limit)
                          (re-search-forward clearcase-version-stamp-begin-regexp
                                             limit
                                             'move))
                (setq v-start (point))
                (end-of-line)
                (let ((line-end (point)))
                  (goto-char v-start)
                  (if (re-search-forward clearcase-version-stamp-end-regexp
                                         line-end
                                         'move)
                      (setq v-end (match-beginning 0)))))
              (if v-end
                  (let ((new-version-stamp (clearcase-increment-version latest-version)))
                    (goto-char v-start)
                    (delete-region v-start v-end)
                    (insert-and-inherit new-version-stamp)))))))))

(defun clearcase-hook-write-file-hook ()

  (clearcase-version-stamp)
  ;; Important to return nil so the files eventually gets written.
  ;;
  nil)

;;}}}

;;{{{ A kill-buffer hook

(defun clearcase-hook-kill-buffer-hook ()
  (let ((filename (buffer-file-name)))
    (if (and filename
             ;; W3 has buffers in which 'buffer-file-name is bound to
             ;; a URL.  Don't attempt to unstore their properties.
             ;;
             (boundp 'buffer-file-truename)
             buffer-file-truename)
        (clearcase-fprop-unstore-properties filename))))

;;}}}

;;{{{ A kill-emacs-hook

(defun clearcase-hook-kill-emacs-hook ()
  (clearcase-utl-clean-tempfiles))

;;}}}

;;}}}
;;{{{ Function:  to replace toggle-read-only

(defun clearcase-toggle-read-only (&optional arg)
  "Change read-only status of current buffer, perhaps via version control.
If the buffer is visiting a ClearCase version, then check the file in or out.
Otherwise, just change the read-only flag of the buffer.  If called with an
argument then just change the read-only flag even if visiting a ClearCase
version."
  (interactive "P")
  (cond (arg
	 (toggle-read-only))
	((and (clearcase-fprop-mtype buffer-file-name)
              buffer-read-only
              (file-writable-p buffer-file-name)
              (/= 0 (user-uid)))
         (toggle-read-only))

        ((clearcase-fprop-mtype buffer-file-name)
         (clearcase-next-action-current-buffer))

        (t
         (toggle-read-only))))

;;}}}
;;{{{ Functions: file-name-handlers

;;{{{ Start dynamic views automatically when paths to them are used

;; This handler starts views when viewroot-relative paths are dereferenced.
;;
;; nyi: for now really only seems useful on Unix.
;;
(defun clearcase-viewroot-relative-file-name-handler (operation &rest args)

  (clearcase-when-debugging
   (if (fboundp 'clearcase-utl-syslog)
       (clearcase-utl-syslog "*clearcase-fh-trace*"
                             (cons "clearcase-viewroot-relative-file-name-handler:"
                                   (cons operation args)))))

  ;; Inhibit the handler to avoid recursion.
  ;;
  (let ((inhibit-file-name-handlers
         (cons 'clearcase-viewroot-relative-file-name-handler
               (and (eq inhibit-file-name-operation operation)
                    inhibit-file-name-handlers)))
        (inhibit-file-name-operation operation))

    (let ((first-arg (car args)))
      ;; We don't always get called with a string.
      ;; e.g. one file operation is verify-visited-file-modtime, whose
      ;; first argument is a buffer.
      ;;
      (if (stringp first-arg)
          (progn
            ;; Now start the view if necessary
            ;;
            (save-match-data
              (let* ((path (clearcase-path-remove-useless-viewtags first-arg))
                     (viewtag (clearcase-vrpath-viewtag path))
                     (default-directory (clearcase-path-remove-useless-viewtags default-directory)))
                (if viewtag
                    (clearcase-viewtag-try-to-start-view viewtag))))))
      (apply operation args))))

;;}}}

;;{{{ Completion on viewtags

;; This handler provides completion for viewtags.
;;
(defun clearcase-viewtag-file-name-handler (operation &rest args)

  (clearcase-when-debugging
   (if (fboundp 'clearcase-utl-syslog)
       (clearcase-utl-syslog "*clearcase-fh-trace*"
                             (cons "clearcase-viewtag-file-name-handler:"
                                   (cons operation args)))))
  (cond

   ((eq operation 'file-name-completion)
    (save-match-data (apply 'clearcase-viewtag-completion args)))

   ((eq operation 'file-name-all-completions)
    (save-match-data (apply 'clearcase-viewtag-completions args)))

   (t
    (let ((inhibit-file-name-handlers
           (cons 'clearcase-viewtag-file-name-handler
                 (and (eq inhibit-file-name-operation operation)
                      inhibit-file-name-handlers)))
          (inhibit-file-name-operation operation))
      (apply operation args)))))

(defun clearcase-viewtag-completion (file dir)
  (try-completion file (clearcase-viewtag-all-viewtag-dirs-obarray)))

(defun clearcase-viewtag-completions (file dir)
  (let ((tags (all-completions file
                               (clearcase-viewtag-all-viewtags-obarray))))
    (mapcar
     (function (lambda (tag)
                 (concat tag "/")))
     tags)))

;;}}}

;;{{{ File name handler for version extended file names

;; For version extended pathnames there are two possible answers
;; for each of
;;   file-name-directory
;;   file-name-nondirectory
;;
;; 1. that pertaining to the element path, e.g.
;;   (file-name-directory "DIR/FILE@@/BRANCH/VERSION")
;;     ==> "DIR/"
;; 2. that pertaining to the version path, e.g.
;;   (file-name-directory "DIR/FILE@@/BRANCH/VERSION")
;;     ==> "DIR/FILE@@/BRANCH/"
;;
;; Often we'd like the former, but sometimes we'd like the latter, for example
;; inside clearcase-browse-vtree, where it calls dired.  Within dired on Gnu
;; Emacs, it calls file-name-directory on the supplied pathname and in this
;; case we want the version (i.e. branch) path to be used.
;;
;; How to get the behaviour we want ?

;; APPROACH A:
;; ==========
;;
;; Define a variable clearcase-treat-branches-as-dirs, which modifies
;; the behaviour of clearcase-vxpath-file-name-handler to give answer (1).
;;
;; Just before we invoke dired inside clearcase-browse-vtree, dynamically
;; bind clearcase-treat-branches-as-dirs to t. Also in the resulting Dired Mode
;; buffer, make clearcase-treat-branches-as-dirs buffer-local and set it.
;;
;; Unfortunately this doesn't quite give us what we want. For example I often
;; invoke grep from a dired buffer on a branch-qua-directory to scan all the
;; version on that branch for a certain string.  The grep-mode buffer has no
;; buffer-local binding for clearcase-treat-branches-as-dirs so the grep
;; command runs in "DIR/" instead of in "DIR/FILE@@/BRANCH/".
;;
;; APPROACH B:
;; ==========
;;
;; Modify the semantics of clearcase-vxpath-file-name-handler so that
;; if the filename given is a pathname to an existing branch-qua-directory
;; give answer 2, otherwise give answer 1.
;;
;; APPROACH C:
;; ==========
;;
;; Use the existence of a Dired Mode buffer on "DIR/FILE@@/BRANCH/" to
;; change the semantics of clearcase-vxpath-file-name-handler.
;;
;; (A) is unsatisfactory and I'm not entirely happy with (B) nor (C) so for now
;; I'm going to disable this filename handler until I'm more convinced it is
;; needed.

(defun clearcase-vxpath-file-name-handler (operation &rest args)
  (clearcase-when-debugging
   (if (fboundp 'clearcase-utl-syslog)
       (clearcase-utl-syslog "*clearcase-fh-trace*"
                             (cons "clearcase-vxpath-file-name-handler:"
                                   (cons operation args)))))
  ;; Inhibit recursion:
  ;;
  (let ((inhibit-file-name-handlers
         (cons 'clearcase-vxpath-file-name-handler
               (and (eq inhibit-file-name-operation operation)
                    inhibit-file-name-handlers)))
        (inhibit-file-name-operation operation))

    (cond ((eq operation 'file-name-nondirectory)
	   (file-name-nondirectory (clearcase-vxpath-element-part
				    (car args))))

	  ((eq operation 'file-name-directory)
	   (file-name-directory (clearcase-vxpath-element-part
				 (car args))))

	  (t
	   (apply operation args)))))

;;}}}

;;}}}
;;{{{ Advice: Disable VC in the MVFS

;; This handler ensures that VC doesn't attempt to operate inside the MVFS.
;; This stops it from futile searches for RCS directories and the like inside.
;; It prevents a certain amount of clutter in the MVFS' noent-cache.
;;

(defadvice vc-registered (around clearcase-interceptor disable compile)
  "Disable normal behavior if in a clearcase dynamic view.
This is enabled/disabled by clearcase-integrate/clearcase-unintegrate."
  (if (clearcase-file-would-be-in-view-p (ad-get-arg 0))
      nil
    ad-do-it))

;;}}}

;;{{{ Functions: integrate and un-integrate.

(defun clearcase-integrate ()
  "Enable ClearCase integration"
  (interactive)

  ;; 0. Empty caches.
  ;;
  (clearcase-fprop-clear-all-properties)
  (clearcase-vprop-clear-all-properties)

  ;; 1. Install hooks.
  ;;
  (add-hook 'find-file-hooks 'clearcase-hook-find-file-hook)
  (add-hook 'find-file-hooks 'clearcase-hook-vxpath-find-file-hook)
  (add-hook 'dired-mode-hook 'clearcase-hook-dired-mode-hook)
  (add-hook 'dired-after-readin-hook 'clearcase-hook-dired-after-readin-hook)
  (add-hook 'kill-buffer-hook 'clearcase-hook-kill-buffer-hook)
  (add-hook 'write-file-hooks 'clearcase-hook-write-file-hook)
  (add-hook 'kill-emacs-hook 'clearcase-hook-kill-emacs-hook)

  ;; 2. Install file-name handlers.
  ;;
  ;;    2.1 Start views when //view/TAG or m:/TAG is referenced.
  ;;
  (add-to-list 'file-name-handler-alist
               (cons clearcase-vrpath-regexp
                     'clearcase-viewroot-relative-file-name-handler))

  ;;    2.2 Completion on viewtags.
  ;;
  (if clearcase-complete-viewtags
      (add-to-list 'file-name-handler-alist
                   (cons clearcase-viewtag-regexp
                         'clearcase-viewtag-file-name-handler)))

  ;;    2.3 Turn off RCS/VCS/SCCS activity inside a ClearCase dynamic view.
  ;;
  (if clearcase-suppress-vc-within-mvfs
      (when clearcase-suppress-vc-within-mvfs
	(ad-enable-advice 'vc-registered 'around 'clearcase-interceptor)
	(ad-activate 'vc-registered)))

  ;; Disabled for now. See comments above clearcase-vxpath-file-name-handler.
  ;;
  ;;   ;;    2.4 Add file name handler for version extended path names
  ;;   ;;
  ;;   (add-to-list 'file-name-handler-alist
  ;;                (cons clearcase-vxpath-glue 'clearcase-vxpath-file-name-handler))
  )

(defun clearcase-unintegrate ()
  "Disable ClearCase integration"
  (interactive)

  ;; 0. Empty caches.
  ;;
  (clearcase-fprop-clear-all-properties)
  (clearcase-vprop-clear-all-properties)

  ;; 1. Remove hooks.
  ;;
  (remove-hook 'find-file-hooks 'clearcase-hook-find-file-hook)
  (remove-hook 'find-file-hooks 'clearcase-hook-vxpath-find-file-hook)
  (remove-hook 'dired-mode-hook 'clearcase-hook-dired-mode-hook)
  (remove-hook 'dired-after-readin-hook 'clearcase-hook-dired-after-readin-hook)
  (remove-hook 'kill-buffer-hook 'clearcase-hook-kill-buffer-hook)
  (remove-hook 'write-file-hooks 'clearcase-hook-write-file-hook)
  (remove-hook 'kill-emacs-hook 'clearcase-hook-kill-emacs-hook)

  ;; 2. Remove file-name handlers.
  ;;
  (setq file-name-handler-alist
        (delete-if (function
                    (lambda (entry)
                      (memq (cdr entry)
                            '(clearcase-viewroot-relative-file-name-handler
                              clearcase-viewtag-file-name-handler
                              clearcase-vxpath-file-name-handler))))
                   file-name-handler-alist))

  ;; 3. Turn on RCS/VCS/SCCS activity everywhere.
  ;;
  (ad-disable-advice 'vc-registered 'around 'clearcase-interceptor)
  (ad-activate 'vc-registered))

;;}}}

;; Here's where we really wire it all in:
;;
(defvar clearcase-cleartool-path nil)
(defvar clearcase-clearcase-version-installed nil)
(defvar clearcase-lt nil)
(defvar clearcase-v3 nil)
(defvar clearcase-v4 nil)
(defvar clearcase-v6 nil)
(defvar clearcase-servers-online nil)
(defvar clearcase-setview-root nil)
(defvar clearcase-setview-viewtag)
(defvar clearcase-setview-root nil)
(defvar clearcase-setview-viewtag nil)

(progn
  ;; If the SHELL environment variable points to the wrong place,
  ;; call-process fails on Windows and this startup fails.
  ;; Check for this and unset the useless EV.

  (let ((shell-ev-value (getenv "SHELL")))
    (if clearcase-on-mswindows
        (if (stringp shell-ev-value)
            (if (not (executable-find shell-ev-value))
                (setenv "SHELL" nil)))))

  ;; Things have to be done here in a certain order.
  ;;
  ;; 1. Make sure cleartool is on the shell search PATH.
  ;;
  (if (setq clearcase-cleartool-path (clearcase-find-cleartool))
      (progn
        ;; 2. Try to discover what version of ClearCase we have:
        ;;
        (setq clearcase-clearcase-version-installed (clearcase-get-version-string))
        (setq clearcase-lt
              (not (null (string-match "ClearCase LT"
                                       clearcase-clearcase-version-installed))))
        (setq clearcase-v3
              (not (null (string-match "^ClearCase version 3"
                                       clearcase-clearcase-version-installed))))
        (setq clearcase-v4
              (not (null (string-match "^ClearCase version 4"
                                       clearcase-clearcase-version-installed))))
        (setq clearcase-v5
              (not (null (string-match "^ClearCase \\(LT \\)?version 2002.05"
                                       clearcase-clearcase-version-installed))))
        (setq clearcase-v6
              (not (null (string-match "^ClearCase \\(LT \\)?version 2003.06"
                                       clearcase-clearcase-version-installed))))

        ;; 3. Gather setview information:
        ;;
        (if (setq clearcase-setview-root (if (not clearcase-on-mswindows)
                                             (getenv "CLEARCASE_ROOT")))
            (setq clearcase-setview-viewtag
                  (file-name-nondirectory clearcase-setview-root)))

        ;; 4. Discover if the servers appear to be online.
        ;;
        (setq clearcase-servers-online (clearcase-registry-server-online-p))

        (if clearcase-servers-online

            ;; 5. Everything seems in place to ensure that ClearCase mode will
            ;;    operate correctly, so integrate now.
            ;;
            (progn
              (clearcase-integrate)
              ;; Schedule a fetching of the view properties when next idle.
              ;; This avoids awkward pauses after the user reaches for the
              ;; ClearCase menubar entry.
              ;;
              (if clearcase-setview-viewtag
                  (clearcase-vprop-schedule-work clearcase-setview-viewtag)))))))

(if (not clearcase-servers-online)
    (message "ClearCase apparently not online. ClearCase/Emacs integration not installed."))

;;}}}

(provide 'clearcase)

;;; clearcase.el ends here

;; Local variables:
;; folded-file: t
;; clearcase-version-stamp-active: t
;; End:
