;;; Complete ANYTHING

;; BUG: (emacs bug) overriding keymap doesn't work from timers
;; todo: show entire name, not just end in pseudo
;; todo: auto-insert-space on completion (hook?)

(require 'tooltip)
(require 'cl)

(defgroup company-mode nil
  "Interactive completion framework."
  :group 'convenience)

;; common - all
(defcustom company-auto-expand nil
  "*Determines whether to automatically expand the first completion."
  :group 'company-mode
  :type '(choice (const :tag "Off" nil)
                 (const :tag "On" t)))

(defcustom company-verbose t
  "*Determines whether to explain user choices in the minibuffer."
  :group 'company-mode
  :type '(choice (const :tag "Off" nil)
                 (const :tag "On" t)))

(defcustom company-display-style 'pseudo-tooltip
  "*Determines how to display possible completions."
  :group 'company-mode
  :type '(choice (const :tag "None" nil)
                 (const :tag "Minibuffer" minibuffer)
                 (const :tag "Tooltip" tooltip)
                 (const :tag "Pseudo Tooltip" pseudo-tooltip)))

(defcustom company-idle-delay 1.2
  "*How many seconds to wait before automatically showing a word's completions.
If this is off, it will not happen automatically."
  :group 'company-mode
  :type '(choice
          (const :tag "Off" nil)
          (number :tag "seconds")))

(defcustom company-complete-on-idle-min-chars 1
  "*Completion on idle will only occur if the prefix is this long."
  :group 'company-mode
  :type 'number)

(defcustom company-complete-on-edit 3
  "*Completion will start after typing this many characters of a word.
If this is off, never complete while typing a word."
  :group 'company-mode
  :type '(choice
          (const :tag "Off" nil)
          (number :tag "seconds")))

(defcustom company-tooltip-delay 1.2
  "*How many seconds to wait before showing a tooltip help.
This value will be added to a delay in `company-idle-delay'.
If this is off, no tooltip will be shown, unless you cycle through completions."
  :group 'company-mode
  :type '(choice
          (const :tag "Off" nil)
          (number :tag "seconds")))

(defface company-common-face
  '((((class color) (background dark))
     (:foreground "orange"))
    (((class color) (background light))
     (:foreground "DarkOrchid4")))
  "*Face used for common parts during dynamic completion."
  :group 'company-mode)

(defface company-expand-face
  '((((class color) (background dark))
     (:background "DarkOrchid4"))
    (((class color) (background light))
     (:background "orange")))
  "*Face used for first complete match during dynamic completion."
  :group 'company-mode)

(defface company-last-expansion-face
  '((((class color) (background dark))
     (:background "DarkOrchid4"))
    (((class color) (background light))
     (:background "orange")))
  "*Face used to highlight the last expansion while it still can be altered.
Altering it can be done with `company-cycle-last-expansion'."
  :group 'company-mode)

(defface company-pseudo-tooltip-face
  '((t :inherit default
       :background "lightyellow"
       :foreground "black"))
  "*Bla."
  :group 'company-mode)

(defface company-pseudo-tooltip-selection-face
  '((t :inherit company-pseudo-tooltip-face
       :background "orange"))
  "*Bla."
  :group 'company-mode)

(defface company-tooltip-selection-face
  '((t :background "orange"
       :foreground "black"))
  "*Bla."
  :group 'company-mode)

(defcustom company-how-many-completions-to-show 10
  "*"
  :group 'company-mode)

(defcustom company-tooltip-offset 0
  "*Y Offset for displaying the tooltip."
  :group 'company-mode)

(defconst company-tooltip-os-shows-below
  "Identifies if the OS shows tooltips below (and not above) the mouse cursor."
  (not (eq window-system 'x)))

(defvar company-overlay nil)
(defvar company-common-overlay nil)
(defvar company-hide-overlay nil)
(defvar company-pseudo-tooltip-overlays nil)

(defvar company-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "\t" 'company-expand-common)
    map)
  "Keymap used in by `company-mode'.")

(defmacro company-expand-number-macro (n)
  "Create in interactive function to select a completion by number N."
  `(lambda () (interactive) (company-expand-number ,n)))

(defvar company-active-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map company-mode-map)
    (define-key map [down] 'company-cycle)
    (define-key map "\M-n" 'company-cycle)
    (define-key map [up] 'company-cycle-backwards)
    (define-key map "\M-p" 'company-cycle-backwards)
    (define-key map (kbd "M-<return>") 'company-expand-top)
    (define-key map "\M-1" (company-expand-number-macro 1))
    (define-key map "\M-2" (company-expand-number-macro 2))
    (define-key map "\M-3" (company-expand-number-macro 3))
    (define-key map "\M-4" (company-expand-number-macro 4))
    (define-key map "\M-5" (company-expand-number-macro 5))
    (define-key map "\M-6" (company-expand-number-macro 6))
    (define-key map "\M-7" (company-expand-number-macro 7))
    (define-key map "\M-8" (company-expand-number-macro 8))
    (define-key map "\M-9" (company-expand-number-macro 9))
    (define-key map "\M-0" (company-expand-number-macro 10))
    map)
  "Keymap used in by `company-mode' when a word is being completed.")

(defvar company-completion-func nil)
(defvar company-auto-expand-func nil)
(defvar company-completion-alist nil)
(defvar company-no-cache nil)

;; TODO: use property!
(defconst company-continue-commands
  '(company-expand-common company-expand-anything company-cycle
    company-cycle-backwards universal-argument self-insert-command
    company-start-showing backward-delete-char-untabify delete-backward-char
    backward-delete-char)
  "Commands as given by `last-command' that don't abort extending.")

(defvar company-timer nil)
(make-variable-buffer-local 'company-timer)
(defvar company-tooltip-timer nil)
(make-variable-buffer-local 'company-tooltip-timer)

(defvar company-common nil)
(defvar company-completions nil)
(defvar company-completions-cache nil)
(defvar company-selection 0)
(defvar company-prefix nil)

;;;###autoload
(define-minor-mode company-mode
  ""
  nil " Company" company-mode-map
  (if company-mode
      (progn
        (add-hook 'post-command-hook 'company-post-command t t)
        (add-hook 'pre-command-hook 'company-mode-pre-command nil t)
        (add-hook 'before-change-functions 'company-before-change nil t)
        (add-hook 'after-change-functions 'company-after-change nil t)
        (setq company-common-overlay nil)
        (setq company-overlay nil)
        (setq company-hide-overlay nil)
        (setq company-completions nil)
        (setq company-completions-cache nil)
        (setq company-pseudo-tooltip-overlays nil)
        (setq company-selection 0)
        (setq company-timer
              (when company-idle-delay
                (run-with-idle-timer
                 company-idle-delay t
                 '(lambda (buf)
                    (when (eq buf (current-buffer))
                      (unless (or company-completions
                                  (= (char-syntax (char-after)) ?w))
                        (when (company-begin nil nil
                                             company-complete-on-idle-min-chars)
                          (company-show-overlay)
                          (when (or (and company-tooltip-delay
                                         (zerop company-tooltip-delay))
                                    (eq last-command 'company-cycle))
                            (company-show-overlay-tips)
                            (company-show-tips))))))
                 (current-buffer))))
        (setq company-tooltip-timer nil))
    (company-done)
    (when company-timer
      (cancel-timer company-timer))
    (kill-local-variable 'company-timer)
    (when company-tooltip-timer
      (cancel-timer company-tooltip-timer))
    (kill-local-variable 'company-tooltip-timer)
    (remove-hook 'post-command-hook 'company-post-command t)
    (remove-hook 'pre-command-hook 'company-mode-pre-command t)
    (remove-hook 'before-change-functions 'company-before-change t)
    (remove-hook 'after-change-functions 'company-after-change t)))

(defun company-auto-expand-maybe (char)
  (when (and company-auto-expand
             (or (and company-auto-expand-func
                      (funcall company-auto-expand-func char))
                 (not (memq (char-syntax char) '(?w ?_)))))
    (company-expand-top)))

(defun company-mode-pre-command ()
  (when company-completions
    (company-hide-overlay)
    (company-hide-pseudo-tooltip)
    (when (eq last-command 'set-mark-command)
      ;; fix this, because it has been disabled by our edits
      (setq mark-active t))))

(defun company-post-command ()
  (if (eq this-command 'set-mark-command)
      ;; fix this, because it has been disabled by our edits
      (setq mark-active t)
    (when (or company-complete-on-edit
              company-completions)
      (unless (memq this-command company-continue-commands)
        (company-abort)))
    (company-show-everything)))

(defvar company-last-command-change nil)

(defun company-before-change (beg end)
  (setq company-last-command-change (- end beg)))

(defun company-after-change (beg end range)
  (when (or company-completions
            company-complete-on-edit)
    (let ((changed (- end beg)))
      (cond
       ((and (> changed 0) (= company-last-command-change 0))
        ;; input 1 char
        (let ((input (buffer-substring-no-properties beg end)))
          (unless (company-auto-expand-maybe input)
            (company-begin nil input company-complete-on-edit))))
        ((and (= changed 0) (> company-last-command-change 0))
         ;; deleted chars
         (company-begin nil nil company-complete-on-edit))))
    (company-show-everything)))

(defun company-show-everything ()
  (when company-completions
    (company-show-overlay)
    (when (or (and company-tooltip-delay
                   (zerop company-tooltip-delay))
              (eq this-command 'company-cycle))
      (company-show-overlay-tips)
      (company-show-tips))))

(defun company-done ()
  (company-disable-active-keymap)
  (setq company-completions nil)
  (setq company-completions-cache nil)
  (when (and (display-graphic-p)
             (fboundp 'x-show-tip))
    (tooltip-hide))
  (company-hide-overlay)
  (company-hide-pseudo-tooltip))

(defun company-get-completions (prefix)
  (if company-completion-func
      (let ((completions (funcall company-completion-func company-prefix)))
        (sort completions 'string<))
    nil))

(defun company-begin (&optional prefix new min-chars)
  (if (and new company-completions (null company-no-cache))
      ;; incremental
      (unless (equal new "")
        (setq company-prefix (concat company-prefix new)
              company-completions (all-completions company-prefix
                                                   company-completions)))
    ;; not incremental
    (setq company-prefix (or prefix (company-grab-prefix))))
  ;; get completions
  (setq company-completions
        ;; try cache
        (or (unless company-no-cache
              (cdr (assoc company-prefix company-completions-cache)))
            ;; or fetch
            (and company-prefix
                 (not (and min-chars (< (length company-prefix) min-chars)))
                 (let ((comp (company-get-completions company-prefix)))
                   (push `(,company-prefix . ,comp) company-completions-cache)
                   comp))))
  (if (and company-completions
           (not (equal company-completions (list company-prefix))))
      ;; begin
      (progn
        (company-hide-last-expansion)
        (company-enable-active-keymap)
        (setq company-common
              (try-completion "" company-completions))
        (when company-tooltip-timer
          (cancel-timer company-tooltip-timer))
        (setq company-tooltip-timer
              (when (and company-tooltip-delay
                         (> company-tooltip-delay 0))
                (run-with-timer
                 company-tooltip-delay nil
                 '(lambda (buf) (when (eq buf (current-buffer))
                                  (when company-completions
                                    (company-show-overlay-tips)
                                    (company-show-tips))))
                 (current-buffer)))))
    ;; end
    (company-done))
  (setq company-selection 0)
  company-completions)

(defsubst company-chop (completion)
  (let ((len (length company-prefix)))
    (substring completion len)))

(defun company-insert-completion (completion)
    (insert completion)
  ;; TODO run hooks
  )
;;; interactive commands ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; TODO: make this the Nth _visible_ completion?
(defun company-expand-number (n)
  "Expand the Nth completion."
  (interactive "P")
  (unless company-mode (error "company-mode not enabled")) 
  (unless company-completions
    (company-begin))
  (if (not company-completions)
      (error "No completions found")
    (let ((completion (company-chop (nth (1- n) company-completions))))
      (if completion
          (let ((beg (point))
                (old-completions (nthcdr (1- n) company-completions)))
            ;; this calls after-change hook!
            (company-insert-completion completion)
            (company-mark-last-expansion beg (point) old-completions))
        (error "No such completion"))
      (company-done)
      completion)))

(defmacro company-expand-then (command)
  `(lambda () (interactive) (company-expand-top) (call-interactively ,command)))

(defun company-start-showing ()
  (interactive)
  (unless company-completions
    (company-begin)))

(defun company-abort ()
  (interactive)
  (when company-completions
    (company-done)))

(defun company-cycle (&optional n)
  (interactive)
  (unless company-mode (error "company-mode not enabled")) 
  (unless company-completions
    (company-begin))
  (if company-completions
      (setq company-selection
            (min (max 0 (+ company-selection (or n 1)))
                 (1- (length company-completions))))
    (error "No completions found")))

(defun company-cycle-backwards (&optional n)
  (interactive)
  (company-cycle (- (or n 1)))
  (setq this-command 'company-cycle))

(defun company-expand-top ()
  (interactive)
  (company-expand-number (1+ company-selection)))

(defun company-expand-anything ()
  (interactive)
  (when (equal (company-expand-common) "")
    (company-expand-top)))

(defun company-expand-common ()
  (interactive)
  (unless company-mode (error "company-mode not enabled")) 
  (unless company-completions
    (company-begin))
  (if company-completions
      (if (eq last-command 'company-expand-common)
          ;; hit twice, show completion buffer
          (company-show-completion-buffer)
        (let ((common (company-chop company-common)))
          ;; this calls after-change hook!
          (company-insert-completion common)
          (unless (cdr company-completions)
            (company-done))
          common))
    (when (called-interactively-p)
      (error "No completions found"))))

(defmacro company-without-undo (&rest body)
  `(let ((buffer-undo-list nil)
         (inhibit-modification-hooks t))
     ,@body))

(defun company-grab-prefix ()
  (let ((pairs (cdr (assoc major-mode company-completion-alist)))
        prefix keyw)
    (while pairs
      (let ((decider (caar pairs)))
        (setq prefix
              (cond
               ((functionp decider)
                (funcall decider))
               ((stringp decider) ;; regexp
                (company-grab decider))
               ((consp decider) ;; regexp w/ subexpression
                (company-grab (car decider) (cdr decider)))
               (t nil)))
        (if (null prefix)
            (pop pairs)
          (setq company-completion-func (cadar pairs))
          ;; TODO no-cache
          (while (keywordp (setq keyw (pop (cddar pairs))))
            (case keyw
              (:no-cache (setq company-no-cache t))))
          (setq pairs nil))))
    (or prefix
        (unless (equal major-mode 'otherwise)
          ;; try default
          (let ((major-mode 'otherwise))
            (company-grab-prefix))))))

(defun company-put-overlay (beg end &optional prop value prop2 value2)
  (let ((ov (make-overlay beg end)))
    (overlay-put ov 'window t)
    (when prop
      (overlay-put ov prop value)
      (when prop2
        (overlay-put ov prop2 value2)))
;;     (overlay-put ov 'keymap company-active-map)
    (when (eq prop 'keymap)
      (overlay-put ov 'face 'company-common-face))
    ov))

(defsubst company-first (n list)
  "Return the first N elements of LIST."
  (butlast list (- (length list) n)))

(defsubst company-butfirst (n list)
  "Return all but the first N elements of LIST."
  (dotimes (i n)
    (pop list))
  list)

;; TODO there must be a library function for this ....
(defun company-sublist (list start length)
  (dotimes (i start)
    (pop list))
  (unless (or (= length 0) (null list))
    (let* ((new-list (cons (pop list) nil))
           (tail new-list))
      (while (and list (> (decf length) 0))
        (setq tail (setcdr tail (cons (pop list) nil))))
      new-list)))

(defsubst company-get-selection-offset (page-size)
  (when company-how-many-completions-to-show
    (setq page-size (min page-size company-how-many-completions-to-show)))
  (let ((offset (* (/ company-selection page-size) page-size)))
;;     (when (equal (car company-completions) "")
;;       (incf offset))
    offset))

(defsubst company-pick-completions (page-size)
  (when company-how-many-completions-to-show
    (setq page-size (min page-size company-how-many-completions-to-show)))
  (mapcar 'company-chop
          (company-sublist company-completions
                           (company-get-selection-offset page-size) page-size)))

(defun company-key-string (command)
  (let ((keys (where-is-internal command overriding-local-map)))
    (if keys
        (mapconcat 'key-description keys ", ")
      (concat "[M-x " (symbol-name command) "]"))))

(defun company-show-list-minibuffer ()
  (let* ((candidates company-completions)
         (width (frame-width))
         (string "")
         (i 0))
    (while (and candidates (< (length string) width))
      (let* ((candidate (pop candidates)))
        (setq string (concat string
                             (when (< i 10)
                               (concat "(" (number-to-string i) ") "))
                             (if (eq i company-selection)
                                 (propertize candidate 'face '(:underline t))
                               candidate)
                             "  "))
        (incf i)))
    (when (>= (length string) width)
      (setq string (concat (substring string 0 (- width 3)) "...")))
    (when company-verbose
      (setq string
            (format
             (concat "Press %s to expand common parts.\nUse %s and %s"
                     " to cycle through completions.\nPress %s to expand the"
                     " current selection. \n%s")
                    (company-key-string 'company-expand-common)
                    (company-key-string 'company-cycle)
                    (company-key-string 'company-cycle-backwards)
                    (company-key-string 'company-expand-top)
                    string)))
    (message "%s" string)
    (sit-for 999)))

;;; keymap ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar company-original-keymap nil)
(make-variable-buffer-local 'company-original-keymap)

(defun company-enable-active-keymap ()
  (setcdr (assoc 'company-mode minor-mode-map-alist)
          company-active-map))

(defun company-disable-active-keymap ()
  (setcdr (assoc 'company-mode minor-mode-map-alist)
          company-mode-map))

;;; completion buffer ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defconst company-completions-buffer-name "*Company Completions*")

(defun company-show-completion-buffer ()
  (let ((window (get-buffer-window company-completions-buffer-name)))
    (if window
        (with-current-buffer company-completions-buffer-name
          (if (pos-visible-in-window-p (point-max) window)
              (set-window-start window (point-min) nil)
            (with-selected-window window
                (scroll-up))))
      (unless company-completions
        (company-begin))
      (when company-completions
        (with-output-to-temp-buffer company-completions-buffer-name
                                     (display-completion-list
                                      company-completions
                                      (company-chop company-common)))
        (add-hook 'after-change-functions 'company-kill-completion-buffer nil t)
      ))))

(defun company-kill-completion-buffer (&optional a b c)
  (let ((buffer (get-buffer company-completions-buffer-name)))
    (when buffer
      (kill-buffer buffer)))
  (remove-hook 'after-change-functions 'company-kill-completion-buffer t))

;;; retroactive cycling ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar company-previous-completions nil)
(make-variable-buffer-local 'company-previous-completions)
(defvar company-last-expansion-overlay nil)
(make-variable-buffer-local 'company-last-expansion-overlay)

(defsubst company-mark-last-expansion (beg end completions)
  (assert (not company-last-expansion-overlay))
  (when (cdr completions)
    (setq company-previous-completions
          (cons
           (- (length (car completions)) (- end beg))
;;            (- (+ beg (length (car completions))) end)
           (cdr completions)))
    (setq company-last-expansion-overlay
          (company-put-overlay beg end 'face 'company-last-expansion-face
                               'evaporate t))))

(defsubst company-hide-last-expansion ()
  (when company-last-expansion-overlay
    (delete-overlay company-last-expansion-overlay)
    (setq company-last-expansion-overlay nil)
    (setq company-previous-completions nil)))

(defun company-cycle-last-expansion ()
  (interactive)
  (when (and (cdr company-previous-completions)
             (overlay-buffer company-last-expansion-overlay))
    (save-excursion
      (let ((inhibit-modification-hooks t))
        (goto-char (overlay-start company-last-expansion-overlay))
        (insert (substring (pop (cdr company-previous-completions))
                           (car company-previous-completions)))
        (delete-region (point) (overlay-end company-last-expansion-overlay))
        (unless (cdr company-previous-completions)
          (company-hide-last-expansion))))))

;;; overlays ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; TODO: this is called too often
(defun company-show-overlay ()
  (unless company-overlay
    (let* ((completion
            (company-chop (nth company-selection company-completions)))
           (length (length completion))
           (common-length (- (length company-common) (length company-prefix)))
           (beg (point)))
      (assert (not company-hide-overlay))
      (company-without-undo
       (save-excursion
         (insert completion)
         (assert (not company-overlay))
         (setq company-overlay
               (company-put-overlay beg (point)
                                    'face 'company-expand-face))
         (assert (not company-common-overlay))
         (setq company-common-overlay
               (company-put-overlay beg (+ beg common-length)
                                    'face 'company-common-face)))))))

;;; tooltips ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun company-show-overlay-tips ()
  (when (cdr company-completions)
    (when (eq company-display-style 'pseudo-tooltip)
      (company-show-list-pseudo-tooltip))))

(defun company-show-tips ()
  (when (cdr company-completions)
    (case company-display-style
      (minibuffer (company-show-list-minibuffer))
      (tooltip (company-show-list-tooltip)))))

(defun company-hide-overlay ()
  (company-without-undo
   (when company-overlay
     (delete-region (overlay-start company-overlay)
                    (overlay-end company-overlay))
     (delete-overlay company-overlay)
     (setq company-overlay nil))
   (when company-common-overlay
     (delete-overlay company-common-overlay)
     (setq company-common-overlay nil))
   (when company-hide-overlay
     (delete-overlay company-hide-overlay)
     (setq company-hide-overlay nil))
   ))

;;; pseudo tooltip ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun company-show-list-pseudo-tooltip (&optional point)
  (let* ((max-lines (- (window-height)
                       (count-lines (point) (window-start))))
         (candidates (company-pick-completions max-lines)))
    (company-show-pseudo-tooltip-at-point
     candidates
     (- company-selection (company-get-selection-offset max-lines)))))

(defun company-hide-pseudo-tooltip ()
  (dolist (ov company-pseudo-tooltip-overlays)
    (delete-overlay ov))
  (setq company-pseudo-tooltip-overlays nil))

(defun company-show-pseudo-tooltip-line (start replacement &optional no-insert)
  ;; start might be in the middle of a tab, which means we need to hide the
  ;; tab and add spaces
  (let ((end (+ start (length replacement)))
        beg-point end-point
        before-string after-string)
    (goto-char (point-at-eol))
    (if (< (current-column) start)
        (progn (setq before-string
                     (make-string (- start (current-column)) ? ))
               (setq beg-point (point)))
      (goto-char (point-at-bol)) ;; Emacs bug, move-to-column is wrong otherwise
      (move-to-column start)
      (setq beg-point (point))
      (when (> (current-column) start)
        (goto-char (1- (point)))
        (setq beg-point (point))
        (setq before-string (make-string (- start (current-column)) ? ))))
    (move-to-column end)
    (setq end-point (point))
    (let ((end-offset (- (current-column) end)))
      (when (> end-offset 0)
        (setq after-string (make-string end-offset ?b))))
    (when no-insert
      ;; prevent inheriting of faces
      (setq before-string (when before-string
                            (propertize before-string 'face 'default)))
      (setq after-string (when after-string
                           (propertize after-string 'face 'default))))
    (let ((string (concat before-string
                          replacement
                          after-string)))
      (if no-insert
          string
        (push (company-put-overlay beg-point end-point
                                   'invisible t
                                   'after-string string)
              company-pseudo-tooltip-overlays)))))

(defun company-show-pseudo-tooltip-at-point (lines &optional highlight)
  (company-hide-pseudo-tooltip)
  (let* ((start (mod (current-column) (window-width)))
         (lengths (mapcar 'length lines))
         (max-length (min (apply 'max lengths)
                          (- (window-width) start)))
         (i -1)
         (lines (mapcar*
                 '(lambda (line length)
                    (let ((diff (- max-length length)))
                      (propertize
                       ;; make the string the correct size
                       (if (> diff 0)
                           ;; pad line with spaces
                           (concat line (make-string diff ? ))
                         ;; we might be at the right end of the buffer
                         (substring line 0 max-length))
                       'face (if (equal (incf i) highlight)
                                 'company-pseudo-tooltip-selection-face
                               'company-pseudo-tooltip-face))))
                 lines lengths)))
    (save-excursion
      (let ((max (point-max)))
        (while (and lines (/= (vertical-motion 1) 0))
          (company-show-pseudo-tooltip-line (+ (current-column) start)
                                            (pop lines))
          )
        (when lines
          ;; append to end of buffer in one giant
          (let* ((newline (propertize "\n" 'face 'default))
                 (append newline))
            (while lines
              (setq append
                    (concat append
                            (company-show-pseudo-tooltip-line
                             (+ (current-column) start) (pop lines) t)
                             newline)))
            ;; Add the appended lines to the last overlay, unless we didn't
            ;; create any yet, or we aren't at point-max yet.  We have to
            ;; append, because otherwise two overlays, both at point-max, will
            ;; be in reversed order.
            (let ((ov (car-safe company-pseudo-tooltip-overlays)))
              (unless (and ov
                           (= (overlay-end ov) (point-max)))
                (setq ov (make-overlay (point-max) (point-max)))
                (push ov company-pseudo-tooltip-overlays))
              (overlay-put ov 'after-string
                           (concat (overlay-get ov 'after-string)
                                   append)))
            ))))))

;;; tooltip ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun company-show-list-tooltip (&optional point)
  (when (and (display-graphic-p)
             (fboundp 'x-show-tip))
    (let* ((max-lines 20)
           (lines (company-pick-completions max-lines))
           (max-len (apply 'max (mapcar 'length lines)))
           (highlight (- company-selection
                               (company-get-selection-offset max-lines)))
           (edges (window-inside-pixel-edges))
           (posn (posn-x-y (posn-at-point point)))
           (x (+ (car edges) (car posn)))
           (y (+ (cadr edges) (cdr posn)))
           (mp (mouse-pixel-position))
           (x-offset (- x (cadr mp)))
           (above (if (< (cdr (posn-x-y (posn-at-point point)))
                         (/ (display-pixel-height) 2)) 1 -1))
           (y-offset
            (+ (- y (cddr mp))
               (* (frame-char-height)
                  (if company-tooltip-os-shows-below 1 (length lines))
                  above
                  )))
           (x-max-tooltip-size '(1000 . 1000))
           (params '((internal-border-width . 0) (border-width . 0)))
           (fg (face-attribute 'tooltip :foreground))
           (bg (face-attribute 'tooltip :background))
           string)
      (when (stringp fg)
        (setq params (tooltip-set-param params 'foreground-color fg))
        (setq params (tooltip-set-param params 'border-color fg)))
      (when (stringp bg)
        (setq params (tooltip-set-param params 'background-color bg)))
      (dotimes (i (length lines))
        (let ((line (pop lines)))
          (setq string
                (concat
                 string
                 (if (= i highlight)
                     (propertize
                      (concat line
                              (make-string (- max-len (length line)) ? ) "\n")
                      'face 'company-tooltip-selection-face)
                   (concat line "\n"))))))
    (x-show-tip string (selected-frame) params 99999 x-offset y-offset))))

;;; Completion Functions ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun company-add-completion-rule (major-mode decider function
                                    &optional no-cache)
  "Add a completion rule for `company-mode'.
DECIDER may be a regular expression, a cons of expression and
subexpression or a function."
  (let ((pair (assoc major-mode company-completion-alist)))
    (unless pair
      (setq pair (cons major-mode nil))
      (push pair company-completion-alist))
    (push (list decider function :no-cache) (cdr pair))
    pair))

(defun company-clear-completion-rules ()
  (setq company-completion-alist))

(defun company-grab (regexp &optional expression)
  (when (save-excursion (re-search-backward regexp nil t))
    (or (match-string-no-properties (or expression 0)) "")))

(defun company-in-symbol-or-comment (&optional point)
  (let ((pos (syntax-ppss)))
    (or (nth 3 pos) (nth 4 pos))))

;;; debug ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; don't debug
(dolist (mess '("^No completions found$"
                "^No such completion$"
                "^company-mode not enabled$"))
  (add-to-list 'debug-ignored-errors (regexp-quote mess)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide 'company-mode)
