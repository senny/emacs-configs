;;;; Perspective
(add-to-list 'load-path (concat dotfiles-dir "/vendor/perspective.el"))
(require 'perspective)
(persp-mode)

;; Perspective Setup
(defmacro senny-persp (name &rest body)
  `(let ((initialize (not (gethash ,name perspectives-hash))))
     (persp-switch ,name)
     (when initialize ,@body)))

(defface senny-persp-selected-face
  '((t (:weight bold :foreground "chocolate4")))
  "The face used to highlight the current perspective on the modeline.")

(defun persp-format-name (name)
  "Format the perspective name given by NAME for display in `persp-modestring'."
  (let ((string-name (format "%s" name)))
    (if (equal name (persp-name persp-curr))
        (propertize string-name 'face 'senny-persp-selected-face))))

(defun persp-update-modestring ()
  "Update `persp-modestring' to reflect the current perspectives.
Has no effect when `persp-show-modestring' is nil."
  (when persp-show-modestring
    (setq persp-modestring
          (append '("[")
                  (persp-intersperse (mapcar 'persp-format-name (persp-names)) "")
                  '("]")))))

;; Perspective Defuns
(defun senny-persp-last ()
  (interactive)
  (persp-switch (persp-name persp-last)))


;;;; Perspective Definitions
(defun senny-persp/jabber ()
  (interactive)
  (senny-persp "@Jabber"
               (jabber-connect-all)
               (call-interactively 'jabber-display-roster)
               (switch-to-buffer jabber-roster-buffer)))

(defun senny-persp/irc ()
  (interactive)
  (senny-persp "@IRC"
               (erc)
               (dolist (channel '("emacs" "ruby" "cucumber"))
                 (erc-join-channel channel))))

(defun senny-persp/emacs ()
  (interactive)
  (senny-persp "@Emacs"))

(defun senny-persp/main ()
  (interactive)
  (senny-persp "main"))
