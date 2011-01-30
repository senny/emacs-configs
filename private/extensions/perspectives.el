;;;; Perspective
(eval-after-load 'perspective
  '(progn

     ;; Perspective Setup
     (defmacro senny-persp (name &rest body)
       `(let ((initialize (not (gethash ,name perspectives-hash)))
              (current-perspective persp-curr))
          (persp-switch ,name)
          (when initialize ,@body)
          (setq persp-last current-perspective)))

     (defun persp-format-name (name)
       "Format the perspective name given by NAME for display in `persp-modestring'."
       (let ((string-name (format "%s" name)))
         (if (equal name (persp-name persp-curr))
             (propertize string-name 'face 'persp-selected-face))))

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

     (defun senny-persp/terminal ()
       (interactive)
       (senny-persp "@terminal"
                    (multi-term-next)
                    (jone-term-binding-fix)))

     (defun senny-persp/emacs ()
       (interactive)
       (senny-persp "@Emacs"))

     (defun senny-persp/org ()
       (interactive)
       (senny-persp "@org"
                    (find-file (first org-agenda-files))))

     (defun senny-persp/main ()
       (interactive)
       (senny-persp "main"))
     ))
