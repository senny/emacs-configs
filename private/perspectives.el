;;;; Perspective
(add-to-list 'load-path (concat dotfiles-dir "/vendor/perspective.el"))
(require 'perspective)
(persp-mode)

(defmacro senny-persp (name &rest body)
  `(let ((initialize (not (gethash ,name perspectives-hash))))
     (persp-switch ,name)
     (when initialize ,@body)))

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

(provide 'senny-perspectives)
