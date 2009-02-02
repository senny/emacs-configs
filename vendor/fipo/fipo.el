(defvar *fipo-view-path* "C:\\views\\"
  "Location where the clearcase views are located")
(defvar *fipo-gf-exclude*
  "/\\.|bomdef|\\.jar|\\.class")

(defvar *fipo-mode-map* (make-sparse-keymap))
(defvar *fipo-project-root* nil)
(defvar *fipo-project-view* nil)
(defvar *fipo-project-path* nil)
(defvar *fipo-project-files* nil)
(defvar *fipo-find-in-project-default* nil)

(defun fipo-bind-keys ()
  (define-key *fipo-mode-map* (kbd "C-S-f v") 'fipo-ido-find-view)
  (define-key *fipo-mode-map* (kbd "C-S-f a") 'fipo-ido-open-admin-ear-file)
  (define-key *fipo-mode-map* (kbd "C-S-f f") 'fipo-ido-open-fipo-ear-file)
  (define-key *fipo-mode-map* (kbd "C-S-f d") 'fipo-ido-open-day-ear-file)
  (define-key *fipo-mode-map* (kbd "C-S-s") 'fipo-find-in-project))

(defun fipo-ido-find-view ()
  (interactive)
  (let* ((view (ido-completing-read "View: "
                                    (directory-files *fipo-view-path* nil "^[^.]")))
         (path-to-view (concat *fipo-view-path* view)))
    (setq *fipo-project-view* view)
    (setq *fipo-project-root* path-to-view)
    (setq *fipo-project-path* (concat *fipo-project-root* "\\fipo\\se"))
    (message (concat *fipo-project-view* " selected"))))

(defun fipo-ido-open-view-file (&optional folder)
  (interactive)
  (let ((path (concat *fipo-project-path* folder)))
    (when (null path)
      (error "The given path is nil"))
    (find-file (concat path 
                       (ido-completing-read (concat path ": ")
                                            (fipo-view-files path))))))

(defun fipo-ido-open-admin-ear-file ()
  (interactive)
  (fipo-ido-open-view-file "\\admin-ear"))

(defun fipo-ido-open-fipo-ear-file ()
  (interactive)
  (fipo-ido-open-view-file "\\fipo-ear"))

(defun fipo-ido-open-day-ear-file ()
  (interactive)
  (fipo-ido-open-view-file "\\day-ear"))

(defun fipo-clear-cache ()
  (interactive)
  (setq *fipo-project-files* nil)
  (message "fipo-mode cache cleared."))

(defun fipo-view-files (path)
  (when (or (null *fipo-project-files*) (not (equal (car *fipo-project-files*) path)))
    (setq *fipo-project-files* (cons path 
                                     (split-string (shell-command-to-string
                                                    (concat
                                                     "find \"" (expand-file-name path) 
                                                     "\" -type f -printf \"\\\\%P\\n\" | grep -vE \""
                                                     *fipo-gf-exclude*))))))
  (cdr *fipo-project-files*))

(defun fipo-find-in-project (&optional pattern)
  (interactive)
  (let ((root *fipo-project-path*)
        (default *fipo-find-in-project-default*))
    (message "fipo-find-in-project")
    (when (null root)
      (error "No view selected"))
    (let ((re (read-string (concat "Search for "
                                   (if (and default (> (length default) 0))
                                       (format "[\"%s\"]" default)) ": ")
                           nil 'fipo-find-in-project-history default))
          (incpat (if pattern pattern "*")))
      (append fipo-find-in-project-history (list re))
      (setq *fipo-find-in-project-default* re)
      (let ((command
             (concat "grep -nr \"" re "\" \"" (expand-file-name root) "\"")))
        (compilation-start command 'grep-mode)))))

(defun fipo-debug-view ()
  (interactive)
  (fipo-execute-ant-command "debug.fipo.dev" "*fipo-server*"))


(defun fipo-execute-ant-command(target &optional buffer)
  (interactive)
  (when (get-buffer buffer)
    (save-excursion
    (set-buffer buffer)
    (kill-compilation)
    (erase-buffer)))
  (let ((fipo-buf (get-buffer-create buffer))
        (ant-command (concat "ant -buildfile " *fipo-project-path* "\\build\\ant\\build.xml " target)))
    (save-excursion
      (switch-to-buffer fipo-buf)
      (erase-buffer)
      (compilation-mode)
      (compile ant-command))))

;;;###autoload
(define-minor-mode fipo-mode "Fipo Minor Mode"
  :global t :lighter " fipo" :keymap *fipo-mode-map*
  (fipo-bind-keys))

(provide 'fipo)
;;; fipo.el ends here