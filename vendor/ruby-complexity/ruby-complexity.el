;;; ruby-complexity.el --- Display ruby code complexity to the left of buffers

;; Modified from pycomplexity.el by Geoffrey Grosenbach http://peepcode.com

;; Copyright (C) 2009 Ignas Mikalajunas

;; Author: Ignas Mikalajunas
;; Keywords: convenience

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file GPL.txt .  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;; Display complexity information for the current buffer.

;; Add to your .emacs:

;;    (add-to-list 'load-path "~/.site-lisp/ruby-complexity/")

;;    (require 'linum)
;;    (require 'ruby-complexity)
;;    (add-hook 'ruby-mode-hook
;;        (function (lambda ()
;;          (flymake-mode)
;;          (linum-mode)
;;          (ruby-complexity-mode))))
;;
;; NOTE: Also needs flymake-mode and the Ruby flog gem.

;;; Code:

(defconst ruby-complexity-version "0.1")

(defvar complexity-bad-score 30
  "Complexities greater than this will be marked bad.")
(make-variable-buffer-local 'complexity-bad-score)
(defvar complexity-good-score 15
  "Complexities lower than this will be marked good.")
(make-variable-buffer-local 'complexity-bad-score)


(defvar complexity-last-change 0 "Time last change to some ruby buffer happened.")
(defvar complexity-data nil "Calcuated code complexity information for this buffer.")
(make-variable-buffer-local 'complexity-data)

(defgroup ruby-complexity nil
  "Show complexity information to the left of buffers"
  :group 'convenience)

(defface ruby-complexity-complexity-low
  '((t (:foreground "#333")))
  "Face that marks simple code "
  :group 'ruby-complexity)

(defface ruby-complexity-complexity-normal
  '((t (:foreground "#222")))
  "Face that marks normal code "
  :group 'ruby-complexity)

(defface ruby-complexity-complexity-high
  '((t (:foreground "#111")))
  "Face that marks complex code "
  :group 'ruby-complexity)

(defcustom ruby-complexity-delay 20
  "Update coverage information once in this many seconds."
  :group 'ruby-complexity
  :type 'int)

(defcustom ruby-complexity-ruby "ruby"
  "Ruby interpreter used to run the complexity calculation script."
  :group 'ruby-complexity
  :type 'string)

(defcustom ruby-complexity-script
  (expand-file-name "ruby-complexity.rb"
                    (file-name-directory (or load-file-name buffer-file-name)))
  "Ruby-Complexity script."
  :group 'ruby-complexity
  :type 'string)

;;;###autoload
(define-minor-mode ruby-complexity-mode
  "Toggle display complexity of the Ruby code you are editing."
  :lighter ""                           ; for desktop.el
  (if ruby-complexity-mode
      (progn
        (add-hook 'after-change-functions 'ruby-complexity-on-change nil t)
        (add-hook 'after-save-hook 'ruby-complexity-on-change-force nil t)
        (setf linum-format 'ruby-complexity-line-format)
        (ruby-complexity-on-change-force))
    (setf linum-format 'dynamic)
    (remove-hook 'after-change-functions 'ruby-complexity-on-change t)))

(defun ruby-complexity-get-complexity (line data)
  (multiple-value-bind (face str complexity)
      (loop for info in data
            for from = (first info)
            for to = (second info)
            for complexity = (third info)
            when (and (>= line from)
                      (<= line to))
            return (cond ((> complexity complexity-bad-score) (values 'ruby-complexity-complexity-high (number-to-string complexity) complexity))
                         ((> complexity complexity-good-score) (values 'ruby-complexity-complexity-normal (number-to-string complexity) complexity))
                         (t (values 'ruby-complexity-complexity-low (number-to-string complexity) complexity)))
            when (< line from)
            return (values 'default " " 0))
    (if face (values face str complexity)
      (values 'default " " 0))))

(defun ruby-complexity-line-format (line)
  (multiple-value-bind (face str complexity)
      (ruby-complexity-get-complexity line complexity-data)
    (propertize str 'face face
                'help-echo (format "Complexity of this function is %d" complexity))))


(defun ruby-complexity-make-buffer-copy ()
  (let*  ((source-file-name       buffer-file-name)
          (file-name (flymake-create-temp-inplace source-file-name "complexity")))
    (make-directory (file-name-directory file-name) 1)
    (write-region nil nil file-name nil 566)
    file-name))

(defun ruby-complexity-get-raw-complexity-data (file-name)
  (shell-command-to-string (format "%s %s %s"
                                   ruby-complexity-ruby
                                   ruby-complexity-script
                                   file-name)))

(defun ruby-complexity-on-change-force (&optional beg end len)
  (ruby-complexity-on-change beg end len t))

;; TODO: Should use built-in emacs timer as shown here:
;; http://github.com/technomancy/dotfiles/blob/master/.emacs.old/idle-highlight.el
(defun ruby-complexity-on-change (&optional beg end len force)
  (let ((since-last-change (- (float-time) complexity-last-change)))
    (when (or (> since-last-change ruby-complexity-delay) force)
      (setf complexity-last-change (float-time))
      (let* ((temp-source-file-name (ruby-complexity-make-buffer-copy))
             (result (ruby-complexity-get-raw-complexity-data temp-source-file-name))
             (data (loop for line in (split-string result "[\n\r]+")
                         for parsed-line = (loop for item in (split-string line)
                                                 when item collect (read item))
                         when (and parsed-line
                                   (equal (car (last parsed-line)) 'function))
                         collect (subseq parsed-line 0 3))))
        (when data (setf complexity-data data))
        (delete-file temp-source-file-name)))))

(provide 'ruby-complexity)
;;; ruby-complexity.el ends here
