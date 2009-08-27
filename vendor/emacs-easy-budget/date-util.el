;;; date-util.el --- Some date utilities used in easy-emacs-budget
;; Copyright (C) 2007 Justin Heyes-Jones
;; Keywords: personal finance money
;; This file is *NOT* part of GNU Emacs, it is part of emacs-easy-budget 
;; which is released under license GPL v3

(require 'timezone)

(defconst time-seconds-in-day
  (* 60 60 24)
  "Number of seconds in a day")

(defconst time-names
  (loop for name in '(SEC MINUTE HOUR DAY MONTH YEAR DOW DST ZONE) 
	for idx from 0 
	collect
	(list name idx))
"An association list that's useful for dealing with time values (see decode-time)
p.s: I found out about with-decoded-time-value after I wrote this")

(defun time-get-day(time)
  "Get day of month from TIME"
  (time-get-decoded-value (decode-time time) 'DAY))

(defun time-dc-year(decoded-time)
  "Get the year of the DECODED-TIME"
  (time-get-decoded-value decoded-time 'YEAR))

(defun time-dc-month(decoded-time)
  "Get the month of the DECODED-TIME"
  (time-get-decoded-value decoded-time 'MONTH))

(defun time-dc-day(decoded-time)
  "Get the day of the DECODED-TIME"
  (time-get-decoded-value decoded-time 'DAY))

(defun time-get-decoded-value(decoded-time value-name)
  "Given a DECODED-TIME return the value with name VALUE-NAME from 
SEC MINUTE HOUR DAY MONTH YEAR DOW DST ZONE"
  (let ((entry (assoc value-name time-names)))
    (if entry
	(nth (second entry) decoded-time)
      (error "couldn't get value name %s from decoded string" value-name))))

(defun time-days-in-month(time)
  "Number of days in month at TIME"
  (let ((datetime (decode-time time)))
    (timezone-last-day-of-month (time-get-decoded-value datetime 'MONTH) (time-get-decoded-value datetime 'YEAR))))

(defun time-diff-days(t1 t2)
  "Return the integer number of whole days between times T1 and T2"
  (let* ((diff (time-subtract t1 t2))
	 (float-days (time-to-number-of-days diff)))
    (truncate (abs float-days))))

; (time-diff-days (current-time) (encode-time 0 0 0 19 9 1971))

(defun time-nth-day(time n)
  "Given a TIME return a time the same but on the Nth day of the same month
So if you give it a time that is the 18th Feb, and n is 12, it will return a time
that is the 12th Feb"
  (let ((decoded-time (decode-time time)))
    (encode-time (time-get-decoded-value decoded-time 'SEC)
		 (time-get-decoded-value decoded-time 'MINUTE)
		 (time-get-decoded-value decoded-time 'HOUR)
		 n
		 (time-get-decoded-value decoded-time 'MONTH)
		 (time-get-decoded-value decoded-time 'YEAR))))

;; Add a number of days to a date and get the new date

(defun time-add-days(time days)
  "Add DAYS number of days to the TIME"
  (let* ((days-as-seconds (* days time-seconds-in-day))
	 (seconds-as-time (seconds-to-time days-as-seconds)))
    (time-add seconds-as-time time)))
    
;; Compare dates

; (time-less-p t1 t2)

;; Get date as string in specified format

(defun time-to-string-dd-mm-yyyy(time)
  "Format the TIME predicably by the name of the function"
  (format-time-string "%02d/%02m/%Y" time nil))

(defun time-to-string-mm-dd-yyyy(time)
  "Format the TIME predicably by the name of the function"
  (format-time-string "%02m/%02d/%Y" time nil))

(defun time-to-string-mmm-dd-yyyy(time)
  "Format the TIME predicably by the name of the function"
  (format-time-string "%02b %02d %Y" time nil))

; (time-to-string-dd-mm-yyyy (current-time))
; (time-to-string-mm-dd-yyyy (current-time))
; (time-to-string-mmm-dd-yyyy (current-time))

; Comparison functions that work by calendar day only

(defun time-fix-year(y)
  "Handles 2 or 4 digit years somewhat arbitarily, will make sense until 2028"
  (if (> y 100)
      y
    (if (> y 28)
	(+ 1900 y)
      (+ 2000 y))))

; (time-fix-year 9)
; (time-fix-year 71)
; (time-fix-year 1971)
; (time-fix-year 666)

(defun time-from-dd-mm-yyyy(date-lst)
  "Take a user input time as a list dd mm yy and stick it in a time using
encode time, if the year is only yy then I handle that"
  (destructuring-bind (day month year) date-lst
    (encode-time 0 0 0 day month (time-fix-year year))))

; (time-from-dd-mm-yyyy '(19 9 1971))
; (time-from-dd-mm-yyyy '(19 9 71))
; (time-from-dd-mm-yyyy '(24 8 2010))
; (time-from-dd-mm-yyyy '(24 8 10))

(defun time-day-later-or-equal(t1 t2)
  "Returns true if T1 is the same day, or a later day than T2"
  (not (time-day-earlier t1 t2)))

(defun time-day-earlier(t1 t2)
  "Returns true if T1 falls on an earlier calendar day than T2"
  (< (time-to-days t1) (time-to-days t2)))

; some tests
; (time-day-earlier (encode-time 0 0 0 19 9 1971) (encode-time 0 0 0 21 3 1971))
; (time-day-earlier (encode-time 0 0 0 21 3 1971) (encode-time 0 0 0 19 9 1971))
; (time-day-earlier (encode-time 0 0 0 20 3 1971) (encode-time 0 0 0 21 3 1971))
; (time-day-earlier (encode-time 0 0 0 21 3 1971) (encode-time 0 0 0 20 3 1971))
; (time-day-earlier (encode-time 0 20 9 21 3 1971) (encode-time 0 30 9 21 3 1971)) ; same day should fail
; (time-day-earlier (encode-time 23 59 9 21 3 1971) (encode-time 0 0 0 22 3 1971)) ; check for midnight problems
; (time-day-later-or-equal (encode-time 0 0 0 21 9 1971) (encode-time 0 0 0 20 9 1971))

(provide 'date-util)

;;; date-util.el ends here
