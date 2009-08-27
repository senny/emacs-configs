;;; date-util.el --- Some date utilities used in easy-emacs-budget
;;; Emacs-Lisp  program that takes a list of expenses 
;;; named outgoings (and incomes if the amount is negative)
;;; which can be displayed and calculating weekly, showing 
;;; the cumulative balance each week
;; Copyright (C) 2007 Justin Heyes-Jones
;; Keywords: personal finance money
;; This file is *NOT* part of GNU Emacs, it is part of emacs-easy-budget 
;; which is released under license GPL v3

; TODO 
;   move this list to the google code issue list
;   let user pass in format function for printing items
;   monthly and bi monthly payments should have the day they fall on stored 
; DONE use emacs dates, nothing from my emacs library (so you can release and blog it)
;   sort transactions by date
; DONE remember high and low balance then display
;   
; bugs
;    Kind of weird how weekly budget stuff is calculated, needs to pro-rate? Possibly integrate with budget worksheet
; fixed bugs
;   when viewed in excel theres a formatting problem with some dates (fixed, was invalid dates due to mm.dd when 
;   excel is set to dd.mm in my locale

(require 'date-util)

(defstruct expense
  name
  amount
  last-paid
  frequency
  next-due)
  
(defun make-new-expense(name amount last-paid frequency)
  "An expense includes NAME cash AMOUNT which is negative for income, FREQUENCY type 
LAST-PAID should be a '(dd mm yy) style date which will be converted and stored as a time
NEXT-DUE will be calculated"
  (make-expense :name name
		:amount amount
		:last-paid (time-from-dd-mm-yyyy last-paid)
		:frequency frequency
		:next-due 0))

(defun days-until-due(ft date-last-paid)
  "Given the frequency type get the number of days"
  (case ft
    ('weekly 7)
    ('biweekly 14)
    ('monthly (time-days-in-month date-last-paid))
    ('bimonthly 
     (let* ((month-days (time-days-in-month date-last-paid))
	    (middle-day 15)
	    (diff-middle (abs (- (time-get-day date-last-paid) middle-day))))
       ;if near middle set as end
       ;if less than middle set as middle
       ;otherwise set as next month middle
       (if (< diff-middle 7)
	   (- month-days (time-get-day date-last-paid))
	 (if (< (time-get-day date-last-paid) middle-day)
	     (- 15 (time-get-day date-last-paid))
	   (+ 15 (- month-days (time-get-day date-last-paid)))))))
    (t (error "no handler for payment frequency %s" f))))

(defun expense-calc-next-due(expense)
  "Figure out the date an expense will be due, set the next-due field appropriately"
  (setf (expense-next-due expense)
	(time-add-days (expense-last-paid expense)
		       (days-until-due (expense-frequency expense) (expense-last-paid expense)))))

(defun calculate-next-due(expenses)
  (dolist (e expenses)
    (expense-calc-next-due e)))

(defun show-balance-forecast-range(current-date end-date current-balance expenses)
  "Calculate and show the running balance foar a specified range of dates
This is formatted so that saving as a .csv file will result in a spreadsheet with a format
date, payee, amount, balance"
  (insert (format "Balance forecast beginning %s -> %s\n" 
		  (time-to-string-mmm-dd-yyyy current-date)
		  (time-to-string-mmm-dd-yyyy end-date)))
  (calculate-next-due expenses)
  (let ((previous-date nil) (high current-balance) (low current-balance))
    (while (time-day-earlier current-date end-date) 
      (setf previous-date current-date)
      (setf current-date (time-add-days current-date 7))
      (insert (format "%s, to %s, $%-04.2f, $%-04.2f\n" 
		      (time-to-string-dd-mm-yyyy previous-date) 
		      (time-to-string-dd-mm-yyyy current-date) 
		      0 current-balance))
      (dolist (e expenses)
	(when (and
	       (time-day-earlier (expense-next-due e) current-date)
	       (time-day-later-or-equal (expense-next-due e) previous-date))
	  (setf current-balance (- current-balance (expense-amount e)))
	  (setf (expense-last-paid e) (expense-next-due e))
	  (expense-calc-next-due e)
	  (insert (format "%s, %s, $%-04.2f, $%-04.2f\n" 
			  (time-to-string-dd-mm-yyyy (expense-last-paid e))
			  (expense-name e) 
			  (expense-amount e) 
			  current-balance))
	  (if (< current-balance low)
	      (setf low current-balance))
	  (if (> current-balance high)
	      (setf high current-balance)))))
    (goto-line 2)
    (insert (format "\n(High $%-04.2f, Low $%-04.2f)\n\n" high low))))

(defun forecast-to-csv(current-date end-date current-balance expenses 
				    &optional open-csv)
  "Creates a forecast into a buffer named by todays date and writes a 
CSV file. CURRENT-DATE is a user date (list of dd mm yy) END-DATE is the date to 
stop the forecast and CURRENT-BALANCE is the balance to begin with. EXPENSES is a list
of expenses (and incomes)
If this is on windows and OPEN-CSV is not nil it will launch whatever you have 
configured to read csv files"
  (let* ((name (format-time-string "finances-%m%d%y.csv"))
	 (filename (format "./%s" name)))
    (save-excursion 
      (set-buffer 
       (get-buffer-create name))
      (goto-line 1)
      (erase-buffer)
      (show-balance-forecast-range (time-from-dd-mm-yyyy current-date) 
				   (time-from-dd-mm-yyyy end-date)
				   current-balance expenses)
      (write-file filename)

      (if (and open-csv (featurep 'w32-win))
	  (shell-command (format "start %s" filename))))))

(provide 'emacs-easy-budget)

;;; emacs-easy-budget.el ends here