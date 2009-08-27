;;; sample.el --- A sample set of data for running money
;; Copyright (C) 2007 Justin Heyes-Jones
;; Keywords: personal finance money
;; This file is *NOT* part of GNU Emacs, it is part of emacs-easy-budget 
;; which is released under license GPL v3

(require 'emacs-easy-budget)

(defun my-bills(start-date)
  "Add your bills, weekly expenses and pay check here"
  (list 
   ; incomes
   (make-new-expense "salary" -1000 '(1 8 2009) 'monthly)

   ; bills 
   (make-new-expense "car insurance" 20 '(4 8 2009) 'monthly)
   (make-new-expense "car loan" 40 '(12 8 2009) 'biweekly)
   (make-new-expense "savings" 200 '(16 8 2009) 'monthly) 
   (make-new-expense "credit card" 10 '(2 8 2009) 'monthly) 
   (make-new-expense "cable" 70 '(25 8 2009) 'monthly) 
   (make-new-expense "rent" 400 '(3 8 2009) 'monthly)
   (make-new-expense "power" 20 '(15 8 2009) 'monthly)

   ; weekly budget items
   (make-new-expense "gas" 20 start-date 'weekly)
   (make-new-expense "groceries" 300 start-date 'weekly)
   (make-new-expense "entertainment" 100 start-date 'weekly)
))

(defun sample()
  (let ((today '(25 8 2009)))
    (forecast-to-csv 
     today
     '(30 6 10)
     2000
   (my-bills today)
   nil)))

(sample)

;;; sample.el ends here

