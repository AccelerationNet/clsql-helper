(in-package :clsql-helper-test)
(cl-interpol:enable-interpol-syntax)
(clsql-sys:file-enable-sql-reader-syntax)

(defparameter +iterate-tests+
  '(test-iterate-clauses-date test-iterate-clauses-date-2
    test-iterate-clauses-date-3 test-iterate-clauses-date-4
    test-iterate-clauses-date-5 test-iterate-clauses-date-6
    test-iterate-clauses-negative-date test-iterate-clauses-negative-date-2))

(define-test test-iterate-clauses-date (:tags '(iterate-clauses))
  (assert-equalp
   '("4/1/2012" NIL "4/3/2012" "4/1/2012" "4/5/2012" "4/3/2012")
   (iter
     (for i from 0 to 2)
     (for d from-date "4/1/2012" by (clsql-sys:make-duration :day 2))
     (for y previous d)
     (collect (print-nullable-date d))
     (collect (print-nullable-date y))
     )))

(define-test test-iterate-clauses-date-2 (:tags '(iterate-clauses))
  (assert-equalp
   '("4/1/2012" NIL "4/3/2012" "4/1/2012" "4/5/2012" "4/3/2012")
   (iter
     (for i from 0 to 2)
     (for d from-datetime "4/1/2012" by (clsql-sys:make-duration :day 2))
     (for y previous d)
     (collect (print-nullable-date d))
     (collect (print-nullable-date y))
     )))

(define-test test-iterate-clauses-date-3 (:tags '(iterate-clauses))
  (assert-equalp
   '("4/1/2012" NIL "4/3/2012" "4/1/2012" )
   (iter
     (for d from-date "4/1/2012" to "4/5/2012" by (clsql-sys:make-duration :day 2))
     (for y previous d)
     (collect (print-nullable-date d))
     (collect (print-nullable-date y))
     )
   :date-to))

(define-test test-iterate-clauses-date-4 (:tags '(iterate-clauses))
  (assert-equalp
   '("4/1/2012" NIL "4/3/2012" "4/1/2012" )
   (iter
     (for d from-datetime "4/1/2012" to "4/5/2012" by (clsql-sys:make-duration :day 2))
     (for y previous d)
     (collect (print-nullable-date d))
     (collect (print-nullable-date y))
     )
   :datetime-to))
(define-test test-iterate-clauses-date-5 (:tags '(iterate-clauses))
  (assert-equalp
   '("4/1/2012" NIL "4/3/2012" "4/1/2012" "4/5/2012" "4/3/2012")
   (iter
     (for d from-date "4/1/2012" thru "4/5/2012" by (clsql-sys:make-duration :day 2))
     (for y previous d)
     (collect (print-nullable-date d))
     (collect (print-nullable-date y))
     )
   :date-thru))

(define-test test-iterate-clauses-date-6 (:tags '(iterate-clauses))
  (assert-equalp
   '("4/1/2012" NIL "4/3/2012" "4/1/2012" "4/5/2012" "4/3/2012")
   (iter
     (for d from-datetime "4/1/2012" thru "4/5/2012" by (clsql-sys:make-duration :day 2))
     (for y previous d)
     (collect (print-nullable-date d))
     (collect (print-nullable-date y))
     )
   :datetime-thru))

(define-test test-iterate-clauses-negative-date (:tags '(iterate-clauses))
  (assert-equalp
   '("4/1/2012" "3/31/2012" "3/30/2012" "3/29/2012" "3/28/2012")
   (iter
     (for d from-datetime "4/1/2012" thru "3/28/2012" by :negative-day)
     (collect (print-nullable-date d)))
   :datetime-thru-negative-day))

(define-test test-iterate-clauses-negative-date-2  (:tags '(iterate-clauses))
  (assert-equalp
   '("4/1/2012" "3/1/2012" "2/1/2012")
   (iter
     (for d from-datetime "4/1/2012" to "1/1/2012" by :negative-month)
     (collect (print-nullable-date d)))
   :datetime-thru-negative-month))

(define-test test-iterate-clauses-negative-date-3  (:tags '(iterate-clauses))
  (assert-equalp
   '("4/1/2012" "3/1/2012" "2/1/2012")
   (iter
     (for d from-datetime "4/1/2012" to "1/1/2012" by :negative-month)
     (collect (print-nullable-date d)))
   :datetime-thru-negative-month))

