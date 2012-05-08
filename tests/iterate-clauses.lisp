(in-package :clsql-helper-test)
(cl-interpol:enable-interpol-syntax)
(clsql-sys:file-enable-sql-reader-syntax)

(define-test test-iterate-clauses-date
  (assert-equalp
   '("4/1/2012" NIL "4/3/2012" "4/1/2012" "4/5/2012" "4/3/2012")
   (iter
     (for i from 0 to 2)
     (for d from-date "4/1/2012" by (clsql-sys:make-duration :day 2))
     (for y previous d)
     (collect (print-nullable-date d))
     (collect (print-nullable-date y))
     )))
(define-test test-iterate-clauses-date-2
  (assert-equalp
   '("4/1/2012" NIL "4/3/2012" "4/1/2012" "4/5/2012" "4/3/2012")
   (iter
     (for i from 0 to 2)
     (for d from-datetime "4/1/2012" by (clsql-sys:make-duration :day 2))
     (for y previous d)
     (collect (print-nullable-date d))
     (collect (print-nullable-date y))
     )))
(define-test test-iterate-clauses-date-3
  (assert-equalp
   '("4/1/2012" NIL "4/3/2012" "4/1/2012" )
   (iter
     (for d from-date "4/1/2012" to "4/5/2012" by (clsql-sys:make-duration :day 2))
     (for y previous d)
     (collect (print-nullable-date d))
     (collect (print-nullable-date y))
     )
   :date-to))
(define-test test-iterate-clauses-date-4
  (assert-equalp
   '("4/1/2012" NIL "4/3/2012" "4/1/2012" )
   (iter
     (for d from-datetime "4/1/2012" to "4/5/2012" by (clsql-sys:make-duration :day 2))
     (for y previous d)
     (collect (print-nullable-date d))
     (collect (print-nullable-date y))
     )
   :datetime-to))
(define-test test-iterate-clauses-date-5
  (assert-equalp
   '("4/1/2012" NIL "4/3/2012" "4/1/2012" "4/5/2012" "4/3/2012")
   (iter
     (for d from-date "4/1/2012" thru "4/5/2012" by (clsql-sys:make-duration :day 2))
     (for y previous d)
     (collect (print-nullable-date d))
     (collect (print-nullable-date y))
     )
   :date-thru))

(define-test test-iterate-clauses-date-6
  (assert-equalp
   '("4/1/2012" NIL "4/3/2012" "4/1/2012" "4/5/2012" "4/3/2012")
   (iter
     (for d from-datetime "4/1/2012" thru "4/5/2012" by (clsql-sys:make-duration :day 2))
     (for y previous d)
     (collect (print-nullable-date d))
     (collect (print-nullable-date y))
     )
   :datetime-thru))

