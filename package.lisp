(cl:defpackage :clsql-helper
  (:use :cl :cl-user :iter)
  (:export
   ;; date stuff
   #:current-sql-date #:current-sql-time
   #:print-nullable-date #:print-nullable-datetime
   #:date-day #:date-year #:date-month
   #:month-string #:month-day-string
   #:convert-to-clsql-date #:convert-to-clsql-datetime
   #:convert-to-clsql-date! #:convert-to-clsql-datetime!
   #:clsql-datetime-equal
   #:clsql-date/times->utime
   #:clsql-date/times->local-time
   #:iso8601-datestamp
   #:iso8601-timestamp
   #:first-of-the-month
   #:last-of-the-month
   #:first-of-next-month
   #:next-month
   #:last-month
   #:days-in-month
   #:day-before
   #:+a-month+
   #:+a-day+
   #:date-diff

   ;; rest
   #:clsql-get-val #:clsql-exp #:db-string
   #:clsql-ands #:clsql-and
   #:clsql-ors #:clsql-or
   #:clsql-column-name
   #:by-id #:by-col
   #:primary-key-slot-names #:primary-key-where-clauses
   #:db-eql #:new-object-p #:save-failed
   #:pretty-print-sql
   #:coerce-value-to-db-type #:format-value-for-database
   #:log-database-command

   #:table-name-exp #:column-name-exp
   #:table-name-string #:column-name-string
   #:db-type-from-lisp-type

   ;;connection functions
   #:with-database-function
   #:with-database
   #:with-a-database

   ;;migration functions
   #:*migration-table-name*
   #:migrations
   ))

;; Put clsql into the features list so that we can
;; conditionally compile things based on this
(pushnew :clsql *features*)
