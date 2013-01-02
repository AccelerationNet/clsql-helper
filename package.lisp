(cl:defpackage :clsql-helper
  (:use :cl :cl-user :iter)
  (:export
   ;; dirty
   #:dirty-slots
   #:slot-dirty?
   #:dirty-test
   #:record-dirty?
   #:reset-dirty?
   #:find-dirty-test
   #:dirty-slots-mixin
   #:dirty-db-slots-mixin


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
   #:first-of-the-month?
   #:last-of-the-month
   #:first-of-next-month
   #:next-month
   #:last-month
   #:days-in-month
   #:day-before
   #:+a-second+
   #:+a-minute+
   #:+an-hour+
   #:+a-month+
   #:+a-day+
   #:+a-year+
   #:+a-negative-second+
   #:+a-negative-minute+
   #:+a-negative-hour+
   #:+a-negative-month+
   #:+a-negative-day+
   #:+a-negative-year+

   #:date-diff
   #:before-day-of-month
   #:after-day-of-month


   ;; rest
   #:join-slot?
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

   #:db-exec
   #:db-select
   #:db-query
   #:db-scalar
   #:db-select-scalar
   #:db-query-plists
   #:db-objs
   #:db-objs-select
   #:make-instance-plist
   #:make-instances
   #:make-instances-setting-slot-value
   #:make-instances-setting-accessors
   #:make-instances-setting-access
   #:save!

   #:table-name-exp #:column-name-exp
   #:table-name-string #:column-name-string
   #:db-type-from-lisp-type

   ;;connection functions
   #:*connection-settings*
   #:with-database-function
   #:with-database
   #:with-a-database

   #:with-command-logging
   #:*command-log-stream*
   #:with-command-logging-to-string

   ;;migration functions
   #:*migration-table-name*
   #:migrations
   ))

;; Put clsql into the features list so that we can
;; conditionally compile things based on this
(pushnew :clsql *features*)
