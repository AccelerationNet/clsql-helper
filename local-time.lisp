(in-package :clsql-helper)

(defmethod date-day ((d local-time:timestamp))
  (local-time:timestamp-day d))

(defmethod date-month ((d local-time:timestamp))
  (local-time:timestamp-month d))

(defmethod date-year ((d local-time:timestamp))
  (local-time:timestamp-year d))

(defmethod iso8601-timestamp ((d local-time:timestamp))
  (iso8601-timestamp (convert-to-clsql-datetime d)))

(defmethod convert-to-clsql-datetime ((d local-time:timestamp))
  (local-time->clsql-datetime val))

(defmethod convert-to-clsql-date ((d local-time:timestamp))
  (convert-to-clsql-date
   (local-time->clsql-datetime val)))

(defun clsql-date/times->local-time (obj)
  "obj is either a wall-time or a date"
  (apply #'local-time:encode-timestamp
	 ;;we dont want day of week
         (multiple-value-bind (usec second minute hour day month year)
             (clsql-sys:decode-time (convert-to-clsql-datetime obj))
           (list (* 1000 usec) second minute hour day month year ))))

(defun local-time->clsql-datetime (obj)
  "obj is either a wall-time or a date"
  (apply #'clsql-sys:make-time
	 ;;we dont want day of week
         (multiple-value-bind (nsec second minute hour day month year)
             (clsql-sys:decode-time (convert-to-clsql-datetime obj))
           (list year month day hour minute second (floor (/ 1000 nsec))))))