
(in-package :clsql-helper)
(cl-interpol:enable-interpol-syntax)
(clsql:file-enable-sql-reader-syntax)

(defun current-sql-date ()
  "current date"
  (clsql-sys:get-date))

(defun current-sql-time ()
  "current date and time"
  (clsql-sys:get-time))



(defun print-nullable-date (field &key (in-utc? nil))
  "if the date exists, prints m?m/d?d/yyyy"
  (when field
    (typecase field
      (string field)
      (T (clsql:print-date
	  (typecase field
	    (clsql-sys:date
             (funcall (if in-utc?
                          #'clsql-sys:time-to-utc
                          #'clsql-sys:time-to-localtime)
                      (clsql-sys::date->time field)))
	    (clsql-sys:wall-time
             (funcall (if in-utc?
                          #'clsql-sys:time-to-utc
                          #'clsql-sys:time-to-localtime)
                      field)))
	  :day)))))

(defmethod print-object ((o clsql-sys:date) stream)
  (let ((date (clsql-sys::iso-datestring o)))
    (if *print-escape*
	(print-unreadable-object (o stream :type T :identity T)
	  (format stream "~A" date))
	(format stream "~A" date))))

(defgeneric date-day (d)
  (:documentation "Given an object that encodes a date, return the day component")
  (:method (d)
    (etypecase d
      (clsql-sys:date
       (third (multiple-value-list (clsql-sys:date-ymd d))))
      (clsql-sys:wall-time
       (third (multiple-value-list (clsql-sys:time-ymd d))))
      ((or string integer)
       (date-day (convert-to-clsql-datetime d)))
      (null nil))))

(defgeneric date-year (d )
  (:documentation "Given an object that encodes a date, return the year component")
  (:method (d)
    (etypecase d
      (clsql-sys:date (clsql-sys:date-ymd d))
      (clsql-sys:wall-time (clsql-sys:time-ymd d))
      ((or string integer)
       (date-year (convert-to-clsql-datetime d)))
      (null nil))))

(defgeneric date-month (d)
  (:documentation "Given an object that encodes a date, return the month component")
  (:method (d)
    (etypecase d
      (clsql-sys:date
       (second (multiple-value-list (clsql-sys:date-ymd d))))
      (clsql-sys:wall-time
       (second (multiple-value-list (clsql-sys:time-ymd d))))
      ((or string integer)
       (date-month (convert-to-clsql-datetime d)))
      (null nil))))

(defun month-string  (d)
  "Converts the date to the full name, January, February,etc"
  (let ((d (date-month d)))
    (when d (clsql-sys:month-name d))))

(defun month-day-string  (d)
  "prints dates as January 3"
  (let ((d (date-day d))
        (m (month-string d)))
    (when (and d m) #?"${m} ${d}")))

(defun print-nullable-datetime (field &key (in-utc? nil))
  "if the date exists, prints mm/dd/yyyy hh:mm:ss"
  (let ((*print-pretty* nil))
    (when field
      (typecase field
	(string field)
	(T (multiple-value-bind (usec second minute hour day month year)
	       (clsql-sys:decode-time
                (funcall (if in-utc?
                             #'clsql-sys:time-to-utc
                             #'clsql-sys:time-to-localtime)
                 (convert-to-clsql-datetime field)))
	     (declare (ignore usec))
	     (format nil "~2,'0d/~2,'0d/~4,'0d ~2,'0d:~2,'0d:~2,'0d"
                     month day year hour minute second)))))))

(defun print-timestamp (field &key (in-utc? nil))
  "if the date exists, prints yyyy-mm-dd hh:mm:ss.uuuuuu"
  (let ((*print-pretty* nil))
    (when field
      (typecase field
	(string field)
	(T (multiple-value-bind (usec second minute hour day month year is-utc?)
	       (clsql-sys:decode-time
                (funcall (if in-utc?
                             #'clsql-sys:time-to-utc
                             #'clsql-sys:time-to-localtime)
                         (convert-to-clsql-datetime field)))
	     (format nil "~4,'0d-~2,'0d-~2,'0d ~2,'0d:~2,'0d:~2,'0d.~3,'0d~a"
                     year month day hour minute second (floor usec 1000)
                     (if is-utc? "Z" "")
                     )))))))

(defmethod print-object ((o clsql:wall-time) stream)
  (let ((date (clsql-sys:iso-timestring o)))
    (if *print-escape*
	(print-unreadable-object (o stream :type T :identity T)
		  (format stream "~A" date))
    (format stream "~A" date))))

(defun clsql-datetime-equal (x y)
  "Tries to handle full datetime equality reguardless of the format
    (string datestamp, date, datetime, utime)"
  (flet ((cast (x)
	   (typecase x
	     (integer (clsql-sys:utime->time x))
	     (clsql-sys:date (clsql-sys::date->time x))
	     (string (convert-to-clsql-datetime x))
	     (T x))))
    (setf x (cast x)
          y (cast y))
    (or (eql x y) ;; reference equality
        ;; wall time equality
        (and (typep x 'clsql:wall-time)
             (typep x 'clsql:wall-time)
             (clsql-sys:time= x y))
        ;; hey maybe they match?
        (equalp x y))))

(defvar *iso8601-timezone* nil)
(defvar *iso8601-microseconds* nil)
(defvar *iso8601-date-time-separator* " ")
(defvar *iso8601-time-separator* ":")
(defvar *iso8601-date-separator* "-")

(defgeneric iso8601-datestamp (d)
  (:documentation "Given an object that encodes a date
     return an iso8601-datestamp representation of it")
  (:method (d)
    (typecase d
      ((or clsql-sys:wall-time clsql-sys:date)
       (format nil "~4,'0D~A~2,'0D~A~2,'0D"
               (date-year d) *iso8601-date-separator* (date-month d)
               *iso8601-date-separator* (date-day d)))
      ((or string integer) (iso8601-datestamp (convert-to-clsql-datetime d)))
      (null nil))))

(defgeneric iso8601-timestamp (d)
  (:documentation
   "CLSQL has a function (I wrote) to do this, but I wanted more flexibility in output
   so that I could use this in more situations

   clsql:iso-timestamp is used only to write to database backends, so a very strict ISO
     is fine
   ")
  (:method (d)
    (typecase d
      ((or clsql-sys:wall-time clsql-sys:date string integer)
       (multiple-value-bind (usec second minute hour
                             day month year d-o-w is-utc?)
           (clsql-sys:decode-time (convert-to-clsql-datetime d))
         (declare (ignore d-o-w))
         ;; oh yeah, we love recursive format processing
         ;; http://www.lispworks.com/documentation/HyperSpec/Body/22_cgf.htm
         (apply
          #'format nil "~4,'0D~A~2,'0D~A~2,'0D~A~2,'0D~a~2,'0D~A~2,'0D~?~?"
          (nconc
           (list year *iso8601-date-separator* month
                 *iso8601-date-separator* day
                 *iso8601-date-time-separator*
                 hour *iso8601-time-separator*
                 minute *iso8601-time-separator*
                 second)
           (if *iso8601-microseconds*
               (list ".~6,'0D" (list usec))
               (list "" ()))
           ;;(adwutils:spy-break is-utc?  *iso8601-timezone*)
           (cond
             ((or is-utc?
                  (eql *iso8601-timezone* T))
              (list "~A" (list 'Z)))
             ((stringp *iso8601-timezone*) (list "~A" (list *iso8601-timezone*)))
             (T (list "" ())))))))
      (null nil))))

(defparameter +date-sep+ #?r"[\/\\.:T\+\-\s]+")

(defparameter +iso-date-match+
  #?r"(\d{2,4})${ +date-sep+ }(\d{1,2})${ +date-sep+ }(\d{1,2})")

(defparameter +iso-tz-match+
  #?r"(Z|,,0|[\+\-]\d{1,2}(?::\d{2})?)")

(defun isoish-offset-to-seconds (offset)
  (unless (and offset (plusp (length offset)))
    (return-from isoish-offset-to-seconds nil))
  (when (cl-ppcre:scan #?r"^[\s,0zZ\.]+$" offset)
    (return-from isoish-offset-to-seconds 0))
  (cl-ppcre:register-groups-bind
      (pos (#'parse-integer h m))
      (#?r"(\+|-)(\d{1,2})(?::(\d{2}))?" offset)
    (* (+ (* (or h 0) 60 )
          (or m 0))
       60
       (if (string= pos "+") -1 1))))

(defun %to-int (it)
  (typecase it
    (list (mapcar #'%to-int it))
    (null 0)
    (integer it)
    (string (ignore-errors (parse-integer it)))))

(defun %convert-offset (str &aux (c0 (char str 0)))
  (when (string-equal str ",,0")
    (return-from %convert-offset nil))
  (when (string-equal str "z")
    (return-from %convert-offset 0))
  (destructuring-bind (pos? hours &optional min)
      (list* (char= c0 #\+) (cl-ppcre:split ":" (subseq str 1)))
    (setf hours (ignore-errors (parse-integer hours))
          min (ignore-errors (parse-integer min)))
    (* (+ (* 60 60 (or hours 0))
          (* 60 (or min 0)))
       (if pos? -1 1))))

(defun %convert-string-split
    (val &aux year month day hour minute second usec offset)
  (setf val (string-trim (list* #\' #\" +common-white-space-trimbag+)
                         val))
  (destructuring-bind (&optional y mon d h m s &rest extras)
      (remove-if #'null
                 (cl-ppcre:split
                  #?r"[\/\\.:T\+\-\s'\",Z]+" val :omit-unmatched-p t))
    (declare (ignore extras))

    (unless (and y mon d)
      (return-from %convert-string-split nil))

    (setf year (%to-int y)
          month (%to-int mon)
          day (%to-int d)
          hour (or (%to-int h) 0)
          minute (or (%to-int m) 0)
          second (or (%to-int s) 0))

    (unless (and year month day)
      (return-from %convert-string-split nil))

    (when (and (<= year 31) (> day 31))
      (multiple-value-setq (month day year)
        (values year month day ))))

  (cl-ppcre:register-groups-bind
      ((#'parse-integer frac-sec))
      (#?"${ second }\.(\d+)" val)
    (setf usec (clsql-sys::%frac-string-to-usec frac-sec)))

  (when (cl-ppcre:scan #?r"(?i)z|[\+\-\,]+[0:]+(?:\s|$)" val)
    (setf offset 0))

  (cl-ppcre:register-groups-bind
   (offset-str)
   (#?r"((?:\s|:\d\d)[\+\-]\d{1,2}(?::\d{2})?)$" (trim-whitespace val))
   (setf offset (%convert-offset offset-str)))
  (let* ((am/pm? (cl-ppcre:scan-to-strings #?r"[ap]m.?" (string-downcase val)))
         (is-am? (when am/pm?
                   (char= (char am/pm? 0) #\a)))
         (is-pm? (when am/pm?
                   (char= (char am/pm? 0) #\p))))
    (when (and (eql hour 12) is-am?)
      (setf hour 0))
    (when (and (not (eql hour 12)) is-pm?)
      (incf hour 12)))

  (clsql:make-time
   :year year :month month :day day
   :hour hour :minute minute :second second
   :offset offset
   :usec usec))

(defgeneric convert-to-clsql-datetime (val)
  (:documentation
   "Converts a string timestamp into a clsql date time object

    Makes every possible effort to understand your date that will invariably
    be in some format it wont understand.")
  (:method (val)
    (typecase val
      (clsql:date (clsql-sys::date->time val))
      (clsql:wall-time val)
      (integer (clsql-sys::utime->time val))
      (string (%convert-string-split val))
    )))

(defmacro convert-to-clsql-datetime! (&rest places)
  `(setf ,@(iter (for p in places)
             (collect p)
             (collect `(convert-to-clsql-datetime ,p)))))

(defgeneric convert-to-clsql-date (val)
  (:documentation "Convert your value into a clsql:date structure")
  (:method (val)
    (typecase val
      (null nil)
      (clsql:date val)
      (clsql-sys::wall-time (clsql-sys::time->date val))
      (t (convert-to-clsql-date (convert-to-clsql-datetime val))))))

(defmacro convert-to-clsql-date! (&rest places)
  `(setf ,@(iter (for p in places)
             (collect p)
             (collect `(convert-to-clsql-date ,p)))))

(defun clsql-date/times->utime (obj &optional (timezone 0))
  "obj is either a wall-time or a date in local time. Converts to UTC and returns a utime.

  pass timezone nil to skip UTC conversion.

   if you are looking for the other it is clsql-sys:utime->time
  "
  (apply #'encode-universal-time
         (multiple-value-bind (usec second minute hour day month year)
             (clsql-sys:decode-time (convert-to-clsql-datetime obj))
           (declare (ignore usec))
           (list second minute hour day month year timezone))))

(defun last-of-the-month (start-date &aux (month (clsql-helper:date-month start-date)))
  "Returns the first of next month eg: 2/14/2012->2/29/2012"
  (iter
    (for date from-date start-date)
    (for yesterday previous date)
    (while (eql month (clsql-helper:date-month date)))
    (finally (return yesterday))))

(defun last-of-the-month? (start-date)
  "Returns T if its the last day of the month"
  (convert-to-clsql-date! start-date)
  (not (eql
        (clsql-helper:date-month start-date)
        (clsql-helper:date-month
         (clsql-sys:date+ start-date +a-day+)))))

(defun first-of-the-month (&optional (date (clsql-helper:current-sql-date)))
  "returns the first of the month for the month/year of the date passed in"
  (convert-to-clsql-date! date)
  (clsql-sys:make-date :year (date-year date) :month (date-month date) :day 1))

(defun first-of-the-month? (&optional (date (clsql-helper:current-sql-date)))
  "returns whether or not the date passed in is the first of the month"
  (convert-to-clsql-date! date)
  (= 1 (date-day date)))

(defun days-in-month (&optional (date (clsql-helper:current-sql-date)))
  "Return the number of days in the month of the date passed in"
  (date-day (last-of-the-month date)))

(defun day-before (&optional (date (clsql-helper:current-sql-date)))
  (convert-to-clsql-date! date)
  (when date (clsql:date- date +a-day+)))

(defun day-after (&optional (date (clsql-helper:current-sql-date)))
  (convert-to-clsql-date! date)
  (when date (clsql:date+ date +a-day+)))

(defun next-month (&optional (date (clsql-helper:current-sql-date))
                   &aux orig)
  (convert-to-clsql-date! date)
  (when date
    (setf orig (clsql-helper:date-month date)
          date (clsql-sys:date+ date +a-month+))
    ;; make sure we only go one month (1/31 + 1-month = 3/3)
    (iter (while (= (clsql-helper:date-month date)
                    (+ 2 orig)))
      (setf date (clsql-sys:date- date +a-day+)))
    date))

(defun last-month (&optional (date (clsql-helper:current-sql-date))
                   &aux orig)
  (convert-to-clsql-date! date)
  (when date
    (setf
     orig (clsql-helper:date-month date)
     date (clsql-sys:date- date +a-month+))
    ;; make sure we got into last month (3/31 - 1-month = 3/3)
    (iter (while (= orig (clsql-helper:date-month date)))
      (setf date (clsql-sys:date- date +a-day+)))
    date))

(defun next-year (&optional (date (clsql-helper:current-sql-date))
                   &aux orig)
  (convert-to-clsql-date! date)
  (when date
    (setf
     orig (clsql-helper:date-month date)
     date (clsql-sys:date- date +a-month+))
    ;; make sure we got into last month (3/31 - 1-month = 3/3)
    (iter (while (= orig (clsql-helper:date-month date)))
      (setf date (clsql-sys:date+ date +a-year+)))
    date))

(defun last-year (&optional (date (clsql-helper:current-sql-date))
                   &aux orig)
  (convert-to-clsql-date! date)
  (when date
    (setf
     orig (clsql-helper:date-month date)
     date (clsql-sys:date- date +a-month+))
    ;; make sure we got into last month (3/31 - 1-month = 3/3)
    (iter (while (= orig (clsql-helper:date-month date)))
      (setf date (clsql-sys:date- date +a-year+)))
    date))

(defun first-of-next-month (&optional (date (clsql-helper:current-sql-date)))
  (convert-to-clsql-date! date)
  (next-month (first-of-the-month date)))

(defun after-day-of-month (date day)
  "Are we past a specific day of the month"
  (> (date-day date) day))

(defun before-day-of-month (date day)
  "Are we past a specific day of the month"
  (< (date-day date) day))

(defun date-diff (d1 d2)
  "Gets the difference in days between two dates
    returns a negative number to indicate that d1 is after d2
    returns a positive number to indicate that d2 is after d1"
  (convert-to-clsql-date! d1 d2)
  ;; date-diff returns only days and seconds (for times)
  (let ((days (clsql-sys:duration-day (clsql-sys:date-difference d1 d2))))
    (if (clsql:date< d1 d2)
        days
        (- days))))

(defun date-add (d dur)
  (convert-to-clsql-date! d)
  (when d
    (clsql:date+ d (etypecase dur
                     (clsql:duration dur)
                     (integer (clsql:make-duration :day dur))))))

(defun dt< (&rest d)
  (apply #'clsql:time< (mapcar #'convert-to-clsql-datetime d)))

(defun dt<= (&rest d)
  (apply #'clsql:time<= (mapcar #'convert-to-clsql-datetime d)))

(defun dt> (&rest d)
  (apply #'clsql:time> (mapcar #'convert-to-clsql-datetime d)))

(defun dt>= (&rest d)
  (apply #'clsql:time>= (mapcar #'convert-to-clsql-datetime d)))
