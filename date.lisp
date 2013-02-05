
(in-package :clsql-helper)
(cl-interpol:enable-interpol-syntax)
(clsql:file-enable-sql-reader-syntax)

(defvar *clsql-codebase-loaded* T)

;;;; UTILS
(defparameter +common-white-space-trimbag+
  '(#\space #\newline #\return #\tab #\no-break_space))

(defun trim-whitespace (s)
  (string-trim +common-white-space-trimbag+ s))

(defun trim-and-nullify (s)
  "trims the whitespace from a string returning nil
   if trimming produces an empty string or the string 'nil' "
  (when s
    (let ((s (trim-whitespace s)))
      (cond ((zerop (length s)) nil)
	    ((string-equal s "nil") nil)
            ((string-equal s "null") nil)
	    (T s)))))

(defmethod relaxed-parse-float (str &key (type 'double-float))
  "trys to read a value we hope to be a floating point number returns nil on failure

   The goal is to allow reading strings with spaces commas and dollar signs in them correctly 
  "
  (etypecase str
    (null nil)
    (float str)
    (number (float str (ecase type
                         (single-float 0.0)
                         ((float double-float) 0.0d0))))
    ((or string symbol)
     (let* ((str (cl-ppcre:regex-replace-all #?r"\s|\$|\,|\%" (string str) ""))
            (*read-eval* nil)
            (*read-default-float-format* type))
       (ignore-errors
        (coerce (read-from-string str) type))))))
;;;;

(defun current-sql-date ()
  "current date"
  (clsql-sys:get-date))

(defun current-sql-time ()
  "current date and time"
  (clsql-sys:get-time))

(defun print-nullable-date (field)
  "if the date exists, prints m?m/d?d/yyyy"
  (when field
    (typecase field
      (string field)
      (T (clsql:print-date
	  (typecase field
	    (clsql-sys:date (clsql-sys::date->time field))
	    (clsql-sys:wall-time field))
	  :day)))))

(defmethod print-object ((o clsql-sys:date) stream)
  (let ((date (print-nullable-date o)))
    (if *print-escape*
	(print-unreadable-object (o stream :type T :identity T)
	  (format stream "~A" date))
	(format stream "~A" date))))

(defmethod date-day (d)
  (etypecase d
    (clsql-sys:date
       (third (multiple-value-list (clsql-sys:date-ymd d))))
    (clsql-sys:wall-time
       (third (multiple-value-list (clsql-sys:time-ymd d))))
    ((or string integer)
     (date-day (convert-to-clsql-datetime d)))
    (null nil)
    ))

(defmethod date-year (d )
  (etypecase d
    (clsql-sys:date (clsql-sys:date-ymd d))
    (clsql-sys:wall-time (clsql-sys:time-ymd d))
    ((or string integer)
     (date-year (convert-to-clsql-datetime d)))
    (null nil)))

(defmethod date-month (d)
  (etypecase d
    (clsql-sys:date
     (second (multiple-value-list (clsql-sys:date-ymd d))))
    (clsql-sys:wall-time
     (second (multiple-value-list (clsql-sys:time-ymd d))))
    ((or string integer)
     (date-month (convert-to-clsql-datetime d)))
    (null nil)))

(defun month-string  (d)
  "Converts the date to the full name, January, February,etc"
  (let ((d (date-month d)))
    (when d (clsql-sys:month-name d))))

(defun month-day-string  (d)
  "prints dates as January 3"
  (let ((d (date-day d))
        (m (month-string d)))
    (when (and d m) #?"${m} ${d}")))

(defun print-nullable-datetime (field)
  "if the date exists, prints mm/dd/yyyy hh:mm:ss"
  (let ((*print-pretty* nil))
    (when field
      (typecase field
	(string field)
	(T (multiple-value-bind (usec second minute hour day month year)
	       (clsql-sys:decode-time (convert-to-clsql-datetime field))
	     (declare (ignore usec))
	     (format nil "~2,'0d/~2,'0d/~4,'0d ~2,'0d:~2,'0d:~2,'0d"
                     month day year hour minute second)))))))

(defmethod print-object ((o clsql:wall-time) stream)
  (let ((date (print-nullable-datetime o)))
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
    (equalp (cast x) (cast y))))

(defvar *iso8601-timezone* nil)
(defvar *iso8601-microseconds* nil)
(defvar *iso8601-date-time-separator* " ")
(defvar *iso8601-time-separator* ":")
(defvar *iso8601-date-separator* "-")

(defmethod iso8601-datestamp (d)
  (typecase d
    ((or clsql-sys:wall-time clsql-sys:date)
     (format nil "~4,'0D~A~2,'0D~A~2,'0D"
             (date-year d) *iso8601-date-separator* (date-month d)
             *iso8601-date-separator* (date-day d)))
    ((or string integer) (iso8601-datestamp (convert-to-clsql-datetime d)))
    (null nil)))

(defmethod iso8601-timestamp (d)
  "CLSQL has a function (I wrote) to do this, but I wanted more flexibility in output
   so that I could use this in more situations

   clsql:iso-timestamp is used only to write to database backends, so a very strict ISO
     is fine
   "
  (typecase d
    ((or clsql-sys:wall-time clsql-sys:date string integer)
     (multiple-value-bind (usec second minute hour day month year)
         (clsql-sys:decode-time (convert-to-clsql-datetime d))
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
         (cond
           ((eql *iso8601-timezone* T) (list "~A" (list 'Z)))
           ((stringp *iso8601-timezone*) (list "~A" (list *iso8601-timezone*)))
           (T (list "" ())))))))
    (null nil)))

(defparameter +date-sep+ "(?:/|-|\\.|:)")

(defparameter +date-time-regex+
  (cl-ppcre:create-scanner
   #?r"^(?:'|\")?(\d{1,2})${ +date-sep+ }(\d{1,2})${ +date-sep+ }(\d{2,4})(?:\s*(\d{1,2})${ +date-sep+ }(\d{1,2})(?:${ +date-sep+ }(\d{1,2}))?\s*((?:a|p)m\.?)?)?(?:'|\")?"
   :case-insensitive-mode t))

(defparameter +iso-8601-ish-regex-string+
  #?r"^(?:'|\")?(\d{2,4})${ +date-sep+ }(\d{1,2})${ +date-sep+ }(\d{1,2})(?:(?:\s*|T)(\d{1,2})${ +date-sep+ }(\d{1,2})(?:${ +date-sep+ }(\d{1,2}))?\s*((?:a|p)m\.?)?(?:Z|,,0|(?:-|\+)\d{1,2}:?\d{2}?)?)?(?:'|\")?")

(defparameter +iso-8601-ish-regex+
  (cl-ppcre:create-scanner +iso-8601-ish-regex-string+ :case-insensitive-mode t))

(defmethod convert-to-clsql-datetime (val )
  "Converts a string timestamp into a clsql date time object
   Makes every possible effort to understand your date that will invariably be in some format it wont understand."
  (macrolet ((regex-date-to-clsql-date ()
	       "Pretty fugly variable capture, but what are you gonna do.
                I have the exact same code twice with like 6 vars to pass"
	       `(let ((hour (if (and h (< h 12)
				     (string-equal am/pm "PM"))
				(+ 12 h)
				h))
		      (year (and y
				 (cond
				   ((< y 50) (+ y 2000))
				   ((< y 100) (+ y 1900))
				   (T y)))))
		  (clsql:make-time :year year :month mon :day d
				   :hour (or hour 0) :minute (or m 0) :second (or s 0)))))
    (typecase val
      (clsql:date (clsql-sys::date->time val))
      (clsql:wall-time val)
      (integer (clsql-sys::utime->time val))
      (string
	 (or ; as best I can tell these just suck
             ;(ignore-errors (clsql-sys:parse-date-time val))
	     ;(ignore-errors (clsql-sys:parse-timestring val))
	     (cl-ppcre:register-groups-bind ((#'parse-integer mon d y h m s) am/pm)
		 (+date-time-regex+ val)
	       (regex-date-to-clsql-date))
	     (cl-ppcre:register-groups-bind ((#'parse-integer y mon d h m s) am/pm)
		 (+iso-8601-ish-regex+ val)
	       (regex-date-to-clsql-date)
	       ))))))

(defmacro convert-to-clsql-datetime! (&rest places)
  `(setf ,@(iter (for p in places)
             (collect p)
             (collect `(convert-to-clsql-datetime ,p)))))

(defmethod convert-to-clsql-date (val)
  (typecase val
    (null nil)
    (clsql:date val)
    (clsql-sys::wall-time (clsql-sys::time->date val))
    (t (convert-to-clsql-date (convert-to-clsql-datetime val)))))

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

(defparameter +a-negative-second+ (clsql-sys:make-duration :second -1))
(defparameter +a-negative-minute+ (clsql-sys:make-duration :minute -1))
(defparameter +a-negative-hour+ (clsql-sys:make-duration :hour -1))
(defparameter +a-negative-day+ (clsql-sys:make-duration :day -1))
(defparameter +a-negative-month+ (clsql-sys:make-duration :month -1))
(defparameter +a-negative-year+ (clsql-sys:make-duration :year -1))

(defparameter +a-second+ (clsql-sys:make-duration :second 1))
(defparameter +a-minute+ (clsql-sys:make-duration :minute 1))
(defparameter +an-hour+ (clsql-sys:make-duration :hour 1))
(defparameter +a-day+ (clsql-sys:make-duration :day 1))
(defparameter +a-month+ (clsql-sys:make-duration :month 1))
(defparameter +a-year+ (clsql-sys:make-duration :year 1))

(defun last-of-the-month (start-date &aux (month (clsql-helper:date-month start-date)))
  "Returns the first of next month eg: 2/14/2012->2/29/2012"
  (iter
    (for date from-date start-date)
    (for yesterday previous date)
    (while (eql month (clsql-helper:date-month date)))
    (finally (return yesterday))))

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

(defun next-month (&optional (date (clsql-helper:current-sql-date)))
  (convert-to-clsql-date! date)
  (when date (clsql-sys:date+ date +a-month+)))

(defun last-month (&optional (date (clsql-helper:current-sql-date)))
  (convert-to-clsql-date! date)
  (when date (clsql-sys:date- date +a-month+)))

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
