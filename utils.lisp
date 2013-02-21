(in-package :clsql-helper)
(cl-interpol:enable-interpol-syntax)

(defvar *connection-settings* ())

(defvar *clsql-codebase-loaded* T)

(defvar *command-log-stream* nil
  "a stream that we will record sql commands to in the body of ")

(defvar *default-log-fn* 'default-log-fn)

(defvar *record-this-dirty-slot* t
  "Should we record this slot as dirty?")

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

(defgeneric relaxed-parse-float (str &key type)
  (:documentation
   "trys to read a value we hope to be a floating point number returns nil on failure

   The goal is to allow reading strings with spaces commas and dollar signs in them correctly
  ")
  (:method (str &key (type 'double-float))
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
          (coerce (read-from-string str) type)))))))
;;;;
