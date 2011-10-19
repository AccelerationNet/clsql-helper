(in-package :clsql-helper)

(defun maybe-call (it)
  (etypecase it
    (null nil)
    (symbol (funcall it))
    (list (ecase (first it)
            ((quote function) (funcall (second it)))))
    (function (funcall it))))

(defun with-database-function (fn connect-settings &optional post-connect-fn)
  "opens a database connection with the given settings, and runs the function.

connect-settings: a plist of connection info for clsql, also supports :post-connect-fn, a function to run after opening the connection

post-connect-fn: a function of no arguments to run after opening the connection 

"
  (declare (type function fn))
  (destructuring-bind (spec . settings) (copy-list connect-settings)
    (let ((settings-post-connect (getf settings :post-connect-fn)))
      (setf (getf settings :make-default) nil)
      (remf settings :post-connect-fn)
      ;; only ever disconnect the database we connect
      (let ((clsql-sys:*default-database*
              (apply #'clsql-sys:connect spec settings)))
        (unwind-protect
             ;; prevent people from changing what database we will disconnect
             ;; if they setf
             (let ((clsql-sys:*default-database* clsql-sys:*default-database*))
               ;; call post-connect if needed
               (maybe-call post-connect-fn)
               (maybe-call settings-post-connect)
               (return-from with-database-function (funcall fn)))
          (clsql-sys:disconnect :database clsql-sys:*default-database*))))))

(defmacro with-database ((connection-settings &optional post-connect-fn) &body body)
  "opens a database connection and executes the body

connect-settings: a plist of connection info for clsql, also supports :post-connect-fn, a function to run after opening the connection

post-connect-fn: a function of no arguments to run after opening the connection "
  `(with-database-function #'(lambda () ,@body) ,connection-settings ,post-connect-fn))

(defmacro with-a-database ((connection-settings &optional post-connect-fn) &body body)
  "If a database connection exists, use it!, otherwise aquire a new database connection"
  (alexandria:with-unique-names (db-fn)
    `(flet ((,db-fn () ,@body))
       (if clsql-sys:*default-database*
	   (,db-fn)
	   (with-database-function
	       #',db-fn ,connection-settings ,post-connect-fn)))))
