(in-package :clsql-helper)

(defun maybe-call (it)
  (etypecase it
    (null nil)
    (symbol (funcall it))
    (list (ecase (first it)
            ((quote function) (funcall (second it)))))
    (function (funcall it))))

(defun %log-fn-perhaps (log)
  (cond ((eql t log) *default-log-fn*)
        ((null log) nil)
        ((functionp log) log)
        ((macro-function log) (compile nil `(lambda (msg) (,log msg))))
        ((fboundp log) (lambda (msg) (funcall log msg)))))

(defun %call-perhaps-logged (fn log &optional (database clsql-sys:*default-database*))
  (alexandria:if-let ((log-fn (%log-fn-perhaps log)))
    (log-database-command-fn fn :log-fn log-fn :database database)
    (funcall fn)))

(defun with-database-function (fn connect-settings &key post-connect-fn log)
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
      (let ((new-db (apply #'clsql-sys:connect spec settings)))
        (unwind-protect
             (let ((clsql-sys:*default-database* new-db)
                   (*connection-settings* connect-settings))
               ;; call post-connect if needed
               (maybe-call post-connect-fn)
               (maybe-call settings-post-connect)
               (return-from with-database-function
                 (%call-perhaps-logged fn log)))
          (clsql-sys:disconnect :database new-db))))))

(defmacro with-database ((&optional (connection-settings *connection-settings*)
                          &key post-connect-fn log)
                         &body body)
  "opens a database connection and executes the body

connect-settings: a plist of connection info for clsql, also supports :post-connect-fn, a function to run after opening the connection

post-connect-fn: a function of no arguments to run after opening the connection "
  `(with-database-function #'(lambda () ,@body) ,connection-settings
    :post-connect-fn ,post-connect-fn
    :log ,log))

(defvar *connection-settings* ())

(defun coerce-connection-spec (c)
  (etypecase c
    (null nil)
    (clsql-sys:database
     (clsql-sys:connection-spec c))
    (list
     (cond ((listp (first c)) (first c))
           (t c)))))

(defun same-database-connection? (c1 c2)
  "determines whether or not two connections are the same
   by comparing their connection spec (eg '(server db user pass))
   handles connection-settings, connection-specs and clsql:database"
  (equalp (coerce-connection-spec c1)
          (coerce-connection-spec c2)))

(defmacro with-a-database ((&optional (connection-settings *connection-settings*)
                            &key post-connect-fn log)
                           &body body)

  "If a database connection exists and it matches the passed in settings or
   the passed in settings are null, use it!, otherwise aquire a new database
   connection"
  (alexandria:with-unique-names (db-fn connection-settings-sym)
    ;; handle logging here so that whether or not we get a new db connection
    ;; we get a logger if needed
    `(flet ((,db-fn ()
             (%call-perhaps-logged (lambda () ,@body) ,log)))
      (let ((,connection-settings-sym ,connection-settings))
        ;; if we've got an open connection with the same spec, reuse it
        (cond ((same-database-connection?
                ,connection-settings-sym clsql-sys:*default-database*)
               (,db-fn))
              ((and (null ,connection-settings-sym)
                    clsql-sys:*default-database*)
               (,db-fn))
              ((and (null ,connection-settings-sym)
                    (null clsql-sys:*default-database*))
               (error "No database connection available, please provide a clsql::*default-database*"))
              (t (with-database-function
                     #',db-fn ,connection-settings-sym
                   :post-connect-fn ,post-connect-fn)))))))


