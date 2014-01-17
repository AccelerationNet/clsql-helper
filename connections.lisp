(in-package :clsql-helper)

(defclass connection-database ()
  ((names->spec :accessor names->spec :initarg :names->spec :initform nil
                :documentation "A mapping of names to connection specs")
   (names->conn :accessor names->conn :initarg :names->conn :initform nil
                :documentation "A mapping of names to active connections")))

(defmethod print-object ((db connection-database) stream)
  (print-unreadable-object (db stream)
    (format stream "CONNECTION-DATABASE ~a:specs ~a:conns"
            (ignore-errors (length (names->spec db)))
            (ignore-errors (length (names->conn db))))))

(defvar *connection-database* (make-instance 'connection-database)
  "A variable that when bound to a connection-database object will look up ")

(defun new-connection-database (&key (db *connection-database*)
                                name new-connection)
  (make-instance 'connection-database
                 :names->spec (names->spec db)
                 :names->conn (if new-connection
                                  (cons (cons name new-connection)
                                        (names->conn db))
                                  (names->conn db))))

(defun add-connection-spec (name spec &key (db *connection-database*))
  (setf (access:access (names->spec db) name :type :alist)
        spec))

(defun remove-connection-spec (name &key (db *connection-database*))
  (setf (names->spec db)
        (remove name (names->spec db) :key #'car)))

(defun get-connection-spec (name-or-spec &key (db *connection-database*))
  (etypecase name-or-spec
    (list name-or-spec)
    (t (access:access (names->spec db) name-or-spec :type :alist))))

(defun find-connection (name &key (db *connection-database*))
  (etypecase name
    ;; find by connection spec
    (list (iter (for (conn-name . conn) in (names->conn db))
            (when (same-database-connection? name conn)
              (return conn))))
    ;; find by name
    (symbol (access:access (names->conn db) name :type :alist))
    (clsql-sys:database name)))

(defun maybe-call (it)
  (etypecase it
    (null nil)
    (symbol (funcall it))
    (list (ecase (first it)
            ((quote function) (funcall (second it)))))
    (function (funcall it))))

(defun %call-perhaps-logged (fn log &optional (database clsql-sys:*default-database*))
  (alexandria:if-let ((log-fn (%log-fn-perhaps log)))
    (log-database-command-fn fn :log-fn log-fn :database database)
    (funcall fn)))

(defun coerce-connection-spec (c)
  (etypecase c
    (null nil)
    (symbol c)
    (clsql-sys:database (clsql-sys:connection-spec c))
    (list (cond ((listp (first c))
                 (first c))
                (t c)))))

(defun same-database-connection? (c1 c2)
  "determines whether or not two connections are the same
   by comparing their connection spec (eg '(server db user pass))
   handles connection-settings, connection-specs and clsql:database"
  (or
   (eql c1 c2)
   (equalp (coerce-connection-spec c1) (coerce-connection-spec c2))))

(defun with-database-function (fn connect-settings &key post-connect-fn log)
  "opens a database connection with the given settings, and runs the function.

connect-settings: a plist of connection info for clsql, also supports :post-connect-fn, a function to run after opening the connection

post-connect-fn: a function of no arguments to run after opening the connection

"
  (declare (type function fn) (dynamic-extent fn))
  (let ((name connect-settings)
        (spec (copy-list (get-connection-spec connect-settings))))
    (destructuring-bind (spec . settings)
        (if (listp (first spec))
            spec
            (list spec nil))
      (let ((settings-post-connect (getf settings :post-connect-fn)))
        (setf (getf settings :make-default) nil)
        (remf settings :post-connect-fn)
        ;; only ever disconnect the database we connect
        (let ((new-db (apply #'clsql-sys:connect spec settings)))
          (unwind-protect
               (let ((clsql-sys:*default-database* new-db)
                     (*connection-settings* connect-settings)
                     (*connection-database*
                       (new-connection-database
                        :name name
                        :new-connection new-db)))
                 ;; call post-connect if needed
                 (maybe-call post-connect-fn)
                 (maybe-call settings-post-connect)
                 (return-from with-database-function
                   (%call-perhaps-logged fn log)))
            (clsql-sys:disconnect :database new-db)))))))

(defun with-a-database-context (body-fn &key ((:connection-settings *connection-settings*)
                                              *connection-settings*)
                                        post-connect-fn log
                                        (db *connection-database*)
                                &aux existing-connection)
  "If a database connection exists and it matches the passed in settings or
   the passed in settings are null, use it!, otherwise aquire a new database
   connection"
  (declare (dynamic-extent body-fn))
  ;; handle logging here so that whether or not we get a new db connection
  ;; we get a logger if needed
  (flet ((logged-with-a-database-context-body () (%call-perhaps-logged body-fn log)))
    (declare (dynamic-extent #'logged-with-a-database-context-body))
    ;; if we've got an open connection with the same spec, reuse it
    (cond ((same-database-connection? *connection-settings* clsql-sys:*default-database*)
           (logged-with-a-database-context-body))

          ((setf existing-connection
                 (find-connection *connection-settings* :db db))
           (let ((clsql-sys:*default-database* existing-connection))
             (logged-with-a-database-context-body)))

          ((and (null *connection-settings*) clsql-sys:*default-database*)
           (logged-with-a-database-context-body))
          ((and (null *connection-settings*) (null clsql-sys:*default-database*))
           (error "No database connection available, please provide a clsql::*default-database*"))
          (t (with-database-function #'logged-with-a-database-context-body
               *connection-settings* :post-connect-fn post-connect-fn)))))


(defmacro with-database ((&optional (connection-settings *connection-settings*)
                          &key post-connect-fn log)
                         &body body)
  "opens a database connection and executes the body

connect-settings: a plist of connection info for clsql, also supports :post-connect-fn, a function to run after opening the connection

post-connect-fn: a function of no arguments to run after opening the connection "
  `(flet ((with-database-body () ,@body))
    (declare (dynamic-extent #'with-database-body))
    (with-database-function
      #'with-database-body
      ,connection-settings
      :post-connect-fn ,post-connect-fn
      :log ,log)))

(defmacro with-a-database ((&optional (connection-settings '*connection-settings*)
                            &key post-connect-fn log)
                           &body body)

  "If a database connection exists and it matches the passed in settings or
   the passed in settings are null, use it!, otherwise aquire a new database
   connection"
  `(flet ((with-a-database-body () ,@body))
    (declare (dynamic-extent #'with-a-database-body))
    (with-a-database-context
      #'with-a-database-body
      :connection-settings ,connection-settings
      :post-connect-fn ,post-connect-fn
      :log ,log)))


(defvar *thread-local-transaction-catch-tag* nil
  "Variable to hold the gensymed catch tag this thread's with-a-transaction is using.")

(defvar *inner-transaction-error* nil
  "Variable to hold conditions from internal errors, just makes for a bit cleaner code I think.")

(define-condition rollback ()
  ())

(define-condition commit ()
  ())

(defun with-transaction-context (body-fn database)
  "Establish a context for enlisting in transactions and run the body in a new transaction"
  ;(declare (type function body-fn) (dynamic-extent body-fn))
  (let ((*thread-local-transaction-catch-tag* (gensym "w/tran-ct-"))
        *inner-transaction-error*
        rtn)
    (catch *thread-local-transaction-catch-tag*
      (setf rtn
            (multiple-value-list
             (clsql::with-transaction (:database database) (funcall body-fn))))
      ;; we made it through the transaction without aborting to the catch tag
      (signal 'commit))
    (if *inner-transaction-error*
        (error *inner-transaction-error*)
        (apply #'values rtn))))

(defun with-a-transaction-context (body-fn database)
  "Either establish a new transaction context (with-transaction-context) or
   run the body in the extisting transaction context "

  ;(declare (type function body-fn) (dynamic-extent body-fn))
  ;; abort-database-transaction causes this to be false, even if its the same
  ;; connection we aborted the transaction on, thus the catch tag hoop jumping
  (if (clsql-sys::in-transaction-p :database database)
      ;; any error in a nested with-a-transaction, should abort the
      ;; entire transaction, we used to accomplish this by throwing up
      ;; the stack and swallowing the error which was a decidedly bad
      ;; approach
      (handler-bind
          ((error
             (lambda (c)
               (setf *inner-transaction-error* c) ;; convenience
               ;; in debugging is very much helpful to have the orignal
               ;; error context
               (if *debugger-hook*
                   (invoke-debugger c)
                   (throw *thread-local-transaction-catch-tag* c)
                   ))))
        (funcall body-fn))
      ;; dont have a transaction so lets create one
      (with-transaction-context body-fn database)))

(defmacro with-a-transaction ((&key (database 'clsql-sys:*default-database*)) &body body)
  "Wrapper around clsql:with-transaction, when a rollback is issued the code
   escapes (throw) to the outermost with-a-transaction.

   without the catch, its possible for an error handler in an intermediate function 
  "
  `(labels ((with-a-transaction-body-fn () ,@body))
    (declare (dynamic-extent #'with-a-transaction-body-fn))
    (with-a-transaction-context
      #'with-a-transaction-body-fn
      ,database)))

