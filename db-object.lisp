(in-package :clsql-helper)
(cl-interpol:enable-interpol-syntax)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defclass db-object (clsql-sys::standard-db-object)
    ()
    (:METACLASS CLSQL-SYS::STANDARD-DB-CLASS)))

(defmethod clsql-sys::choose-database-for-instance ((obj db-object) &optional database)
  "Always prefer the passed in database or the dynamic database eg: never use
   the view-database slot on the object because that causes thread safety
   issues"
  (or database clsql-sys:*default-database*))


(defgeneric next-identifier-sql (obj &key database)
  (:documentation "returns the SQL query to run to get the next
  identifier. Query should return 1 row with 1 column."))

(defmethod next-identifier (obj &key database)
  "fetch the next unique identifier for this obj/database,
  usually the value of a autoincrement field."
  (let ((new-id (car (clsql-sys:query
                      (next-identifier-sql obj :database database)
                      :flatp t
                      :database
                      (clsql-sys::choose-database-for-instance
                       obj database)))))
    (typecase new-id
      (number new-id)
      (string (parse-integer new-id :junk-allowed T)))))

(defmethod (setf closer-mop:slot-value-using-class) :after
    (new
     (class clsql-sys::standard-db-class)
     (object db-object)
     (slot closer-mop:standard-effective-slot-definition))
  (when (and (clsql-helper:join-slot? slot)
             (typep new (clsql-sys::join-slot-info-value slot :join-class)))
    (let ((hk (clsql-sys::join-slot-info-value slot :home-key))
          (fk (clsql-sys::join-slot-info-value slot :foreign-key)))
      (setf (access:access object hk)
            (access:access new fk)))))

(defun slot-db-stored? (slot-def &key (kinds '(:base :key)))
  (setf kinds (alexandria:ensure-list kinds))
  (and (typep slot-def 'clsql-sys::view-class-slot-definition-mixin)
       (member (clsql-sys::view-class-slot-db-kind slot-def) kinds)))

(defun clsql::stored-slotdefs (it &key (kinds '(:base :key)))
  "Returns a list of all slots that are directly stored in the database "
  (iter (for slot in (access:class-slot-definitions it))
    (when (slot-db-stored? slot :kinds kinds)
      (collect slot))))

(defun has-db-data-to-store? (it &key exclude)
  (setf exclude (alexandria:ensure-list exclude))
  (iter (for slot in (clsql::stored-slotdefs it))
    (for sn = (closer-mop:slot-definition-name slot))
    (unless (member sn exclude)
      (thereis (access:access it sn)))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defclass pg-db-object (db-object)
    ()
    (:METACLASS CLSQL-SYS::STANDARD-DB-CLASS)
    (:documenation
     "A class that knows how to be threadsafe and fill PK Identifiers in pg-databases")))

(defmethod next-identifier-sql ((obj pg-db-object) &key database)
  "pull the sequence value for this object's table"
  (declare (ignore database))
  (let ((tbl-name (clsql-sys::unescaped-database-identifier (class-of obj))))
    #?"SELECT currval('${tbl-name}_id_seq')"))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defclass mssql-db-object (db-object)
    ()
    (:metaclass clsql-sys::standard-db-class))
  (defclass mssql-db-view (db-object)
    ()
    (:metaclass clsql-sys::standard-db-class)))

(defmethod next-identifier-sql ((obj mssql-db-object) &key database)
  "use SCOPE_IDENTITY "
  (declare (ignore database))
  "SELECT SCOPE_IDENTITY()")

(defmethod clsql-sys:update-record-from-slots
    ((o mssql-db-view) slots &key &allow-other-keys)
  "By default views shouldn't be updatable, so specialize a method to signal an error."
  (error "MSSQL view ~a is not updatable because it represents a view not a table."
	 (class-of o)))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defclass sqlite3-db-object (db-object)
    ()
    (:metaclass clsql-sys::standard-db-class)))

(defmethod next-identifier-sql ((obj sqlite3-db-object)
                                         &key database)
  "fetch most recent AUTOINCREMENT on this connection.
 https://www.sqlite.org/lang_corefunc.html#last_insert_rowid"
  (declare (ignore database))
  "SELECT last_insert_rowid()")

