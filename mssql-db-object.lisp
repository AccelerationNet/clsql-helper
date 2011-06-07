(in-package :adwutils)
(cl-interpol:enable-interpol-syntax)

(defvar *clsql-odbc-codebase-loaded* T)

(eval-when (:compile-toplevel :load-toplevel :execute)
    (export 'clsql::mssql-db-object :clsql)
    (export 'clsql::mssql-db-view :clsql))

(eval-always
  (defclass clsql:mssql-db-object (clsql-sys:standard-db-object)
    nil
    (:metaclass clsql-sys::standard-db-class)))

(defmethod print-object ((o CLSQL:MSSQL-DB-OBJECT) s)
  "Print the database object, and a couple of the most common identity slots."
  (print-unreadable-object (o s :type t :identity t)
    (awhen (ignore-errors (accessor-value-by-name o "id"))
      (format s "db-id:~a " it))
    (awhen (ignore-errors (accessor-value-by-name o "title"))
      (format s "Title:~s " it))
    (awhen (ignore-errors (accessor-value-by-name o "name"))
      (format s "Name:~s " it))
    ))

(defmethod clsql-sys::choose-database-for-instance ((object clsql::mssql-db-object) &optional database)
  (or database clsql-sys:*default-database*))

(defclass clsql:mssql-db-view (clsql-sys:standard-db-object)
  nil
  (:metaclass clsql-sys::standard-db-class))

(defmethod clsql::stored-slot-p
    ((slot-def clsql-sys::view-class-effective-slot-definition)
     &optional (include-key-slot t))
  (if include-key-slot
      (find (clsql-sys::view-class-slot-db-kind slot-def) '(:key :base))
      (eql :base (clsql-sys::view-class-slot-db-kind slot-def))))

(defmethod clsql::stored-slotdefs ((obj clsql::standard-db-object)
				      &optional (include-key-slot t))
  (clsql::stored-slotdefs (class-of obj) include-key-slot))

(defmethod clsql::stored-slotdefs ((class clsql-sys::standard-db-class)
				      &optional (include-key-slot t))
  (remove-if #'(lambda (sl)
		    (not (clsql::stored-slot-p sl include-key-slot)))
		(clsql-sys::ordered-class-slots class)))

(defmethod clsql-sys:update-records-from-instance
    ((obj clsql:mssql-db-object) &key clsql-sys:database &allow-other-keys)
  (let ((slots (remove-if
		#'(lambda (sd)
		    (or (not (slot-boundp obj (closer-mop:slot-definition-name sd)))
			(member :identity (ensure-list (clsql-sys::view-class-slot-db-constraints sd)))))
		(clsql::stored-slotdefs obj T))))
  (clsql:update-record-from-slots obj slots :database clsql-sys:database)))

(defmethod clsql-sys:update-record-from-slots :after
    ((obj clsql:mssql-db-object) slots &key clsql-sys:database)
  "After effecting the database record, if the key-slot is empty then use
SCOPE_IDENTITY to fill it. If > 1 key-slot, this won't do anything."
  (arnesi:when-bind
   key-slots
   (clsql-sys::key-slots (class-of obj))
   (when (= 1 (length key-slots))
     (let ((key-slot-name (sb-mop:slot-definition-name
                           (first key-slots))))
       (unless (and (slot-boundp obj key-slot-name)
                    (slot-value obj key-slot-name))
         (setf (slot-value obj key-slot-name)
               (let ((new-id (first
                              (clsql-sys:query
                               "SELECT SCOPE_IDENTITY()"
                               :flatp t
                               :database
                               (clsql-sys::choose-database-for-instance
                                obj clsql-sys:database)))))
                 (typecase new-id
                   (number new-id)
                   (string (parse-integer new-id :junk-allowed T))))))))))

(defmethod clsql-sys:update-record-from-slots
    ((o clsql:mssql-db-view) slots &key &allow-other-keys)
  "By default views shouldn't be updatable, so specialize a method to signal an error."
  (error "MSSQL view ~a is not updatable because it represents a view not a table."
	 (class-of o)))

(defun mssql-db-type-from-lisp-types (data-table)
  (iter
    (for i from 0)
    (for lisp-type in (column-types data-table))
    (collect
        (cond ((subtypep lisp-type 'float) "decimal (19,9)")
              ((subtypep lisp-type 'integer)
               (iter
                 (with thresh = (expt 2 15))
                 (for int in (data-table-value data-table :col-idx i))
                 (when int
                   (minimizing int into min)
                   (maximizing int into max))
                 (finally
                  (return
                    (if (<= (- thresh) (or min 0) (or max 0) thresh)
                        "int"
                        "bigint"
                        )))))
              ((subtypep lisp-type 'string)
               (let ((next-size
                       (next-highest-power-of-two
                        (iter (for s in (data-table-value data-table :col-idx i))
                          (maximizing (length s))))))
                 (cond
                   ((< 8000 next-size) "text")
                   ((< next-size 128) #?"varchar(128)")
                   (t #?"varchar(${next-size})"))))
              ((or (subtypep lisp-type 'clsql-sys:wall-time)
                   (subtypep lisp-type 'clsql-sys:date))
               "datetime")
              (T (error "Couldnt map type"))))))

