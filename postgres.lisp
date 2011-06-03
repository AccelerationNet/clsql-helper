(in-package :clsql-helper)
(cl-interpol:enable-interpol-syntax)

(defvar *clsql-pg-codebase-loaded* T)

(eval-always
  (defclass pg-db-obj ()
    ()
    (:METACLASS CLSQL-SYS::STANDARD-DB-CLASS)))

(defmethod set-through-accessor ((obj pg-db-obj) name value)
  (call-next-method)
  (let ((slot-def (class-slot-by-name (class-of obj) name)))
    (when-bind db-info (clsql-sys::view-class-slot-db-info slot-def)
      (let ((hk (gethash :home-key db-info))
	    (fk (gethash :foreign-key db-info))
	    (cls (gethash :join-class db-info)))
	(when (typep value cls)
	  (ignore-errors ;; just make an attempt to set the backing slot
	    (set-through-accessor obj hk (accessor-value-by-name value fk))))))))

(defmethod clsql-sys::choose-database-for-instance ((object pg-db-obj) &optional database)
  (or database clsql-sys:*default-database*))

(defmethod clsql:update-record-from-slots :after ((obj pg-db-obj) slots &key clsql:database)
  "After effecting the database record, if the key-slot is empty then use
curval(sequence) to fill it. If > 1 key-slot, this won't do anything."
  (arnesi:when-bind key-slots
      (clsql-sys::key-slots (class-of obj))
    (if (= 1 (length key-slots))
	(let ((key-slot-name (sb-mop:slot-definition-name
			      (first key-slots))))
	  (unless (and (slot-boundp obj key-slot-name)
		       (slot-value obj key-slot-name))
	    (setf (slot-value obj key-slot-name)
		  (car (clsql-sys:query
			#?"SELECT currval('${(clsql:view-table (class-of obj))}_id_seq')"
			:flatp t
			:database
			(clsql-sys::choose-database-for-instance
			 obj
			 clsql-sys:database)))))))))

(defmethod print-object ((o pg-db-obj) (s stream))
  "Print the database object, and a couple of the most common identity slots."
  (print-unreadable-object (o s :type t :identity t)
    (awhen (ignore-errors (accessor-value-by-name o "id"))
      (format s "db-id:~a " it))
    (awhen (ignore-errors (accessor-value-by-name o "title"))
      (format s "Title:~a " it))
    (awhen (ignore-errors (accessor-value-by-name o "name"))
      (format s "Name:~a " it))
    ))

(defun format-value-for-postgres (d &optional stream)
  "prints a correctly sql escaped value for postgres"
  (format-value-for-database d stream))
(export 'format-value-for-postgres)


(defun copy-table ( table-from table-to )
  "Makes a copy of a table to a new table in the same database.
     NB:  Not a very quick way to copy tables"
  (setf table-from (clsql-sys:sql (table-name-exp table-from))
	table-to (clsql-sys:sql (table-name-exp table-to)))
  (clsql-sys:execute-command #?"CREATE TABLE ${table-to} (like ${table-from} INCLUDING DEFAULTS INCLUDING CONSTRAINTS INCLUDING INDEXES);")
  (clsql-sys:execute-command #?"INSERT INTO ${table-to} (SELECT * FROM ${table-from});"))
(export 'copy-table)

(defun postgres-db-type-from-lisp-type (lisp-type)
  "Given a lisp data-type make up a postgres type to match"
  (cond ((subtypep lisp-type 'float) "double precision")
        ((subtypep lisp-type 'integer) "int8")
        ((subtypep lisp-type 'string) "text")
        ((or (subtypep lisp-type 'clsql-sys:wall-time)
             (subtypep lisp-type 'clsql-sys:date))
         "timestamp with time zone")
        (T (error "Couldnt map type"))))
