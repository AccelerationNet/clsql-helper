(in-package :clsql-helper)
(cl-interpol:enable-interpol-syntax)
(clsql:file-enable-sql-reader-syntax)

(define-condition save-failed (error)
  ((message :initarg :message :accessor message)))

(defun clsql-get-val (sql)
  (first (clsql:query sql :flatp T)))

(defun clsql-exp (s)
  (clsql-sys:sql-expression :string s))

(defun clsql-column-name (column &optional table)
  (make-instance 'clsql-sys:sql-ident-attribute
                 :name column :qualifier table))

(defun db-string (s &key (prefix "")(postfix "")(wrapper "") )
  "trims, nullifies, escapes and wraps in single quotes so that the string is ready
   to be spliced into a query (eg: with cl-interpol). returns 'foo' or NIL."
  (let ((it (trim-and-nullify (typecase s
                                (string s)
                                (t (princ-to-string s))))))
    (when it
      #?"'${prefix}${wrapper}${(clsql-sys:sql-escape-quotes it)}${wrapper}${postfix}'")))

(defun %clsql-subclauses (clauses)
  (iter (for c in clauses)
    (when c
      (collect
          (typecase c
            (string (clsql-exp c))
            (T c))))))

(defun clsql-ands (clauses)
  (let ((ex (%clsql-subclauses clauses)))
    (when ex
      (case (length ex)
        (1 (first ex))
        (t (apply #'clsql-sys:sql-and ex))))))

(defun clsql-and (&rest clauses)
  "returns a CLSQL:SQL-AND for all non-nil clauses, no nil if there are no non-nil clauses"
  (clsql-ands clauses))

(defun clsql-ors (clauses)
  "returns a CLSQL:SQL-AND for all non-nil clauses, no nil if there are no non-nil clauses"
  (let ((ex (%clsql-subclauses clauses)))
    (when ex
      (case (length ex)
        (1 (first ex))
        (t (apply #'clsql-sys:sql-or ex))))))

(defun clsql-or (&rest clauses)
  "returns a CLSQL:SQL-AND for all non-nil clauses, no nil if there are no non-nil clauses"
  (clsql-ors clauses))

(defun table-name-exp ( table-name )
  "based on what is passed in, tries to figure out what the table name is"
  (labels
      ((rec (table &key from-class from-symbol &aux cls)
         (typecase table
           (symbol
            (cond
              ((and (not from-class) (setf cls (ignore-errors (find-class table))))
               (rec cls))
              (T (rec (symbol-munger:lisp->underscores table :capitalize nil)
                      :from-symbol T))))
           (clsql-sys::standard-db-class
            (rec (or (clsql-sys::view-table table) (class-name table))
                 :from-class T))
           (clsql-sys::standard-db-object (rec (class-of table)))
           (string (if from-symbol
                       (make-instance 'clsql-sys:sql-ident :name table)
                       (make-instance 'clsql-sys:sql-ident-table :name table))))
         ))
    (rec table-name)
    ))

(defun table-name-string ( table-name )
  (clsql-sys::escaped-database-identifier
   table-name clsql-sys:*default-database* T))

(defun column-name-string (column)
  (clsql-sys::escaped-database-identifier
   column clsql-sys:*default-database*))

(defgeneric by-id (class id &optional colname)
  (:documentation "Fetchs the first row for the given class by id")
  (:method (class id &optional (colname [id]))
    "direct implementation of by-id, (select class). fetchs the first row for the given class by id"
    (by-col class colname id)))

(defmethod by-col (class column colvalue)
  "fetchs the first row for the given class by id"
  (setf column (typecase column
                  (symbol (make-instance 'clsql-sys:sql-ident
                                         :name (symbol-munger:lisp->underscores
                                                column)))
                  (string (make-instance 'clsql-sys:sql-ident :name column ))
                  (t column)))
  (first (clsql:select class
	   :where [= column colvalue]
	   :flatp T)))

(defun primary-key-slots (obj)
  (clsql-sys::key-slots
   (typecase obj
     (symbol (find-class obj))
     (clsql-sys::standard-db-class obj)
     (clsql-sys:standard-db-object (class-of obj)))))

(defun primary-key-slot-names (obj)
  (mapcar #'c2mop:slot-definition-name
          (primary-key-slots obj)))

(defun primary-key-where-clauses ( obj )
  "Generates a where clause based on all of the primary keys of the object
    ex: pk1 = val1 and pk2 = val2 ...
  "
  (iter (for slot in (primary-key-slots obj))
    (for key = (c2mop:slot-definition-name slot))
    (for col = (clsql-sys::database-identifier slot))
    (for kfn = (handler-case (fdefinition key)
                 (undefined-function ())))
    (for kfn-v = (and (compute-applicable-methods kfn (list obj))
                      (funcall kfn obj)))
    (for v = (or kfn-v (slot-value obj key)))
    (collecting [= col v] into exprs)
    (collecting key into keys)
    (finally (return (values (clsql-ands exprs) keys)))))

(defmethod new-object-p (obj)
  "Checks that primary keys have values and that the object
   with those primary key values exists in the database"
  (let* ((class (class-of obj))
	 (keys (mapcar #'c2mop:slot-definition-name
		       (clsql-sys::key-slots class))))
    (not (and (every (lambda (k) (slot-boundp obj k)) keys)
	      (every (lambda (k) (slot-value obj k)) keys)
	      (clsql:select (class-name class)
		:flatp T
		:where (primary-key-where-clauses obj))))))

(defmethod db-object-key-slots ((c clsql-sys::standard-db-class))
  (clsql-sys::key-slots c))

(defmethod db-object-key-slots ((o clsql-sys:standard-db-object))
  (db-object-key-slots (class-of o)))

(defmethod db-eql (x y &key (test #'equalp))
  "Tries to determine if the objects are of the same type and have the same primary key values
      Many times objects which pass new-objectp are db-eql ,but once saved are no longer db-eql (due to using serial pkey)"
  (or (and (null x) (null y))
      (and
       (eql (class-of x) (class-of y))
       ;; make sure all the keys have the same values
       (let ((keys (db-obj-key-slots x)))
         (unless keys
           (error "DB-EQL requires that there be at least one key, otherwise all database objects are eql.
Keyless object: ~A - ~A
You can define db-obj-key-slots for the object to assign keys for the purpose of db-eql"
                  x (class-of x)))
         (iter (for key-def in keys)
           (for key = (c2mop:slot-definition-name key-def))
           (for s1 = (slot-boundp x key))
           (for s2 = (slot-boundp y key))
           ;;true when either both slots are unbound or both slots have the same value
           (always (or (not (or s1 s2))
                       (and s1 s2
                            (funcall test (slot-value x key)
                                     (slot-value y key))))))))))

(defun pretty-print-sql (sql-command)
  (when (and sql-command (stringp sql-command))
    (let ((paren-cnt 0)
          wrote-a-newline
          ;; A scanner for top level keywords, secondary keywords
          ;; and sub expressions (as by parens)
          (scanner (load-time-value
                    (cl-ppcre:create-scanner
                     #?r"(?:\s+(?:(select|insert|update|delete|exec)|(from|where|order|group|left join|full join|outer join|join|values|on|limit))\s+)|(\()|(\))|(\n)"
                     :case-insensitive-mode T))))
      (flet ((spaces (&optional (extra-n 1))
               "Creates a string with the correct indent in it"
               (make-string (+ extra-n (* 2 paren-cnt))
                            :initial-element #\space )))
        (cl-ppcre:regex-replace-all
         scanner sql-command
         (lambda (m g1 g2 g3 g4 g5)
           (declare (ignore m))
           (let ((wnl wrote-a-newline))
             (setf wrote-a-newline nil)
             (cond
               ((or g1 g2) ;; we got a sql keyword, or subkeyword
                ;; if we already wrote a new line, we dont need another
                (let ((newline (if wnl "" #\newline)))
                  #?"${newline}${ (spaces (if g1 1 3)) }${ (or g1 g2) } "))
               (g3 (incf paren-cnt) g3)  ;; got an open paren
               (g4 ;; close paren
                (when (> paren-cnt 0) (decf paren-cnt))
                ;; if this paren is alone on a line indent it
                #?"${ (if wnl (spaces) "") }${g4}" )
               ;; we wrote a newline so we may not need to write another
               (g5 (setf wrote-a-newline T) g5)
               (T
                "" ))))
         :simple-calls T)))))

(defmacro log-database-command ((log-fn-name &optional (database 'clsql:*default-database*)) &body body)
  "MUST BE Inside a database connection, creates a lexical scope in which all sql commands
   executed on this connection are logged to a specific logger

   tries to format such that it will be readable in the log

   log-fn-name is a function/macro name that will be called with a string/array as
     (log-fn-name stuff)
   "
  (alexandria:with-unique-names (str record results)
    `(let* (,results
	    (,record (make-array 60 :fill-pointer 0 :adjustable T :element-type 'base-char)))
       (with-output-to-string (,str ,record)
	 (setf (clsql-sys:command-recording-stream ,database)
	       (make-broadcast-stream ,str)
	       ,results
	       (unwind-protect
		    (progn ,@body)
		 (setf (clsql-sys:command-recording-stream ,database) nil)
		 (setf ,record (pretty-print-sql ,record))
		 (,log-fn-name ,record))))
       ,results)))

(defun db-type-from-lisp-type (type &key (length 64) (scale 4)
                                    (pg-default-int-type "int8"))
  (declare (ignore scale)) ;; maybe later?
  (let ((backend (and clsql-sys:*default-database*
                      (clsql-sys::database-underlying-type clsql-sys:*default-database*))))
    (cond
      ((or (subtypep type 'ratio)
           (subtypep type 'float))
       (case backend
         (:postgresql "double precision")
         (:mssql "decimal")
         (T "double")))
      ((or (subtypep type 'fixnum) (subtypep type 'integer))
       (case backend
         (:postgresql pg-default-int-type)
         (T "int")))
      ((subtypep type 'string)
       (case backend
         (:postgresql "text")
         (T #?"varchar (${ length })")))
      ((subtypep type 'clsql-sys:wall-time)
       (ecase backend
         (:postgresql "timestamp without time zone")
         (:mssql "datetime")))
      ((subtypep type 'clsql-sys:date)
       (ecase backend
         (:postgresql "date")
         (:mssql "smalldatetime")))
      )))

(defun coerce-value-to-db-type (val db-type)
  (cond
    ((subtypep db-type 'clsql-sys:varchar)
     (trim-and-nullify (princ-to-string val)))
    ((subtypep db-type 'integer)
     (etypecase val
       (string (parse-integer val))
       (integer val)))
    ((subtypep db-type 'double-float)
     (etypecase val
       (string (relaxed-parse-float val))
       (number val)))
    ((subtypep db-type 'number)
     (etypecase val
       (string (relaxed-parse-float val))
       (number val)))
    ((subtypep db-type 'clsql:date) (convert-to-clsql-date val))
    ((subtypep db-type 'clsql:wall-time ) (convert-to-clsql-datetime val))
    ((subtypep db-type 'boolean)
     (typecase val
       (string (not (null (member val (list "T" "true" "1" "y" "yes") :test #'string-equal))))
       (T val)))
    ((subtypep db-type 'clsql-sys:duration )
     (error "NO COERCION IMPLEMENTED"))
    (T (error "NO COERCION IMPLEMENTED"))))

(defun format-value-for-database (d &optional stream)
  "prints a correctly sql escaped value for postgres"
  (etypecase d
    (null (format stream "null"))
    (string
     (let ((r (db-string d)))
       (if r (format stream "~A" r)
           (format stream "null"))))
    (integer (format stream "~D" d))
    (float (format stream "~F" d))
    (clsql-sys:date (format stream "'~a'" (iso8601-datestamp d)))
    (clsql-sys:wall-time (format stream "'~a'" (clsql-sys:iso-timestring d)))))