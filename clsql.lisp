(in-package :clsql-helper)
(cl-interpol:enable-interpol-syntax)
(clsql:file-enable-sql-reader-syntax)

(define-condition save-failed (error)
  ((message :initarg :message :accessor message)))

(defmacro with-command-logging ((&key (database 'clsql:*default-database*))
                                &body body)
  "record commands to the *command-log-stream* for the duration of body then
   restore its previous binding"
  (alexandria:with-unique-names (original-stream)
    `(let ((,original-stream (clsql-sys:command-recording-stream ,database)))
      (unwind-protect
           (progn
             (setf (clsql-sys:command-recording-stream ,database)
                   (make-broadcast-stream *command-log-stream*))
             ,@body)
        (setf (clsql-sys:command-recording-stream ,database)
              ,original-stream)))))

(defmacro with-command-logging-to-string ((&key string
                                           (database 'clsql:*default-database*))
                                          &body body)
  "Log the sql commands of the body to a string"
  `(with-output-to-string (*command-log-stream* ,string)
    (with-command-logging (:database ,database) ,@body)))

(defun default-log-fn (msg)
  (format *trace-output* "~%~A~%" msg))

(defun %log-fn-perhaps (log)
  (cond ((eql t log) *default-log-fn*)
        ((null log) nil)
        ((functionp log) log)
        ((macro-function log) (compile nil `(lambda (msg) (,log msg))))
        ((fboundp log) (lambda (msg) (funcall log msg)))))

(defun log-database-command-fn (body-fn &key log-fn (database clsql:*default-database*))
  (setf log-fn (%log-fn-perhaps log-fn))
  (cond
    (log-fn
     (let* ((record (make-array 60 :fill-pointer 0 :adjustable T :element-type 'base-char)))
       (with-command-logging-to-string (:string record :database database)
         (unwind-protect (funcall body-fn)
           (funcall log-fn (pretty-print-sql record))))))
    (t (funcall body-fn))))

(defmacro log-database-command ((&optional log-fn-name (database 'clsql:*default-database*))
                                &body body)
  "MUST BE Inside a database connection, creates a lexical scope in which all sql commands
   executed on this connection are logged to a specific logger

   tries to format such that it will be readable in the log

   log-fn-name is a function/macro name that will be called with a string/array as
     (log-fn-name stuff)
   "
  `(%call-perhaps-logged (lambda () ,@body)
    ,(if (constantp log-fn-name)
         `(load-time-value (%log-fn-perhaps ',log-fn-name))
         `(%log-fn-perhaps ',log-fn-name))
    ,database))

(defun clsql-exp (s)
  (clsql-sys:sql-expression :string s))

(defun clsql-column-name (column &optional table)
  (make-instance 'clsql-sys:sql-ident-attribute
                 :name column :qualifier table))

(defgeneric coerce-to-db-string-representation (s)
  (:documentation
   "Convert an object into an unquoted string that the database understands

   mostly used to do the coercion in db-string")
  (:method (s)
    (trim-and-nullify
     (typecase s
       (string s)
       (clsql-sys:date (print-nullable-date s))
       (clsql-sys:wall-time (print-nullable-datetime s))
       (t (princ-to-string s))))))

(defun db-string (s &key (prefix "")(postfix "")(wrapper "") )
  "trims, nullifies, escapes and wraps in single quotes so that the string is ready
   to be spliced into a query (eg: with cl-interpol). returns 'foo' or NIL."
  (let ((s (coerce-to-db-string-representation s)))
    (when s
      #?"'${prefix}${wrapper}${(clsql-sys:sql-escape-quotes s)}${wrapper}${postfix}'")))

(defun list-of-db-strings (list)
  "For use in creating `column IN (${stuff})` type of clauses"
  (when list
    (collectors:with-string-builder-output (out :delimiter ", ")
      (iter (for i in (alexandria:ensure-list list))
        (out (db-string i))))))

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

(defgeneric by-col (class column colvalue)
  (:documentation
   "fetchs the first row for the given class by id")
  (:method (class column colvalue)
    (setf column (typecase column
                   (symbol (make-instance 'clsql-sys:sql-ident
                                          :name (symbol-munger:lisp->underscores
                                                 column)))
                   (string (make-instance 'clsql-sys:sql-ident :name column ))
                   (t column)))
    (first (clsql:select class
             :where
             (if colvalue
                 [= column colvalue]
                 [null column])
             :flatp T))))

(defun primary-key-slots (obj)
  (clsql-sys::key-slots (access:class-of-object obj)))

(defun primary-key-slot (obj)
  (first (clsql-sys::key-slots (access:class-of-object obj))))

(defun primary-key-value (obj)
  (access:access obj (primary-key-slot obj)))

(defun primary-key-column-names (obj)
  (iter (for slot in (primary-key-slots obj))
    (collect
        (or (access:access slot 'clsql-sys::column)
            (closer-mop:slot-definition-name slot)))))

(defun primary-key-slot-names (obj)
  (mapcar #'c2mop:slot-definition-name
          (primary-key-slots obj)))

(define-condition db-object-has-no-keys (warning)
  ((obj :accessor obj :initarg :obj :initform nil))
  (:report (lambda (c s)
	     (format s "The object ~s has no primary-key-slots"
                     (obj c)))))
(defun warn-db-obj-has-no-keys (obj)
  (warn (make-instance 'db-object-has-no-keys :obj obj)))

(defun error-db-obj-has-no-keys (obj)
  (error (make-instance 'db-object-has-no-keys :obj obj)))

(defun primary-key-where-clauses ( obj  &aux (slots (primary-key-slots obj)))
  "Generates a where clause based on all of the primary keys of the object
    ex: pk1 = val1 and pk2 = val2 ...
  "
  (unless slots (warn-db-obj-has-no-keys obj))
  (iter (for slot in slots)
    (for key = (c2mop:slot-definition-name slot))
    (for col = (clsql-sys::database-identifier slot))
    (for kfn = (handler-case (fdefinition key)
                 (undefined-function ())))
    (for kfn-v = (and (compute-applicable-methods kfn (list obj))
                      (funcall kfn obj)))
    (for v = (or kfn-v (and (slot-boundp obj key) (slot-value obj key))))
    (if v
        (collecting [= col v] into exprs)
        (collecting [null col] into exprs))
    (collecting key into keys)
    (finally (return (values (clsql-ands exprs) keys)))))

(defgeneric new-object-p (obj)
  (:documentation
   "Checks that primary keys have values and that the object
   with those primary key values exists in the database")
  (:method (obj)
    (let* ((class (class-of obj))
           (keys (primary-key-slot-names obj)))
      (unless keys
        (warn-db-obj-has-no-keys obj)
        (return-from new-object-p nil))
      (not (and (every (lambda (k) (slot-boundp obj k)) keys)
                (every (lambda (k) (slot-value obj k)) keys)
                (clsql:select 1
                  :from (or (ignore-errors
                             (clsql-sys:view-table class))
                            (class-name class))
                  :flatp T
                  :limit 1
                  :where (primary-key-where-clauses obj)))))))

(defgeneric db-object-key-slots (o)
  (:documentation "returns the primarky key-slots of the given object")
  (:method (o)
    (typecase o
      (clsql-sys:standard-db-object
       (db-object-key-slots (class-of o)))
      (clsql-sys::standard-db-class
       (clsql-sys::key-slots o)))))

(defgeneric db-eql (x y &key test)
  (:documentation
   "Tries to determine if the objects are of the same type and have the same primary key values
      Many times objects which pass new-objectp are db-eql ,but once saved are no longer db-eql (due to using serial pkey)")
  (:method (x y &key (test #'equalp))
    (or (and (null x) (null y))
        (and
         (eql (class-of x) (class-of y))
         ;; make sure all the keys have the same values
         (let ((keys (primary-key-slot-names x)))
           (unless keys (error-db-obj-has-no-keys x))
           (iter (for key in keys)
             (for s1 = (slot-boundp x key))
             (for s2 = (slot-boundp y key))
             ;;true when either both slots are unbound or both slots have the same value
             (always (or (not (or s1 s2))
                         (and s1 s2
                              (funcall test (slot-value x key)
                                       (slot-value y key)))))))))))

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
                  #?"\n${ (spaces (if g1 1 3)) }${ (or g1 g2) } ")
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

;;;; Object Creation shortcuts
(defun make-instance-plist (columns row)
  "Creates a plist intended to be passed to make-instance"
  (iter (for c in columns)
    (for data in row)
    (collect (symbol-munger:underscores->keyword c))
    (collect data)))

(defun make-instances (class columns rows)
  "From N rows and some column-names make N instances of class filling data from rows
   using make instance"
  (iter (for row in rows)
      (for o = (apply #'make-instance class
                      (make-instance-plist columns row)))
      (collect o)))

(defun make-instances-setting-slot-values (class columns rows)
  "From N rows and column-name make N instances of class filling data from rows
   by creating instances and setting slot-values"
  (iter (for row in rows)
      (for o = (make-instance class))
      (iter (for c in columns)
        (for d in row)
        (setf (slot-value o c) d))
      (collect o)))

(defun make-instances-setting-accessors (class columns rows)
  "From N rows and column-name make N instances of class filling data from rows
   by creating instances and setting existing accessor functions"
  (iter (for row in rows)
      (for o = (make-instance class))
      (iter (for c in columns)
        (for d in row)
        (for setter = (fdefinition `(setf ,c)))
        (for fn = (compute-applicable-methods setter (list d o)))
        (when fn (funcall setter d o)))
      (collect o)))

(defun make-instances-setting-access (class columns rows)
  "From N rows and column-name make N instances of class filling data from rows
   by creating instances and setting through access lib"
  (iter (for row in rows)
      (for o = (make-instance class))
      (iter (for c in columns)
        (for d in row)
        (setf (access:access o c) d))
      (collect o)))

;;;; DB-SELECTION Shortcuts

(defun %command-if-needed (cmd params)
  (if params
      (clsql-sys:command-object cmd (alexandria:ensure-list params))
      cmd))

(defun db-exec (cmd &key params log)
  (with-a-database (*connection-settings* :log log)
    (clsql-sys:execute-command
     (%command-if-needed cmd params))))

(defun db-select (&rest select-args
                  &aux log)
  "Runs a clsql:select

   defaulting to :flatp T
   unnests any lists as part of the select list"
  (when (listp (first select-args))
    ;; flatten first element to prevent list-literal results from db
    (setf select-args (append (alexandria:flatten (first select-args))
                              (rest select-args))))
  (access:ensure-arg-list-key-value! T :flatp select-args)
  (multiple-value-bind (ps val)
      (access:rem-arg-list-key-value :log select-args)
    (setf log val
          select-args ps))

  (with-a-database (*connection-settings* :log log)
    (apply #'clsql-sys:select select-args)))

(defun db-query (cmd &rest keys &key params log &allow-other-keys)
  "runs a db query

   sets :flatp to t
   if params are provided we build a command object
      (backend better support this)"
  (access:rem-arg-list-key-value! :params keys)
  (access:rem-arg-list-key-value! :log keys)
  (access:ensure-arg-list-key-value! T :flatp keys)
  (with-a-database (*connection-settings* :log log)
    (apply #'clsql-sys:query
           (%command-if-needed cmd params)
           keys)))

(defun clsql-get-val (sql &key log)
  "alias of get-scalar"
  (db-scalar sql :log log))

(defun db-scalar (cmd &rest keys &key params log)
  "Query a single value from the database"
  (declare (ignore params log))
  (first (apply #'db-query cmd keys)))

(defun db-select-scalar (&rest select-args)
  "query a single row / value from the database using clsql:select

   alias with db-select-first which was deemed more aptly named when selecting a row"
  (apply #'db-select-first select-args))

(defun db-select-first (&rest select-args)
  "query a single row / value from the database using clsql:select

   alias with db-select-scalar"
  (access:ensure-arg-list-key-value! 1 :limit select-args)
  (first (apply #'db-select select-args)))

(defun db-query-plists (cmd &rest keys &key params log)
  "Returns a list of plists that correspond to the query results"
  (declare (ignore params log))
  (multiple-value-bind (rows cols)
      (apply #'db-query cmd keys)
    (unless cols (error "Cannot build plists if column names are unavailable"))
    (iter (for k in cols) (for v in (first rows))
      (collect (symbol-munger:underscores->keyword k))
      (collect v))))

(defun db-objs (class cmd &key params (make-instances-fn #'make-instances)
                          log
                          (column-munger #'symbol-munger:underscores->lisp-symbol))
  "retrieve objects of type class from the database using db-query

  "
  (let* ((rows-fields (multiple-value-list (db-query cmd :params params :log log)))
         (rows (first rows-fields))
         (fields (second rows-fields))
         ;; intern in the package expected
         (*package* (find-package (symbol-package class)))
         (fields (mapcar column-munger fields)))
    (funcall make-instances-fn class fields rows)))

(defun db-objs-select (class columns &key select-args (make-instances-fn #'make-instances))
  (let ((rows (apply #'db-select select-args)))
    (funcall make-instances-fn class columns rows)))

(defun join-slot? (slot-def)
  (and (or (typep slot-def 'clsql-sys::view-class-direct-slot-definition)
           (typep slot-def 'clsql-sys::view-class-effective-slot-definition))
       (eql :join (clsql-sys::view-class-slot-db-kind slot-def))))

(defmethod identity-slot? (slot-def)
  (and (or (typep slot-def 'clsql-sys::view-class-direct-slot-definition)
           (typep slot-def 'clsql-sys::view-class-effective-slot-definition))
       (member :identity (alexandria:ensure-list
                          (clsql-sys::view-class-slot-db-constraints slot-def)))))


;;;; DB - Manipulation shortcuts

(defun save! (db-obj &key log)
  "saves the given object, then returns the saved object"
  (with-a-database (*connection-settings* :log log)
    (clsql-sys:update-records-from-instance db-obj))
  db-obj)
