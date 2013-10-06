(in-package #:clsql-helper)
(clsql-sys:file-enable-sql-reader-syntax)

;; # Migrations
;;
;; This system allows for running queries once and only once, intended to
;; support schema changes and data fixes.
;;
;; The migrations system automatically creates a table in the database to
;; track what queries have been run. Migrations are meant to be run early in
;; the build or start-up process (eg: before any ORM activity).
;;
;; Unlike Ruby migrations, there is no name for each query, and there is no
;; concept of undoing the migration. This system is fast-forward only.

(defvar *migration-table-name* "clsql_helper_migrations"
  "the table name to use for migrations")

(defun ensure-migration-table ()
  "if it doesn't exist, create the migration table"
  (unless (clsql-sys:table-exists-p *migration-table-name*)
    (clsql-sys:create-table
     *migration-table-name*
     ;; hash of our query - not unique because we may wish to rerun
     ;; migrations occasionally and it doesnt hurt to have multiple hashes
     ;; partly migrations give a record of who did what when
     `(([hash] string :not-null)
       ;; the actual query
       ([query] string
		;; specify the underlying db type, supports SQL Server 2005+ and Postgresql
		,(ecase (clsql-sys:database-underlying-type clsql-sys:*default-database*)
		   (:mssql "VARCHAR(MAX)")
		   (:postgresql "text"))
		:not-null)
       ;; when we migrated it
       ([date-entered] clsql-sys:wall-time :not-null)))))

(defun %migration-done-p (hash)
  "returns non-nil if this hashed migration has been run"
  (with-a-database ()
    (clsql:select [date-entered] :from *migration-table-name*
                  :where [= [hash] hash]
                  :flatp T)))

(defun md5-string (string)
  (let* ((chars #(#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9 #\a #\b #\c #\d #\e #\f))
         (vec (md5:md5sum-string string))
         (s (make-string (* 2 (length vec)) :initial-element #\0))
         (i 0))
    (iter (for c in-vector vec)
      (setf (char s i) (aref chars (ldb (byte 4 4) c)))
      (incf i)
      (setf (char s i) (aref chars (ldb (byte 4 0) c)))
      (incf i))
    s))

(defun %sql-hash (sql-statement)
  "returns a hashed form of the query, as a string"
  ;;`md5sum-sequence` returns a vector of bytes, convert it to a hex string
  (md5-string (cl-ppcre:regex-replace-all "\\s" sql-statement "")))

(defclass migration ()
  ((command :reader command :initarg :command :initform nil)
   (migration-done-p :reader migration-done-p :initarg :migration-done-p :initform nil)
   (hash :reader hash :initarg :hash :initform nil)
   ))

(defmethod initialize-instance :after ((o migration) &key &allow-other-keys)
  (let ((h (sql-hash (command o))))
    (setf (slot-value o 'hash) h
          (slot-value o 'migration-done-p) (%migration-done-p h))))

(defgeneric to-migrations (thing)
  (:documentation "Recursively turn the input into a list of migrations to be performed")
  (:method (thing)
    (etypecase thing
      (pathname (to-migrations (alexandria:read-file-into-string thing)))
      (function (to-migrations (funcall thing)))
      (string (list (make-instance 'migration :command thing)))
      (list (iter (for s in thing) (appending (to-migrations s)))))))

(defgeneric migrate (thing &key force)
  (:documentation "perform the migration, returns the number of statments executed")
  (:method ( o &key force)
    (iter (for m in (to-migrations o))
      (summing (migrate m :force force))))
  (:method ((o migration) &key force)
    ;; primary method for SQL strings
    (let ((statement-execution-count 0))
      (when (or force (not (migration-done-p o)))
        ;; Sometimes we don't care if the query threw an error, add a restart
        ;; for that case.
        ;;
        ;; For example, If we accidentally blow away the migration table,
        ;; then "CREATE TABLE" migrations will fail cause we already have the
        ;; tables.
        (with-simple-restart (continue "Ignore error, consider this migration done.")
          (clsql-sys:execute-command (command o))
          (incf statement-execution-count))
        ;; save this migration so we consider it 'done' from now on
        (clsql-sys:insert-records
         :into *migration-table-name*
         :attributes (list [hash] [query] [date-entered])
         :values (list (hash o) (command o) (clsql-helper:current-sql-time))))
      statement-execution-count)))

(defun %default-migrations ()
  "These are migrations necessary to making the system work as it upgrades"
  ;; We calculated migration md5s badly for a while, so insert new ones if we see it broken
  (ecase (clsql-sys:database-underlying-type clsql-sys:*default-database*)
    (:mssql
     ;; drop unique constraint
     (migrate "ERROR-MANUALLY-drop-unique-constraint-on-clsql_helper_migrations")
     (migrate
      "WITH mig AS (
    SELECT LOWER(hash)h0,
    SUBSTRING(master.dbo.fn_varbintohexstr(HASHBYTES('MD5', REPLACE(REPLACE(REPLACE(REPLACE(QUERY, ' ','') ,'\r',''),'\n', ''),'\t',''))), 3,32) h1,
    query, date_entered
    FROM clsql_helper_migrations
  ),
  broken AS (
    SELECT h0,h1, len(h0) l0, len(h1) l1,  query, date_entered
    FROM mig
    WHERE h0 != h1
  )
  INSERT INTO clsql_helper_migrations (hash,query,date_entered)
  (SELECT h1, query, date_entered FROM broken
   WHERE h1 NOT IN (SELECT hash FROM clsql_helper_migrations))"))
    (:postgresql
     (migrate "ALTER TABLE public.clsql_helper_migrations
        DROP CONSTRAINT clsql_helper_migrations_hash_key CASCADE ")
     (migrate
      "WITH mig AS (
    SELECT LOWER(hash)h0,
    MD5(REPLACE(REPLACE(REPLACE(REPLACE(QUERY, ' ','') ,e'\r',''),e'\n', ''),e'\t','')) h1,
    query, date_entered
    FROM clsql_helper_migrations
  ),
  broken AS (
    SELECT h0,h1, length(h0) l0, length(h1) l1,  query, date_entered
    FROM mig
    WHERE h0 != h1
  )
  INSERT INTO clsql_helper_migrations (hash, query, date_entered)
  (SELECT h1, query, date_entered FROM broken
   WHERE h1 NOT IN (SELECT hash FROM clsql_helper_migrations))"))))

(defmethod migrations (&rest sql-statements)
  "run `sql-statements` on the database once and only once. `sql-statements`
can be strings, pathnames, or lists."
  (unless clsql-sys:*default-database* (error "must have a database connection open."))
  (ensure-migration-table)
  (%default-migrations)
  (migrate sql-statements))
