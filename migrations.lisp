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
     ;; hash of our query
     '(([hash] longchar :not-null :unique)
       ;; the actual query
       ([query] longchar :not-null)
       ;; when we migrated it
       ([date-entered] clsql-sys:wall-time :not-null)))))

(defun migration-done-p (hash)
  "returns non-nil if this hashed migration has been run"
  (clsql:select [date-entered] :from *migration-table-name*
                :where [= [hash] hash]
                :flatp T))

(defun sql-hash (sql-statement)
  "returns a hashed form of the query, as a string"
  ;;`md5sum-sequence` returns a vector of bytes, convert it to a hex string
  (format nil "~{~x~}"
          (coerce (md5:md5sum-sequence
                   ;; don't consider whitespace changes relevant
                   (cl-ppcre:regex-replace-all "\\s" sql-statement ""))
                  'list)))

(defgeneric migrate (thing)
  (:documentation "perform the migration")
  ;; primary method for SQL strings
  (:method ((sql-statement string))
    (let ((hash (sql-hash sql-statement)))
      ;; only process if we haven't done the migration already
      (unless (migration-done-p hash)
        ;; Sometimes we don't care if the query threw an error, add a restart
        ;; for that case.
        ;;
        ;; For example, If we accidentally blow away the migration table,
        ;; then "CREATE TABLE" migrations will fail cause we already have the
        ;; tables.
        (with-simple-restart (continue "Ignore error, consider this migration done.")
          (clsql-sys:execute-command sql-statement))
        ;; save this migration so we consider it 'done' from now on
        (clsql-sys:insert-records
         :into *migration-table-name*
         :attributes (list [hash] [query] [date-entered])
         :values (list hash sql-statement (clsql-helper:current-sql-time))))))
  ;; if we get a pathname, read it into a string
  (:method ((sql-file pathname))
    (migrate (alexandria:read-file-into-string sql-file))))

(defun migrations (&rest sql-statements)
  "run `sql-statements` on the database once and only once. `sql-statements`
can be strings, pathnames, or lists."
  (unless clsql-sys:*default-database* (error "must have a database connection open."))
  (ensure-migration-table)
  ;; flatten first to support broader input (makes programatically generating
  ;; `sql-statement`s at call sites a little easier.)
  (mapc #'migrate (alexandria:flatten sql-statements)))
