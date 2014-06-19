(in-package :clsql-helper)
(cl-interpol:enable-interpol-syntax)
(clsql:file-enable-sql-reader-syntax)

(defclass recency-mixin (db-object)
  ((%retrieved-at
    :accessor %retrieved-at :initarg :%retrieved-at :initform nil
    :documentation
    "The Server time this was retrieved from the database
     Named this way to reduce conflict likely hood with database columns"
    :db-kind :virtual
    :type clsql-sys:wall-time)
   (%history-select-fn
    :accessor %history-select-fn :initarg :%history-select-fn :initform nil
    :db-kind :virtual
    :documentation "a function that returns history data about an object the
    first returned value MUST be the most recent time the object was saved in
    the database" ))
  (:metaclass clsql-sys::standard-db-class))

(defmethod clsql-sys::filter-select-list ((o recency-mixin) (sl clsql-sys::select-list)
                                          database)
  (declare (ignore database))
  (push (clsql-sys::sql-expression :string "CURRENT_TIMESTAMP as queried")
        (clsql-sys::select-list sl))
  (push (find '%retrieved-at (clsql-sys::class-direct-slots
                              (find-class 'recency-mixin))
              :key #'clsql-sys::slot-definition-name)
        (clsql-sys::slot-list sl)))

(defgeneric get-history-info (o)
  (:method ((o recency-mixin))
    (when (%history-select-fn o)
      (funcall (%history-select-fn o) o))))

(define-condition recency-error (error)
  ((instance :accessor instance :initarg :instance :initform nil)
   (history-info :accessor history-info :initarg :history-info :initform nil)))

(defgeneric validate-recency (o &key history-info %retrieved-at)
  (:method ((o recency-mixin) &key history-info %retrieved-at)
    (let* ((history-info (or history-info
                             (get-history-info o)))
           (most-recent-historic-date
             (convert-to-clsql-datetime
              (first (alexandria:ensure-list history-info))))
           (%ret (convert-to-clsql-datetime (or %retrieved-at (%retrieved-at o)))))
      (when (and most-recent-historic-date %ret
                 (clsql-sys::time< %ret most-recent-historic-date))
        (error 'recency-error :instance o :history-info history-info)))))

(defun %before-update-recency-check (o)
  (validate-recency o))

(defun current-timestamp ()
  (with-a-database ()
    (convert-to-clsql-datetime
     (first (clsql:query
             (case (clsql-sys:database-underlying-type clsql-sys:*default-database*)
               (:sqlite3 "SELECT STRFTIME('%Y-%m-%d %H:%M:%f', 'NOW')")
               (t "SELECT CURRENT_TIMESTAMP")) :flatp t)))))

(defun %after-update-recency-check (o)
  (setf (%retrieved-at o)
        (current-timestamp)))


(defmethod clsql-sys::update-records-from-instance :before ((o recency-mixin) &key database &allow-other-keys)
  (declare (ignore database))
  (%before-update-recency-check o))

(defmethod clsql-sys::update-records-from-instance :after ((o recency-mixin) &key database &allow-other-keys)
  (declare (ignore database))
  (%after-update-recency-check o))

(defmethod clsql-sys::update-record-from-slots :before ((o recency-mixin) slots &key database &allow-other-keys)
  (declare (ignore database slots))
  (%before-update-recency-check o))

(defmethod clsql-sys::update-record-from-slots :after ((o recency-mixin) slots &key database &allow-other-keys)
  (declare (ignore database slots))
  (%after-update-recency-check o))





