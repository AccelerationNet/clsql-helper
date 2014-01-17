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

(defmethod clsql-sys::update-records-from-instance :before ((o recency-mixin) &key database &allow-other-keys)
  (declare (ignore database))
  (let* ((history-info (get-history-info o))
         (most-recent-historic-date
           (convert-to-clsql-datetime
            (first (alexandria:ensure-list history-info)))))
    (when (and most-recent-historic-date
               (clsql-sys::time< (%retrieved-at o) most-recent-historic-date))
      (error 'recency-error :instance o :history-info history-info))))
