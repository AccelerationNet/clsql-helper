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

  (push (clsql-sys::sql-expression :string #?"${(current-timestamp-sql)} queried")
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

(defun most-recent-history-date (o)
  (convert-to-clsql-datetime
   (first
    (alexandria:ensure-list
     (get-history-info o)))))

(defgeneric validate-recency (o &key history-info %retrieved-at)
  (:method ((o recency-mixin) &key history-info %retrieved-at
            &aux (clsql-helper::*iso8601-microseconds* t))
    (let* ((history-info (or history-info
                             (get-history-info o)))
           (most-recent-historic-date
             (convert-to-clsql-datetime
              (first (alexandria:ensure-list history-info))))
           (%ret (convert-to-clsql-datetime (or %retrieved-at (%retrieved-at o)))))
      (when (and most-recent-historic-date %ret
                 (clsql-sys::time< %ret most-recent-historic-date))
        (with-simple-restart (continue "Consider the recency error handled")
          (error 'recency-error :instance o :history-info history-info))))))

(defun %before-update-recency-check (o)
  (validate-recency o))

(defun current-timestamp-sql ()
  (case (clsql-sys:database-underlying-type clsql-sys:*default-database*)
    (:sqlite3 "STRFTIME('%Y-%m-%d %H:%M:%f', 'NOW')")
    (:postgresql "clock_timestamp()")
    (t "CURRENT_TIMESTAMP")))

(defun current-timestamp ()
  (with-a-database ()
    (convert-to-clsql-datetime
     (first (clsql:query #?"SELECT ${ (current-timestamp-sql) }"
              :flatp t)))))

(defun %after-update-recency-check (o)
  (setf (%retrieved-at o) (current-timestamp)))


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


(defmethod save! ((o recency-mixin) &key original &allow-other-keys)
  (labels ((do-merge (more-recent &aux (cnt 0) )
             (collectors:with-collector-output (conflicts)
               (handler-bind ((merge-conflict
                                (lambda (c)
                                  (conflicts c)
                                  (invoke-restart 'skip)))
                              (merging-values (lambda (c) (declare (ignore c))
                                                (incf cnt))))
                 (merge-changes original more-recent o)
                 (when (plusp cnt)
                   (clsql-sys:update-records-from-instance more-recent)
                   (clsql-sys:update-instance-from-records o))))))
    (handler-bind ((recency-error
                     (lambda (c) (declare (ignore c))
                       (when original
                         (let ((more-recent (copy-instance original)))
                           (clsql-sys:update-instance-from-records more-recent)
                           (let ((conflicts (do-merge more-recent)))
                             (if conflicts
                                 (error 'merge-conflicts :conflicts conflicts)
                                 (continue))))))))
      (call-next-method))))





