(in-package :clsql-helper-test)
(cl-interpol:enable-interpol-syntax)
(clsql-sys:file-enable-sql-reader-syntax)

(clsql-sys:def-view-class recency-test-obj (recency-mixin)
  ((id :accessor id :initarg :id :initform nil :db-kind :key
       :db-constraints (:not-null :autoincrement) :type integer)
   (name :accessor name :initarg :name :initform nil :type string )
   (date :accessor date :initarg :date :initform nil :type clsql-sys:wall-time)
   (value :accessor value :initarg :value :initform nil :type integer)
   (dateentered
    :accessor dateentered :initarg :dateentered
    :initform (current-timestamp)
    :type clsql-sys:wall-time)
   (datemodified
    :accessor datemodified :initarg :datemodified
    :initform (current-timestamp)
    :type clsql-sys:wall-time))
  (:base-table "RecencyTestObj")
  (:default-initargs :%history-select-fn 'recency-test-history-fn))

(defmethod recency-test-history-fn ((o recency-test-obj))
  (when (id o)
    (convert-to-clsql-datetime
     (first
      (clsql-helper:db-select
       [datemodified]
       :from [recencytestobj]
       :where [= [id] (id o)])))))

(defmethod clsql-sys:update-records-from-instance :before ((o recency-test-obj) &key &allow-other-keys)
  (setf (datemodified o) (current-timestamp)))



(defun with-sqlite3-test-context (body)
  (clsql-tests::rapid-load :sqlite3)
  (unwind-protect
       (progn
         (unless (clsql-sys:table-exists-p "RecencyTestObj")
           (clsql-sys:create-view-from-class 'recency-test-obj))
         (funcall body))
    (when clsql-sys::*default-database*
      (clsql-sys::disconnect :database clsql-sys::*default-database*))))

(lisp-unit2:define-test test-recency-checks (:tags '(recency diff merge)
                                             :contexts 'with-sqlite3-test-context)
  (let ((a (make-instance 'recency-test-obj
                          :name "A-Russ"
                          :date (convert-to-clsql-datetime "12/1/2000")
                          :value 1)))
    (clsql-sys:update-records-from-instance a)
    (let* ((b (first (clsql-helper:db-select 'recency-test-obj :where [= [id] (id a)]))))
      (setf (value a) 2)
      (setf (value b) 3 (name b) "B-Russ")
      (sleep .1);; just give us a millisec or 100
      (lisp-unit2:assert-no-error
       'clsql-helper:recency-error
       (clsql-sys:update-records-from-instance a)
       (print-timestamp (clsql-helper::%retrieved-at a))
       (print-timestamp (datemodified a))
       (print-timestamp (clsql-helper::%retrieved-at b))
       (print-timestamp (datemodified b)))
      
      (sleep .1)
      (let ((ra-b (print-timestamp (clsql-helper::%retrieved-at b)))
            (mrhd (print-timestamp (clsql-helper::most-recent-history-date b))))
        (lisp-unit2:assert-error
         'clsql-helper:recency-error 
         (clsql-sys:update-records-from-instance b)
         (print-timestamp (clsql-helper::%retrieved-at a))
         (print-timestamp (datemodified a))
         ra-b 
         mrhd))
      )))

(lisp-unit2:define-test test-merge (:tags '(recency diff merge)
                                    :contexts 'with-sqlite3-test-context)
  (let ((a (make-instance 'recency-test-obj
                          :name "Russ"
                          :date (convert-to-clsql-datetime "12/1/2000")
                          :value 1)))
    (clsql-helper:save! a)
    (let ((a-prime (copy-instance a))
          (b (first (clsql-helper:db-select 'recency-test-obj :where [= [id] (id a)]))))
      (setf (value a-prime) 2)
      (clsql-helper:save! a-prime)
      (setf (date b) (convert-to-clsql-datetime "1/1/2000"))
      (lisp-unit2:assert-signal 'clsql-helper::merging-values
                                (clsql-helper:merge-changes a a-prime b))

      (lisp-unit2:assert-equalp
       (convert-to-clsql-datetime "1/1/2000")
       (date a-prime))
      (setf (value b) 3)
      (let ( handled? )
        (handler-bind ((clsql-helper::merge-conflict
                         (lambda (c) (declare (ignore c))
                           (setf handled? t)
                           (invoke-restart 'clsql-helper::overwrite))))
          (clsql-helper:merge-changes a a-prime b))
        (assert-true handled?)
        (assert-eql 3 (value a-prime)))
      )))

(lisp-unit2:define-test test-automerging-save (:tags '(recency diff merge)
                                               :contexts 'with-sqlite3-test-context)
  (let ((a (make-instance 'recency-test-obj
                          :name "Russ"
                          :date (convert-to-clsql-datetime "12/1/2000")
                          :value 1)))
    (clsql-helper:save! a)
    (sleep .1)
    (let* ((b (first (clsql-helper:db-select 'recency-test-obj :where [= [id] (id a)])))
           (original (copy-instance b)))
      (setf (value a) 2)
      (setf (value b) 3
            (name b) "B-Russ")
      (sleep .1)      
      (save! a)
      (sleep .1)
      (let ((handled?
              (block handled
                (handler-bind
                    ((merge-conflicts
                       (lambda (c)
                         (let* ((conflict (first (clsql-helper::conflicts c))))
                           (assert-eql 'value (clsql-helper::slot conflict)))
                         (return-from handled c)) ))
                  (save! b :original original)
                  nil))) )
        (assert-true handled?))

      (clsql-sys:update-instance-from-records a)
      ;; db has correct values
      (assert-eql 2 (value a))
      (assert-equal "B-Russ" (name a))
      )))




