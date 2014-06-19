(in-package :clsql-helper-test)
(cl-interpol:enable-interpol-syntax)
(clsql-sys:file-enable-sql-reader-syntax)

(defmethod recency-test-history-fn ((o recency-test-obj))
  (when (id o)
    (convert-to-clsql-datetime
     (first
      (clsql-helper:db-select
       [datemodified]
       :from [recencytestobj]
       :where [= [id] (id o)])))))

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

(defmethod clsql-sys:update-records-from-instance :before ((o recency-test-obj) &key &allow-other-keys)
  (setf (datemodified o) (current-timestamp)))



(defun with-sqlite3-test-context (body)
  (clsql-tests::test-connect-to-database
   :sqlite3 (nth 0 (clsql-tests::db-type-spec
                    :sqlite3
                    (clsql-tests::read-specs))))
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
                          :name "Russ"
                          :date (convert-to-clsql-datetime "12/1/2000")
                          :value 1)))
    (clsql-helper:save! a)
    (let ((b (first (clsql-helper:db-select 'recency-test-obj :where [= [id] (id a)]))))
      (setf (value a) 2)
      (setf (value b) 3
            (name b) "B-Russ")
      (lisp-unit2:assert-no-error
       'clsql-helper:recency-error
       (clsql-sys:update-records-from-instance a))
      (lisp-unit2:assert-error
       'clsql-helper:recency-error 
       (clsql-sys:update-records-from-instance b))
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
      ;; already merged
      (lisp-unit2:assert-no-signal 'clsql-helper::merging-values (clsql-helper:merge-changes a a-prime b))
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




