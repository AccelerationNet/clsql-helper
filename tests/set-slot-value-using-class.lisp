(defpackage :clsql-helper-slot-coercer-test
  (:use :cl :clsql-helper :lisp-unit :iter))

(in-package :clsql-helper-slot-coercer-test)
(cl-interpol:enable-interpol-syntax)

(clsql-sys:def-view-class role ()
    ((name :column first_name :accessor name
           :db-constraints nil :initform nil :type clsql-sys:varchar
           :initarg name)
     (id :column id :accessor id :db-kind key :db-constraints
         (not-null) :type integer :initarg id)))

(clsql-sys:def-view-class user ()
    ((date-entered :column date_entered :accessor date-entered
                   :db-constraints nil :type clsql-sys:wall-time :initarg
                   date-entered)
     (edate :column edate :accessor edate
            :db-constraints nil :type clsql-sys:date :initarg
            edate)

     (amount :column amount :accessor amount
            :db-constraints nil :type double-float :initarg
            edate)
     (deleted :column deleted :accessor deleted :db-constraints nil
              :type boolean :initarg deleted)
     (email :column email :accessor email :db-constraints (not-null)
            :type (clsql-sys:varchar 128) :initarg email)
     (enabled :column enabled :accessor enabled :db-constraints
              (not-null) :initform t :type boolean :initarg enabled)
     (first-name :column first_name :accessor first-name
                 :db-constraints nil :initform nil :type clsql-sys:varchar
                 :initarg first-name)
     (id :column id :accessor id :db-kind key :db-constraints
         (not-null) :type integer :initarg id)
     (last-name :column last_name :accessor last-name :db-constraints
                nil :initform nil :type clsql-sys:varchar :initarg last-name)
     (password :column password :accessor password :db-constraints
               (not-null) :type (clsql-sys:varchar 32) :initarg password)
     (role-id :column role_id :accessor role-id :db-constraints
              (not-null) :type integer :initarg role-id)
     (role-join :accessor role-join :db-kind join :db-info
                (:join-class role :home-key role-id :foreign-key id
                                 :set nil))
     (salt :column salt :accessor salt :db-constraints (not-null) :type
           (clsql-sys:varchar 4) :initarg salt))
 (base-table users))

(define-test test-slot-value-coersion
  (let ((u (make-instance 'user)))
    (setf (first-name u) "First" )
    (assert-true (stringp (first-name u)))
    (setf (last-name u) :Last )
    (assert-true (stringp (last-name u)))
    (setf (role-id u) "47")
    (assert-true (typep (role-id u) 'integer))

    (setf (date-entered u) "7/7/1977 11:43:26.123456")
    (assert-true (typep (date-entered u) 'clsql-sys:wall-time) (date-entered u))
    (setf (date-entered u) (convert-to-clsql-date "7/7/1977 11:43:26.123456"))
    (assert-true (typep (date-entered u) 'clsql-sys:wall-time) (date-entered u))
    (setf (date-entered u) (convert-to-clsql-datetime "7/7/1977 11:43:26.123456"))
    (assert-true (typep (date-entered u) 'clsql-sys:wall-time) (date-entered u))
    (setf (date-entered u) nil)
    (assert-true (typep (date-entered u) 'null) (date-entered u))
    (assert-error 'error (setf (date-entered u) "asdf"))

    (setf (edate u) nil)
    (assert-true (typep (edate u) 'null) (edate u))
    (setf (edate u) "7/7/1977")
    (assert-true (typep (edate u) 'clsql-sys:date) (edate u))
    (setf (edate u) (convert-to-clsql-datetime "7/7/1977"))
    (assert-true (typep (edate u) 'clsql-sys:date) (edate u))
    (assert-error 'error (setf (edate u) "asdf"))

    (setf (amount u) 23)
    (assert-true (typep (amount u) 'double-float) (amount u))
    (setf (amount u) "23")
    (assert-true (typep (amount u) 'double-float) (amount u))
    (setf (amount u) 23.2)
    (assert-true (typep (amount u) 'double-float) (amount u))
    (setf (amount u) 23.2d0)
    (assert-true (typep (amount u) 'double-float) (amount u))
    ))

(run-tests)