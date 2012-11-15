(in-package :clsql-helper)
(cl-interpol:enable-interpol-syntax)
(clsql:file-enable-sql-reader-syntax)

(defclass dirty-slot ()
  ((slot-name :accessor slot-name :initarg :slot-name :initform nil)
   (old-value :accessor old-value :initarg :old-value :initform nil)
   (new-value :accessor new-value :initarg :new-value :initform nil)))

(defun make-dirty-slot (name old new)
  (make-instance 'dirty-slot :slot-name name :old-value old :new-value new))

(defclass dirty-db-slots-mixin ()
  ((dirty-slots :accessor dirty-slots :initarg :dirty-slots :initform nil
                :db-kind :virtual)
   (record-dirty? :accessor record-dirty? :initarg :record-dirty? :initform nil
                  :db-kind :virtual)
   (dirty-test :accessor dirty-test :initarg :dirty-test :initform `((T . ,#'equalp))
               :db-kind :virtual))
  (:metaclass clsql-sys::standard-db-class))

(defun find-dirty-test ( o slot-name )
  (or
   (iter (for (slot-names . test) in (dirty-test o))
     (when (or (eql t slot-names)
               (member slot-name (alexandria:ensure-list slot-names)))
       (return test)))
   #'equalp))

(defun %dirty-before (new class object slot
                      &aux (name (closer-mop:slot-definition-name slot)))
  ;; if its not bound but has an init form, then we are in object creation
  (when (and
         (slot-boundp object 'record-dirty?)
         (slot-boundp object 'dirty-slots)
         (slot-boundp object 'dirty-test)
         (record-dirty? object)
         (not (member name '(dirty-slots dirty-test record-dirty?))))
    (let* ((test-fn (find-dirty-test object name))
           (old (when (slot-boundp object name)
                      (closer-mop:slot-value-using-class class object slot)))
           (dirty? (not (funcall test-fn new old))))
      (when dirty?
        (pushnew (make-dirty-slot name old new) (dirty-slots object) :key #'slot-name)))))

(defun reset-dirty ( o &optional (start? t))
  (setf (dirty-slots o) nil
        (record-dirty? o) start?))

(defmethod slot-dirty? ((o dirty-db-slots-mixin) slot-name)
  (iter (for sn in (alexandria:ensure-list slot-name))
    (when (member sn (dirty-slots o) :key #'slot-name)
      (return T))))

(defmethod clsql-sys::get-slot-values-from-view :after
    ((o dirty-db-slots-mixin) slotdefs vals)
  "This setfs slot values from the database values during select, so it makes sense to reset after
   ward"
  (reset-dirty o))

(defmethod (setf closer-mop:slot-value-using-class) :before
    (new
     (class clsql-sys::standard-db-class)
     (object dirty-db-slots-mixin)
     (slot closer-mop:standard-effective-slot-definition))
  (%dirty-before new class object slot))

(defmethod clsql-sys::update-records-from-instance-slots-and-values
    ((obj dirty-db-slots-mixin) view-class database)
  (let* ((database (clsql-sys::choose-database-for-instance obj database))
         (all-slots (if (clsql-sys::normalizedp view-class)
                        (clsql-sys::ordered-class-direct-slots view-class)
                        (clsql-sys::ordered-class-slots view-class)))
         (record-values
           (loop for s in all-slots
                 when (and (clsql-sys::%slot-storedp obj s)
                           (slot-dirty? obj s))
                 collect (clsql-sys::%slot-value-list obj s database))))
    record-values))

#| reimplementation for normal classes (different metaclass)

(defclass dirty-slots-mixin ()
  ((dirty :accessor dirty :initarg :dirty :initform nil)
   (record-dirty? :accessor record-dirty? :initarg :record-dirty? :initform nil)
   (dirty-test :accessor dirty-test :initarg :dirty-test :initform `((T . ,#'equalp)))))

(defmethod (setf closer-mop:slot-value-using-class) :before
    (new
     (class standard-class)
     (object dirty-slots-mixin)
     (slot closer-mop:standard-effective-slot-definition))
  (%dirty-before new class object slot))

|#