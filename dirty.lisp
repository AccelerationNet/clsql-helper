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
         *record-this-dirty-slot* ;; dont record if updating from database
         (slot-boundp object 'dirty-slots)
         (slot-boundp object 'dirty-test)
         (not (member name '(dirty-slots dirty-test))))
    (let* ((test-fn (find-dirty-test object name))
           (old (when (slot-boundp object name)
                      (closer-mop:slot-value-using-class class object slot)))
           (dirty? (not (funcall test-fn new old))))
      (when dirty?
        (pushnew (make-dirty-slot name old new) (dirty-slots object) :key #'slot-name)))))

(defun reset-dirty ( o )
  (setf (dirty-slots o) nil))

(defmethod slot-dirty? ((o dirty-db-slots-mixin) slot-name &key (all? nil))
  (iter (for sn in (alexandria:ensure-list slot-name))
    (let ((res (find (clsql-sys::to-slot-name sn)
                     (dirty-slots o)
                     :key #'slot-name)))
      (if all?
          (always res)
          (thereis res)))))

(defmethod clsql-sys::get-slot-values-from-view :after
    ((o dirty-db-slots-mixin) slotdefs vals)
  "This setfs slot values from the database values during select, so it makes sense to reset after
   ward"
  (reset-dirty o))

(defvar *record-this-dirty-slot* t
  "Should we record this slot as dirty?")

(defmethod clsql-sys::update-slot-from-db-value :around ((o dirty-db-slots-mixin) slot value)
  " disable dirty slot recording if the value is from the database "
  (let ( *record-this-dirty-slot* )
    (call-next-method)))

(defmethod clsql-sys:update-instance-from-records :after
    ((o dirty-db-slots-mixin) &key &allow-other-keys)
  (reset-dirty o))

(defmethod (setf closer-mop:slot-value-using-class) :before
    (new
     (class clsql-sys::standard-db-class)
     (object dirty-db-slots-mixin)
     (slot closer-mop:standard-effective-slot-definition))
  (%dirty-before new class object slot))

(defmethod clsql-sys::update-records-from-instance-slots-and-values
    ((obj dirty-db-slots-mixin) view-class database)
  "TODO: remove this defunct function, once the oodml refactor branch is merged"
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

(defmethod clsql-sys::view-classes-and-storable-slots ((object dirty-db-slots-mixin))
  (let ((classes-and-slots (call-next-method)))
    (iter (for class-and-slots in classes-and-slots)
      (for defs = (iter (for slot-def in (clsql-sys::slot-defs class-and-slots))
                    (when (slot-dirty? object slot-def)
                      (collect slot-def))))
      (when defs
        (setf (clsql-sys::slot-defs class-and-slots) defs)
        (collect class-and-slots)))))

#| reimplementation for normal classes (different metaclass)

(defclass dirty-slots-mixin ()
  ((dirty :accessor dirty :initarg :dirty :initform nil)
   (dirty-test :accessor dirty-test :initarg :dirty-test :initform `((T . ,#'equalp)))))

(defmethod (setf closer-mop:slot-value-using-class) :before
    (new
     (class standard-class)
     (object dirty-slots-mixin)
     (slot closer-mop:standard-effective-slot-definition))
  (%dirty-before new class object slot))

|#